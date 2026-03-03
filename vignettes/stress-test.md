<!--
%\VignetteIndexEntry{Stress Testing rformat}
%\VignetteEngine{simplermarkdown::mdweave_to_html}
%\VignetteEncoding{UTF-8}
-->
---
title: "Stress Testing rformat"
---

# Stress Testing rformat

This vignette documents rformat's stress testing methodology and results against
126 CRAN and base R packages.

## Correctness Invariants

rformat is designed around three invariants:

1. **Parse preservation.** Formatted output must always parse. If input parses,
   output parses. This is the minimum correctness bar for any formatter.

2. **Semantic preservation.** Formatting changes only whitespace and style tokens.
   Assignment conversion (`=` to `<-`) and brace insertion use R's parse tree
   token types (e.g., `EQ_ASSIGN` vs `EQ_SUB`), ensuring they never change
   program semantics.

3. **Idempotency.** Formatting is a fixed point: `rformat(rformat(x)) == rformat(x)`.
   A formatter that isn't idempotent produces different output on each run, which
   makes it unusable in CI pipelines and pre-commit hooks.

## Why These Tests Matter

A whitespace-only formatter could verify correctness by comparing ASTs before and
after formatting. But rformat also converts `=` to `<-` and inserts braces around
bare control flow bodies, both of which modify the AST. These changes are guided
by R's parser token types and are semantically safe, but they raise the testing
bar: we need both parse preservation and idempotency as quality gates.

The parse gate is necessary but not sufficient. Code that parses may still have
subtle formatting issues (e.g., continuation lines at the wrong indent level that
happen to produce valid R). Idempotency catches these: if a second formatting pass
produces different output, the first pass made a formatting decision that the
second pass disagrees with. This signals an internal inconsistency.

## Test Methodology

The stress test script (`stress_test.R`) does the following for each package:

1. Extracts the `R/` directory from cached source tarballs
2. Filters to parseable `.R` files (some packages have files that don't parse)
3. Formats each file with `rformat()` using **randomized style parameters**
   (indent, wrap, brace_style, control_braces, line_limit, expand_if, etc.)
4. **Parse gate**: verifies the formatted output parses without errors
5. **Idempotency gate**: formats the output a second time with the same
   parameters and checks `rformat(rformat(x)) == rformat(x)`
6. Reports results per-package and per-file

### Randomized Parameters

Each file is formatted with a randomly chosen parameter combination. This
exercises all option interactions and catches edge cases that fixed parameters
would miss. The 10 strategic combinations cover:

- Default settings, narrow limits, 2-space indent, tab indent
- Allman braces, control braces, expanded if-else, function space
- Fixed wrap style, combined options

### Running the Stress Test

```
# Full 126-package run (parallel)
cd ~/rformat-lab
r -e 'cat(read.csv("stress_timing.csv")$package, sep="\n")' |
    parallel -j16 --timeout 900 r stress_test.R {} 2>/dev/null |
    grep -E "FAIL|IDEMP|_DONE_"

# Test specific packages
r stress_test.R dplyr
r stress_test.R ggplot2

# Test one file with all 10 strategic combos
parallel -j10 --timeout 120 r stress_one_combo.R {1} {2} \
    ::: path/to/file.R ::: $(seq 1 10)
```

## Package List

The test suite covers 126 packages across multiple ecosystems:

| Category | Packages |
|----------|----------|
| Base R (15) | base, compiler, graphics, grDevices, grid, methods, parallel, splines, stats, stats4, tcltk, tools, utils, datasets, class |
| Recommended (15) | boot, cluster, codetools, foreign, KernSmooth, lattice, MASS, Matrix, mgcv, nlme, nnet, rpart, spatial, survival |
| Core / Infrastructure | Rcpp, rlang, vctrs, glue, cli, withr, lifecycle, magrittr, pillar, crayon |
| Data Manipulation | dplyr, tidyr, tibble, readr, stringr, forcats, lubridate, purrr, data.table, janitor |
| Visualization | ggplot2, scales, patchwork, cowplot, plotly, lattice, leaflet, ggridges, viridis, DT |
| Shiny / Web | shiny, shinydashboard, bslib, htmltools, httpuv, httr, httr2, plumber, rvest, xml2 |
| Modeling / Stats | caret, recipes, parsnip, workflows, yardstick, glmnet, lme4, survival, brms, xgboost |
| Devtools / Packaging | testthat, tinytest, devtools, usethis, roxygen2, pkgdown, covr, remotes, pak, renv |
| Data Import / Storage | readxl, writexl, DBI, RSQLite, duckdb, arrow, sparklyr, pins, qs2, fst |
| Time Series / Specialized | forecast, zoo, xts, tsibble, fable, igraph, sf, terra, sp, rmarkdown |
| String / Parsing / Lang | stringi, jsonlite, yaml, digest, R6, xmlparsedata, evaluate, callr, processx, here |
| Performance / Parallel | future, furrr, parallelly, RcppParallel, bench, profvis, memoise, progress, curl, openssl |

## Results

Results as of March 2026 with the C++ (Rcpp) backend.

### Summary

| Metric | Value |
|--------|-------|
| Packages tested | 126 |
| Files formatted | 5,843 |
| Parse failures (FAIL) | 0 |
| Idempotency failures (IDEMP) | 0 |
| Wall clock time (16 cores) | ~58 seconds |

**0 failures across 5,843 files with randomized style parameters.**

## Bugs Found and Fixed

The stress test has been instrumental in finding and fixing bugs. Each round of
testing surfaced new failure patterns, all now covered by regression tests.

### Tab-expanded column positions

R's `getParseData()` reports column numbers with tabs expanded to 8-column tab
stops, but `substring()` works on character positions. Using `col1`/`col2`
directly with `substring()` produces wrong results when source lines contain
tabs. Fixed with `col_to_charpos()` and `display_width()` helper functions.

### Long string truncation

`getParseData()` truncates `STR_CONST` tokens longer than ~1000 characters to a
placeholder like `[1200 chars quoted with '"']`. Fixed by detecting truncated
tokens and recovering the original text from source lines using the token's
line/column positions.

### ELSE search crosses brace boundaries

`reformat_one_inline_if` searches forward from `x <- if (cond) value` to find a
matching `else`. The search tracked if-else nesting but not brace boundaries. For
code like:

```
if (outer) {
    space <- if (cond) padding
    use(space)
} else {
    other()
}
```

The search would cross the `}` and find the outer `else`. Fixed by tracking brace
depth and stopping when `brace_depth < 0`.

### Braced false body collapsed

When expanding `x <- if (cond) a else { stmt1; stmt2 }`, the formatter would
try to inline the braced false body, producing `x <- { stmt1 stmt2 }` with
missing semicolons. Fixed by skipping expansion when the false body starts
with `{`.

### Nested call wrap corruption

When a long line contained nested function calls, the wrap pass would try to
wrap an inner call already inside another call's parentheses on the same line.
Fixed by checking `paren_depth_before > 0` and skipping nested calls.

### Comment transparency in bare body detection

Comments between a trailing operator and a value (e.g., `else # comment` with
body on the next line) were treated as statement boundaries. Fixed by tracking
`last_real_tok` and only checking non-comment tokens against statement-ending
token types.

## Idempotency Design

Achieving idempotency required careful coordination between formatting passes.
The key insight: two systems compute indentation, and they must agree.

**`format_tokens`** (the initial pass) computes indentation from brace and
bracket depth: `indent = (brace_depth + paren_depth) * indent_size`. It tracks
`(`, `)`, `[`, `]`, and `[[` as part of paren depth.

**Wrap functions** (`wrap_long_calls`, `wrap_long_operators`) split long lines and
must produce continuation indentation that `format_tokens` will preserve on the
next pass. They use depth-based indentation matching `format_tokens`, rather than
column-aligned indentation (which `format_tokens` would normalize away):

- **Operator wrap**: continuation indent = `base_indent + bracket_depth * indent_size`
- **Call wrap**: continuation indent = `base_indent + (bracket_depth_before + 1) * indent_size`

Paren-column alignment (aligning continuation to the opening parenthesis) is
preserved for function definitions via `reformat_function_defs`, which rebuilds
function signatures from scratch on each pass.
