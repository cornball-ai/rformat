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
100 popular CRAN packages.

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

The stress test script (`lab/stress_test.R`) does the following for each package:

1. Downloads the source tarball from CRAN
2. Extracts the `R/` directory
3. Filters to parseable `.R` files (some packages have files that don't parse)
4. Formats each file with `rformat()`
5. **Parse gate**: verifies the formatted output parses without errors
6. **Idempotency gate**: formats the output a second time and checks
   `rformat(rformat(x)) == rformat(x)`
7. Reports results per-package and per-file

A per-file timeout (default 10 seconds) prevents hanging on unusually large files.

### Running the Stress Test

```r
# Run all 100 packages
r lab/stress_test.R

# Run first N packages
N=10 r lab/stress_test.R

# Run specific packages
PKGS="dplyr rlang Rcpp" r lab/stress_test.R

# Adjust per-file timeout (seconds)
TIMEOUT=30 r lab/stress_test.R
```

## Package List

The test suite covers 100 packages across 10 ecosystems:

| Category | Packages |
|----------|----------|
| Core / Infrastructure | Rcpp, rlang, vctrs, glue, cli, withr, lifecycle, magrittr, pillar, crayon |
| Data Manipulation | dplyr, tidyr, tibble, readr, stringr, forcats, lubridate, purrr, data.table, janitor |
| Visualization | ggplot2, scales, patchwork, cowplot, plotly, lattice, leaflet, ggridges, viridis, DT |
| Shiny / Web | shiny, shinydashboard, bslib, htmltools, httpuv, httr, httr2, plumber, rvest, xml2 |
| Modeling / Stats | caret, recipes, parsnip, workflows, yardstick, glmnet, lme4, survival, brms, xgboost |
| Devtools / Packaging | testthat, tinytest, devtools, usethis, roxygen2, pkgdown, covr, remotes, pak, renv |
| Data Import / Storage | readxl, writexl, DBI, RSQLite, duckdb, arrow, sparklyr, pins, qs, fst |
| Time Series / Specialized | forecast, zoo, xts, tsibble, fable, igraph, sf, terra, sp, rmarkdown |
| String / Parsing / Lang | stringi, jsonlite, yaml, digest, R6, xmlparsedata, evaluate, callr, processx, here |
| Performance / Parallel | future, furrr, parallelly, RcppParallel, bench, profvis, memoise, progress, curl, openssl |

## Results

Results from a single run on February 2026.

### Summary

| Status | Packages | Files |
|--------|----------|-------|
| OK (parse + idempotent) | 68 | 4,751 |
| IDEMP (parse OK, not idempotent) | 13 | 20 files affected |
| FAIL (parse errors in output) | 17 | 56 files affected |
| SKIP (download failed) | 2 | -- |

**Overall: 4,751 / 4,807 files formatted successfully (98.8%)**

26 files across 11 packages timed out (exceeded the 10-second per-file limit).

### Fully Passing Packages (68)

Rcpp, rlang, vctrs, glue, cli, withr, lifecycle, magrittr, pillar, crayon,
dplyr, tibble, readr, stringr, forcats, purrr, janitor, scales, cowplot,
leaflet, viridis, bslib, htmltools, httpuv, httr, httr2, plumber, rvest,
xml2, parsnip, workflows, yardstick, xgboost, testthat, tinytest, devtools,
usethis, roxygen2, pkgdown, remotes, pak, readxl, writexl, DBI, RSQLite,
duckdb, sparklyr, fst, xts, tsibble, igraph, stringi, jsonlite, yaml,
digest, R6, xmlparsedata, evaluate, callr, here, furrr, RcppParallel, bench,
profvis, memoise, progress, curl, openssl

### Packages with Idempotency Issues (13)

These packages format correctly (output parses) but produce slightly different
output when formatted a second time. Typically 1-2 files per package, usually
due to edge cases in operator continuation or call wrapping.

tidyr (1 file), lubridate (1), patchwork (1), plotly (1), ggridges (1),
brms (1), covr (1), arrow (1), pins (2), forecast (1), terra (6),
processx (2), parallelly (1)

### Packages with Parse Failures (17)

These packages have files where the formatted output doesn't parse. This
indicates bugs in rformat's formatting passes that need investigation.

data.table (6 files), ggplot2 (1), lattice (7), DT (2), shiny (1),
shinydashboard (1), caret (2), glmnet (1), lme4 (4), survival (5),
renv (3), zoo (4), fable (1), sf (12), sp (3), rmarkdown (1), future (2)

## Bugs Found and Fixed

The stress test has been instrumental in finding bugs. Each round of testing
surfaces new failure patterns that are then fixed and regression-tested.

### Round 1: Core formatting bugs

These bugs were found in the initial stress test run and fixed before the first
published results.

**Tab-expanded column positions.** R's `getParseData()` reports column numbers
with tabs expanded to 8-column tab stops, but `substring()` works on character
positions. Using `col1`/`col2` directly with `substring()` produces wrong
results when source lines contain tabs. Fixed with `col_to_charpos()` and
`display_width()` helper functions.

**Nested call wrap corruption.** When a long line contained nested function
calls (e.g., `sprintf(..., paste(sprintf(...)))`) the wrap pass would try to
wrap an inner call that was already inside another call's parentheses on the
same line. Fixed by checking `paren_depth_before > 0` and skipping nested calls.

**Comment between if-condition and body.** Code like `if (!x) # comment`
followed by the body on the next line would lose the comment. Fixed by detecting
COMMENT tokens after `)` and preserving them on the opening brace line.

**Operator continuation instability.** Wrap passes used paren-column alignment
for continuation lines, but `format_tokens` uses depth-based indentation.
The two systems disagreed, causing the second format pass to change indentation.
Fixed by switching wrap passes to depth-based indent matching `format_tokens`.

**Anonymous function spacing.** `function(x)` was not getting its required
space: `function (x)`. The `needs_space()` function returned FALSE for `(`
after `FUNCTION`. Fixed to return TRUE.

### Round 2: Cross-boundary and truncation bugs

These bugs were found in the second stress test run (17 FAIL packages remaining,
down from 23).

**Long string truncation (Bug A).** `getParseData()` truncates `STR_CONST`
tokens longer than ~1000 characters to a placeholder like
`[1200 chars quoted with '"']`. The formatter would output this placeholder
instead of the actual string, breaking parsing. Fixed by detecting truncated
tokens and recovering the original text from source lines using the token's
line/column positions. Affected: httr2, tinytest, igraph.

**ELSE search crosses brace boundaries (Bug B).** `reformat_one_inline_if`
searches forward from `x <- if (cond) value` to find a matching `else`. The
search tracked if-else nesting but not brace boundaries. For code like:

```r
if (outer) {
    space <- if (cond) padding
    use(space)
} else {
    other()
}
```

The search would cross the `}` and find the outer `else`, incorrectly expanding
the inner `if` and collapsing `use(space)` into the else branch. Fixed by
tracking brace depth and stopping the search when `brace_depth < 0` (exiting
the enclosing block). Only matches ELSE at `brace_depth == 0`. Affected: sf
(14 files), lattice (7), zoo, sp, future, rmarkdown, and others.

**Braced false body collapsed (Bug C).** When expanding
`x <- if (cond) a else { stmt1; stmt2 }`, the formatter would try to inline
the braced false body using `format_line_tokens`, which strips newlines and
produces `x <- { stmt1 stmt2 }` — missing semicolons, broken parse. Fixed by
skipping expansion when the false body starts with `{`. Affected: zoo,
data.table, and others with multi-statement else blocks.

## Idempotency Design

Achieving idempotency required careful coordination between rformat's formatting
passes. The key insight: two systems compute indentation, and they must agree.

**`format_tokens`** (the initial pass) computes indentation from brace and
bracket depth: `indent = (brace_depth + paren_depth) * indent_size`. It tracks
`(`, `)`, `[`, `]`, and `[[` as part of paren depth.

**Wrap functions** (`wrap_one_long_call`, `wrap_one_long_operator`) split long
lines and must produce continuation indentation that `format_tokens` will
preserve on the next pass.

The solution: wrap functions use depth-based indentation that matches
`format_tokens`, rather than column-aligned indentation (which `format_tokens`
would normalize away). Specifically:

- **Operator wrap**: continuation indent = `base_indent + bracket_depth * indent_size`
- **Call wrap**: continuation indent = `base_indent + (bracket_depth_before + 1) * indent_size`

Paren-column alignment (aligning continuation to the opening parenthesis) is
preserved for function definitions via `reformat_one_function`, which rebuilds
function signatures from scratch on each pass.

## Future Work

- Fix the remaining 56 parse failures across 17 packages
- Resolve remaining 20 idempotency edge cases across 13 packages
- Add AST comparison testing for semantic preservation verification
