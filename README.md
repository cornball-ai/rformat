# rformat

Token-based R code formatter following R Core style conventions.

## Installation

```r
remotes::install_github("cornball-ai/rformat")
```

## Usage

```r
library(rformat)

# Format a code string
rformat("x<-1+2")
#> x <- 1 + 2

# Format a file
rformat_file("R/my_script.R")

# Format all R files in a directory
rformat_dir("R/")
```

## Options

```r
# Default: 4-space indentation, paren alignment
rformat(code)

# Custom indentation (2 spaces)
rformat(code, indent = 2L)

# Tab indentation
rformat(code, indent = "\t")

# Fixed 8-space continuation (instead of paren alignment)
rformat(code, wrap = "fixed")

# Expand inline if-else to multi-line
rformat(code, expand_if = TRUE)
```

## Style

Based on analysis of actual R Core source code (see `vignette("r-core-style")`).

### Spacing

- Spaces around binary operators: `x <- 1 + 2`
- Space after `function`: `function (x, y)`
- No space before `(` in calls: `foo(x)` not `foo (x)`
- Space after commas: `c(1, 2, 3)`
- Space after control flow: `if (`, `for (`, `while (`

### Function Definitions

Short signatures stay on one line. Long signatures wrap with continuation indent. Opening brace on its own line.

```r
# Short
lapply <- function (X, FUN, ...)
{
    FUN <- match.fun(FUN)
    ...
}

# Long (default: paren alignment)
lm <- function (formula, data, subset, weights, na.action, method = "qr",
                model = TRUE, x = FALSE, y = FALSE, qr = TRUE,
                singular.ok = TRUE, contrasts = NULL, offset, ...)
{
    ...
}

# Long (wrap = "fixed": 8-space indent)
lm <- function (formula, data, subset, weights, na.action,
        method = "qr", model = TRUE, x = FALSE, y = FALSE,
        qr = TRUE, singular.ok = TRUE, contrasts = NULL, offset, ...)
{
    ...
}
```

### Inline If-Else

Preserved by default (R Core compatible). Use `expand_if = TRUE` to expand:

```r
# Default: preserved
x <- if (a) b else c

# With expand_if = TRUE
if (a) {
    x <- b
} else {
    x <- c
}
```

## Correctness Guarantees

rformat is designed around three invariants:

1. **Parse preservation.** Formatted output always parses. rformat uses R's own parser (`utils::getParseData()`) for tokenization and verifies output validity. If input parses, output parses.

2. **Semantic preservation.** Formatting changes only whitespace and style tokens. Assignment conversion (`=` to `<-`) and brace insertion are guided by R's parse tree token types (e.g., `EQ_ASSIGN` vs `EQ_SUB`), ensuring they never change meaning.

3. **Idempotency.** Formatting is a fixed point: `rformat(rformat(x)) == rformat(x)`. This is enforced by a stress test suite that formats every file twice and diffs the results. Continuation indentation uses depth-based offsets (matching the initial indentation pass) rather than column-aligned positions, specifically to guarantee stability across passes.

### Conservative Transforms

Structural rewrites (collapsing calls, adding braces, wrapping long lines, reformatting function definitions) are applied conservatively:

- Every transform pass is wrapped in a **parse gate** — if the result doesn't parse, the original is kept.
- Complex one-liner bodies (containing nested control flow or function literals) are left alone rather than risk mis-association.
- Large files (>250 lines) get reduced iteration budgets for expensive rewrite loops; very large files (>1500 lines) skip structural rewrites entirely and receive only base token formatting.
- Anonymous function literals inside calls are not reformatted — only named function definitions get signature/brace normalization.

### Stress Testing

The test suite (`lab/check_stress_pkgs.R`) formats every `.R` file from 100 CRAN source tarballs, checking:

- **Parse gate**: formatted code must parse without errors
- **Idempotency**: formatting twice produces identical output

Tested against 100 popular CRAN packages spanning data manipulation (dplyr, data.table), visualization (ggplot2, plotly), modeling (caret, xgboost, brms), infrastructure (Rcpp, rlang, vctrs), web (shiny, plumber), and more.

## Philosophy

This formatter follows [tinyverse](https://www.tinyverse.org) principles. The only dependency is `utils::getParseData()` for tokenization.

## License

GPL-3
