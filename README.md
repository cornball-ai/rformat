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

## Philosophy

This formatter follows [tinyverse](https://www.tinyverse.org) principles. The only dependency is `utils::getParseData()` for tokenization.

## License

GPL-3
