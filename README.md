# rformat

Token-based R code formatter following a strict Base R style guide.

## Installation

```r
remotes::install_github("cornball-ai/rformat")
```

## Usage

```r
library(rformat)

# Format a code string
code <- "x<-1+2"
rformat(code)
#> x <- 1 + 2

# Format a file
rformat_file("R/my_script.R")

# Format all R files in a directory
rformat_dir("R/")
```

## Style Rules

- 2-space indentation
- Spaces around binary operators (`<-`, `+`, `==`, etc.)
- No space before `(` in function calls: `foo(x)` not `foo (x)`
- Space after commas: `c(1, 2, 3)`
- Space after control flow keywords: `if (`, `for (`, `while (`
- Braces on same line: `if (x) {`
- Function arguments on separate lines (when 2+ args)
- No inline if-else expressions

### Function Definitions

```r
# Before
compute_ci <- function(x, alpha = 0.05, na.rm = TRUE) { ... }

# After
compute_ci <- function(
  x,
  alpha = 0.05,
  na.rm = TRUE
) {
  ...
}
```

### Inline If-Else

```r
# Before
label <- if (p < 0.05) "sig" else "ns"

# After
if (p < 0.05) {
  label <- "sig"
} else {
  label <- "ns"
}
```

## Philosophy

This formatter follows [tinyverse](https://www.tinyverse.org) principles - base R only, minimal dependencies. The only dependency is base R's `getParseData()` for tokenization.

## License
MIT
