# rformat

A code formatter for R, built on R's parser, written in base R.

`rformat` uses `parse()` and `getParseData()` to make formatting decisions
from the token stream and expression structure, not from regex or
indentation heuristics. All transforms operate on an enriched token
DataFrame.

No dependencies beyond base R.

## Installation

```r
remotes::install_github("cornball-ai/rformat")
```

## Usage

```r
library(rformat)

# Format a string
rformat("x<-1+2")
#> x <- 1 + 2

# Format a file (overwrites in place)
rformat_file("script.R")

# Format all R files in a directory
rformat_dir("R/")

# Dry run
rformat_file("script.R", dry_run = TRUE)
```

## Example

```r
rformat("f=function(x,y){
if(x>0)
y=mean(x,na.rm=TRUE)
else y=NA
}")
```

```r
f <- function(x, y) {
    if (x > 0)
        y <- mean(x, na.rm = TRUE)
    else y <- NA
}
```

## What it does

- Normalizes spacing around operators, commas, and keywords
- Indents by syntactic nesting depth
- Converts `=` to `<-` for assignment (where the parser confirms `EQ_ASSIGN`, not `EQ_SUB`)
- Wraps long lines at logical operators and commas
- Wraps long function signatures with continuation indent
- Collapses short multi-line calls back to one line
- Preserves comments and strings exactly
- Removes trailing whitespace and excess blank lines
- Optionally adds braces to bare control-flow bodies
- Optionally expands inline if-else to multi-line

## Options

| Parameter | Default | Description |
|-----------|---------|-------------|
| `indent` | `4L` | Spaces per level, or a string like `"\t"` |
| `line_limit` | `80L` | Line width before wrapping |
| `wrap` | `"paren"` | `"paren"` aligns to `(`, `"fixed"` uses 8-space continuation |
| `brace_style` | `"kr"` | `"kr"`: `){` same line. `"allman"`: `{` on its own line |
| `control_braces` | `FALSE` | Add braces to bare control-flow bodies |
| `expand_if` | `FALSE` | Expand all inline if-else to multi-line |
| `else_same_line` | `TRUE` | Join `}\nelse` to `} else` |
| `function_space` | `FALSE` | Space before `(` in `function(x)` |

Defaults are derived from [analysis](https://github.com/cornball-ai/rformat-lab/) of the 30 packages that ship with R.

## Correctness

**Parse preservation.** If input parses, output parses. Token types and
ordering are preserved. Strings and comments are never modified.

**Semantic preservation.** Only whitespace and style tokens change.
Assignment conversion and brace insertion are guided by parser token
types (`EQ_ASSIGN` vs `EQ_SUB`, structural body detection), so they
never change meaning.

**Idempotency.** `rformat(rformat(x)) == rformat(x)`. Verified across
126 CRAN and base R packages with randomized parameter combinations
(indent, wrap, brace_style, control_braces, line_limit, etc.):
0 failures, 0 idempotency exceptions.

## Stress testing

The [stress test suite](https://github.com/cornball-ai/rformat-lab)
formats every `.R` file from 126 packages (base, recommended, and
popular CRAN), checking that formatted code parses and that formatting
twice produces identical output. Tests run with randomized style
parameters to exercise all option combinations.

## License

GPL-3
