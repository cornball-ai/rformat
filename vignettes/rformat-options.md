<!--
%\VignetteIndexEntry{Formatting Options}
%\VignetteEngine{simplermarkdown::mdweave_to_html}
%\VignetteEncoding{UTF-8}
-->
---
title: "Formatting Options"
---

# Formatting Options

rformat applies base R style conventions to R code. All transforms operate
on the token stream from `parse()` + `getParseData()`, so they never change
the meaning of your code -- only its whitespace and style.

This vignette walks through each option with before/after examples.

## Default formatting

With no options, rformat normalizes spacing, converts `=` assignment to `<-`,
and indents by nesting depth.

```{.R}
library(rformat)

cat(rformat("x=1+2"))

cat(rformat("f=function(x,y){
if(x>0)
y=mean(x,na.rm=TRUE)
else y=NA
}"))
```

## `indent`

Indentation per nesting level. Default is 4 spaces. Pass an integer for
spaces or a character string for a literal indent.

```{.R}
# 2 spaces per level
cat(rformat("f <- function(x) {\nif (x) {\ny\n}\n}", indent = 2L))

# Tab indent (shown as \t in output)
rformat("f <- function(x) {\nif (x) {\ny\n}\n}", indent = "\t")
```

## `line_limit`

Maximum line width before wrapping kicks in (default 80). This affects
function signature wrapping and long expression wrapping.

```{.R}
long_call <- "result <- some_function(alpha, bravo, charlie, delta)"

# Default 80: fits on one line
cat(rformat(long_call))

# Narrow limit: forces wrapping
cat(rformat(long_call, line_limit = 40))
```

## `wrap`

Controls continuation indent style for wrapped function signatures.

- `"paren"` (default): aligns continuation to the opening parenthesis
- `"fixed"`: uses an 8-space continuation indent

```{.R}
long_sig <- paste0(
    "my_function <- function(alpha, beta, gamma, ",
    "delta, epsilon, zeta, eta, theta, iota) ",
    "{\n    1\n}")

# Paren-aligned (default): continuation aligns to (
cat(rformat(long_sig, wrap = "paren"))

# Fixed: continuation uses 8-space indent
cat(rformat(long_sig, wrap = "fixed"))
```

## `brace_style`

Controls opening brace placement for function definitions.

- `"kr"` (default): K&R style, opening brace on same line as `)`
- `"allman"`: opening brace on its own line

```{.R}
long_def <- paste0(
    "my_function <- function(alpha, beta, gamma, ",
    "delta, epsilon, zeta, eta, theta, iota) ",
    "{\n    y <- 1\n    y\n}")

# K&R (default): { on same line as )
cat(rformat(long_def, brace_style = "kr"))

# Allman: { on its own line
cat(rformat(long_def, brace_style = "allman"))
```

## `control_braces`

When TRUE, adds braces to bare (unbraced) control flow bodies. Default
FALSE matches R Core source code, where 59% of control flow bodies are bare.

```{.R}
code <- "f <- function(x) {
if (x > 0) y <- log(x)
if (x < 0) stop('negative')
}"

# Default: bare bodies left alone
cat(rformat(code))

# Add braces
cat(rformat(code, control_braces = TRUE))
```

## `expand_if`

When TRUE, expands inline if-else expressions to multi-line form.

```{.R}
code <- "y <- if (x > 0) log(x) else NA"

# Default: inline if-else preserved
cat(rformat(code))

# Expanded to multi-line
cat(rformat(code, expand_if = TRUE))
```

## `else_same_line`

When TRUE (default), joins `} else` on the same line. Matches R Core
source code where 70% use same-line else. This also repairs a common
problem: `else` on its own line is a parse error at top level in R,
so this option makes such code formattable.

```{.R}
# } and else on separate lines -- normally a top-level parse error.
# else_same_line (the default) joins them and formats:
code <- "if (x) {\n    1\n}\nelse {\n    2\n}"
cat(rformat(code))
```

## `function_space`

When TRUE, adds a space before `(` in function definitions:
`function (x)` instead of `function(x)`. Default FALSE matches 96% of
R Core source code.

```{.R}
code <- "f <- function(x, y) {\n    x + y\n}"

# Default: no space
cat(rformat(code))

# With space
cat(rformat(code, function_space = TRUE))
```

## Combining options

Options compose naturally. Here is a more opinionated style that adds
braces, expands if-else, and uses Allman braces with 2-space indent:

```{.R}
code <- "f=function(x){
y=if(x>0) log(x) else NA
y
}"

cat(rformat(code,
    indent = 2L,
    brace_style = "allman",
    control_braces = TRUE,
    expand_if = TRUE))
```

## Formatting files and directories

`rformat_file()` formats a file in place (or to a new path).
`rformat_dir()` formats all `.R` files in a directory. Both accept the
same options as `rformat()`.

```{.R}
# Format a file (dry run)
f <- tempfile(fileext = ".R")
writeLines("x=1+2", f)
rformat_file(f, dry_run = TRUE)

# Format all R files in a directory (dry run)
d <- file.path(tempdir(), "rformat_demo")
dir.create(d, showWarnings = FALSE)
writeLines("y = 3", file.path(d, "example.R"))
rformat_dir(d, dry_run = TRUE)
unlink(d, recursive = TRUE)
unlink(f)
```
