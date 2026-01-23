---
title: "R Core Style Analysis"
output: simplermarkdown::md_document
vignette: >
  %\VignetteIndexEntry{R Core Style Analysis}
  %\VignetteEngine{simplermarkdown::mdweave_to_md}
  %\VignetteEncoding{UTF-8}
---

# R Core Style Analysis

This vignette documents the analysis of actual R Core source code that informed
the formatting choices in rformat.

## Background

When implementing a code formatter, a key question is: what style should we
target? For R, the natural reference is the R Core source code itself.

However, there's a catch: examining functions via `deparse()` in the console
shows *reconstructed* formatting, not the original source. To understand the
actual R Core style, we need to examine the source files directly.

## Analysis of Base R Functions

First, let's analyze the formatting patterns across exported functions in base R
packages using `deparse()` output:

```{r}
get_pkg_functions <- function(pkg)
{
    ns <- getNamespace(pkg)
    exports <- getNamespaceExports(pkg)
    funs <- Filter(function(nm) is.function(get(nm, envir = ns)), exports)
    lapply(setNames(funs, funs), function(nm) get(nm, envir = ns))
}

analyze_signature <- function(fn, name)
{
    txt <- deparse(fn)
    func_line_idx <- grep("^function\\s*\\(", txt)
    if (length(func_line_idx) == 0) return(NULL)
    func_line_idx <- func_line_idx[1]

    brace_idx <- grep("^\\{", txt)
    if (length(brace_idx) == 0) {
        brace_idx <- func_line_idx
    } else {
        brace_idx <- brace_idx[1]
    }

    sig_lines <- brace_idx - func_line_idx
    if (sig_lines == 0 && grepl("\\{\\s*$", txt[func_line_idx])) {
        sig_lines <- 1
    }

    formals_list <- formals(fn)
    n_args <- length(formals_list)
    first_line_len <- nchar(txt[func_line_idx])

    if (sig_lines <= 1) {
        style <- "single_line"
    } else {
        style <- "continuation"
    }

    list(
        name = name,
        n_args = n_args,
        sig_lines = sig_lines,
        first_line_len = first_line_len,
        style = style
    )
}
```

Now analyze functions from core packages:

```{r}
pkgs <- c("base", "stats", "utils", "graphics", "grDevices")
all_results <- list()

for (pkg in pkgs) {
    funs <- get_pkg_functions(pkg)
    results <- lapply(names(funs), function(nm) {
        tryCatch(analyze_signature(funs[[nm]], nm), error = function(e) NULL)
    })
    all_results <- c(all_results, Filter(Negate(is.null), results))
}

df <- do.call(rbind, lapply(all_results, function(x) {
    data.frame(
        name = x$name,
        n_args = x$n_args,
        style = x$style,
        stringsAsFactors = FALSE
    )
}))

cat("Total functions analyzed:", nrow(df), "\n\n")

cat("Style distribution:\n")
print(table(df$style))
cat("\nPercentages:\n")
print(round(prop.table(table(df$style)) * 100, 1))
```

Style by number of arguments:

```{r}
df$arg_group <- cut(df$n_args,
    breaks = c(-1, 0, 1, 2, 3, 5, 10, Inf),
    labels = c("0", "1", "2", "3", "4-5", "6-10", "11+"))
print(table(df$arg_group, df$style))
```

## Actual Source Code Analysis

The `deparse()` output differs from the actual source. Let's examine the real
R source files from the R repository.

Example from `lm.R` (fetched from r-source repository):

```
lm <- function (formula, data, subset, weights, na.action,
		method = "qr", model = TRUE, x = FALSE, y = FALSE,
		qr = TRUE, singular.ok = TRUE, contrasts = NULL,
		offset, ...)
{
    ret.x <- x
    ret.y <- y
```

Example from `lapply.R`:

```
lapply <- function (X, FUN, ...)
{
    FUN <- match.fun(FUN)
```

Using `cat -A` to reveal tabs (`^I`) vs spaces:

```
lm <- function (formula, data, subset, weights, na.action,$
^I^Imethod = "qr", model = TRUE, x = FALSE, y = FALSE,$
^I^Iqr = TRUE, singular.ok = TRUE, contrasts = NULL,$
^I^Ioffset, ...)$
{$
    ret.x <- x$
```

## Key Findings

**The R Core style is consistent, with one minor variation:**

### Consistent Rules

1. Space between `function` and `(` -- i.e. `function (` not `function(`
2. Arguments start on same line as `function (`
3. If args don't fit, continuation lines align (using tabs)
4. Closing `)` on same line as last argument
5. `{` always on its own line, aligned with start of statement
6. Never puts first arg on a new line (no "super vertical" style)

### One Variation

Body indentation uses either **4 spaces** OR **2 tabs** (display equivalently
with default tab width of 8, showing as 16 characters, but commonly rendered
as 8 with tab width 4). This is mixed even within the same file, likely due to
historical editor differences rather than intentional style variation.

## Style Statistics

| Args   | Single Line | Continuation |
|--------|-------------|--------------|
| 0      | 63          | 0            |
| 1      | 354         | 0            |
| 2      | 590         | 4            |
| 3      | 326         | 14           |
| 4-5    | 286         | 94           |
| 6-10   | 46          | 191          |
| 11+    | 1           | 81           |

**Summary:** 81% single-line, 19% continuation, 100% brace on own line.

## R Developer Guide Recommendation

The [R Dev Guide PR #256](https://github.com/r-devel/rdevguide/pull/256)
recommends these EditorConfig settings:

- **Indent size:** 4 spaces
- **Tab width:** 8 (for display)
- **Use spaces, not tabs**

This means the modern convention is 4 spaces, while the vintage R Core source
files used tabs (which displayed as 8 characters).

## rformat Implementation

Based on this analysis, rformat provides flexible indent options:

- **Default (4 spaces):** Modern style, matches R Dev Guide recommendation
- **Tabs:** For those who prefer tab characters

```{r eval=FALSE}
# Modern style (default) - 4 spaces
rformat("f <- function(x, y) { x + y }")

# Single tab per indent level
rformat("f <- function(x, y) { x + y }", indent = "\t")

# 2 spaces (tidyverse-ish)
rformat("f <- function(x, y) { x + y }", indent = 2L)
```

rformat uses R Core continuation style for function signatures:

- **Short signatures** stay on one line: `function (x, y)`
- **Long signatures** wrap with alignment to the opening paren
- **Opening brace** `{` always on its own line
