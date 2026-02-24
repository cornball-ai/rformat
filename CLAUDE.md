# rformat Style Guide

This project formats R code following R Core style conventions, based on analysis of actual base R source code.

## Core Principles

1. Code reads top-to-bottom like ordinary procedural code
2. Whitespace serves structure, not aesthetics
3. Names describe objects, not transformations
4. Favor explicitness over brevity

## Indentation

- 4 spaces per level (R Core recommendation)
- Configurable via `indent` parameter

## Naming

- Use `snake_case` consistently
- Functions are verbs: `fit_model()`, `read_table()`
- Objects are nouns: `model`, `coef_table`
- Constants in `ALL_CAPS`
- Avoid single-letter names except loop indices (`i`, `j`, `k`)

## Assignment

- `<-` only, never `=`
- One assignment per line
- No chained assignment

## Spacing

- Spaces around operators
- Space after `function`: `function (x)`
- No space before `(` in function calls: `foo(x)`
- Space after commas
- Space after control flow keywords: `if (`, `for (`

## Control Flow

- Braces always, even for one-liners
- `else` on same line as closing brace: `} else {`
- Inline if-else optionally expanded via `expand_if` parameter

```r
if (x > 0) {
    y <- log(x)
} else {
    y <- NA
}
```

## Functions

- Space between `function` and `(`
- Short signatures stay on one line
- Long signatures wrap with continuation indent (default: align to paren)
- Default brace style is K&R (opening brace on same line); `brace_style = "allman"` for brace on its own line

```r
# Short signature
lapply <- function (X, FUN, ...) {
    body
}

# Long signature (default: paren alignment)
lm <- function (formula, data, subset, weights, na.action, method = "qr",
                model = TRUE, x = FALSE, y = FALSE, qr = TRUE,
                singular.ok = TRUE, contrasts = NULL, offset, ...) {
    body
}

# Long signature (wrap = "fixed": 8-space indent)
lm <- function (formula, data, subset, weights, na.action,
        method = "qr", model = TRUE, x = FALSE, y = FALSE,
        qr = TRUE, singular.ok = TRUE, contrasts = NULL, offset, ...) {
    body
}
```

## Error Handling

- Use `stop()` with clear messages
- Avoid silent failure
- `warning()` only when execution can safely continue

```r
if (!is.numeric(x)) {
    stop("`x` must be numeric")
}
```

## Data Access

- Prefer `[[` over `$` for programmatic access
- Never rely on partial matching
- Explicit namespaces (`stats::`, `utils::`) in packages

## Line Length

- 80 character limit for function signature wrapping
- Break lines at commas

## Column Positions in getParseData()

R's `getParseData()` column positions (`col1`, `col2`) have two behaviors:
- **Multi-byte characters**: Character-based, not byte-based. An em-dash (`—`, 3 bytes UTF-8) counts as 1 column.
- **Tabs**: Expanded to 8-column tab stops. A tab at column 1 advances to column 9.

This means `substring()` with `col1`/`col2` is **wrong** when the line contains tabs. Use the `col_to_charpos()` helper to convert tab-expanded columns to character positions, and `display_width()` for tab-expanded line lengths.

## Package Development

- Explicit namespaces required for external functions
- Minimal dependencies
