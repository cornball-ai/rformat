# Base R Style Guide

This project follows a strict base-R style: clarity, predictability, and minimal magic. No tidyverse assumptions. No pipeline aesthetics.

## Core Principles

1. Code reads top-to-bottom like ordinary procedural code
2. Whitespace serves structure, not aesthetics
3. Names describe objects, not transformations
4. No hidden evaluation, no NSE, no clever shortcuts
5. Favor explicitness over brevity

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

## Pipes

**No pipes.** Not magrittr, not native `|>`. Use intermediate variables or nested calls.

## Spacing

- Spaces around operators
- No alignment columns for "pretty" code
- One blank line between logical blocks

## Control Flow

- Braces always, even for one-liners
- `else` on same line as closing brace
- No inline `if` expressions

```r
# Correct
if (x > 0) {
  stop("`x` must be positive")
}

if (p < 0.05) {
  label <- "sig"
} else {
  label <- "ns"
}

# Wrong
label <- if (p < 0.05) "sig" else "ns"
y <- if (x > 0) log(x) else NA
```

## Conditionals

- `ifelse()` is discouraged for scalar logic; use explicit branches
- `ifelse()` acceptable for vectorized operations when intent is clear

## Loops Over Cleverness

- Prefer `for` loops to nested `*apply()` when clarity matters
- Preallocate vectors
- No side effects hidden in `lapply()`

```r
out <- numeric(n)
for (i in seq_len(n)) {
  out[i] <- x[i] ^ 2
}
```

## Functions

- Short, single-purpose
- Arguments listed vertically if more than 2–3
- Default values explicit
- Return value on its own line

```r
compute_ci <- function(
  x,
  alpha = 0.05,
  na.rm = TRUE
) {
  if (na.rm) {
    x <- x[!is.na(x)]
  }

  q <- quantile(x, c(alpha / 2, 1 - alpha / 2))
  q
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

```r
mean_val <- stats::mean(df[["value"]], na.rm = TRUE)
```

## Line Formatting

- 80–100 character soft limit
- Break lines at commas, not operators
- No vertical alignment

```r
# Correct
result <- some_function(
  x,
  y,
  z
)

# Wrong
result <- some_function(x,
                        y,
                        z)
```

## Package Development

- Explicit namespaces required for external functions
- Minimal dependencies
- No tidyverse dependencies
