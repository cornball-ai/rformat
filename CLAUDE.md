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
- No space before `(` in `function(x)` or calls `foo(x)`
- Space after commas
- Space after control flow keywords: `if (`, `for (`

## Control Flow

- Bare bodies left alone by default; `control_braces = TRUE` adds braces
- `else` on same line as closing brace: `} else {` (default); `else_same_line = FALSE` to preserve
- Inline if-else optionally expanded via `expand_if` parameter

```r
if (x > 0) {
    y <- log(x)
} else {
    y <- NA
}
```

## Functions

- No space between `function` and `(` (default; `function_space = TRUE` for space)
- Short signatures stay on one line
- Long signatures wrap with continuation indent (default: align to paren)
- Default brace style is K&R (opening brace on same line); `brace_style = "allman"` for brace on its own line

```r
# Short signature
lapply <- function(X, FUN, ...) {
    body
}

# Long signature (default: paren alignment)
lm <- function(formula, data, subset, weights, na.action, method = "qr",
                model = TRUE, x = FALSE, y = FALSE, qr = TRUE,
                singular.ok = TRUE, contrasts = NULL, offset, ...) {
    body
}

# Long signature (wrap = "fixed": 8-space indent)
lm <- function(formula, data, subset, weights, na.action,
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

## Stress Test Bug-Squashing Routine

All 126 stress test packages are pre-extracted at `~/.cache/rformat_cran_src/extracted/<pkg>/R/`. Use `~/rformat-lab/extract_packages.sh` to re-extract if needed.

### Scripts

- `~/rformat-lab/stress_test.R` — per-package stress test (random params per file)
- `~/rformat-lab/stress_one_combo.R` — test one file with one combo index (for parallel)
- `/tmp/debug_fails.R` — show parse error context for a file+params

### Routine

1. **Fix FAILs** — test only on failing files:
   ```bash
   # Reproduce failures (parallel by file x combo)
   parallel -j8 --colsep ' ' r /tmp/debug_fails.R {1} {2} {3} {4} {5} {6} {7} {8} {9} :::: /tmp/fail_jobs.txt 2>/dev/null
   ```

2. **Fix IDEMPs** — test only on failing files (same pattern).

3. **Test on bottom 63 packages** (fastest half):
   ```bash
   cd ~/rformat-lab && tail -63 stress_timing.csv | cut -d, -f1 | tr -d '"' | parallel -j16 --timeout 300 r stress_test.R {} 2>/dev/null | grep -E "FAIL|IDEMP|_DONE_"
   ```

4. **Return to 1** if any FAILs or IDEMPs.

5. **Full 126-package stress test**:
   ```bash
   cd ~/rformat-lab && r -e 'cat(read.csv("stress_timing.csv")$package, sep="\n")' | parallel -j16 --timeout 900 r stress_test.R {} 2>/dev/null | grep -E "FAIL|IDEMP|_DONE_"
   ```

6. **Return to 1** if needed.

7. **Success** — 0 FAIL, 0 IDEMP across all 126 packages.

### Test coverage rule

Every FAIL or IDEMP fix **must** have a corresponding test in `inst/tinytest/test_ast.R` or `inst/tinytest/test_rformat.R` that reproduces the bug pattern. This prevents regressions when future changes are made.

### Testing specific files with all 10 strategic combos

```bash
cd ~/rformat-lab && parallel -j10 --timeout 120 r stress_one_combo.R {1} {2} ::: /path/to/file.R ::: $(seq 1 10) 2>/dev/null
```

## Package Development

- Explicit namespaces required for external functions
- Minimal dependencies
