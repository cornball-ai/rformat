# rformat blog notes

## The paren alignment vs depth-based indent tension

The formatting pipeline has a `format_tokens` function that does depth-based
indentation -- it counts brace/paren nesting depth and indents each line by
`depth * 4 spaces`. That's the "canonical" pass.

When `reformat_one_function` wraps a long signature, it produces paren-aligned
continuation:

```r
format_tokens <- function (code, indent = 4L, wrap = "paren",
                           expand_if = FALSE, brace_style = "kr",
```

The continuation line is indented to column 28 (under the opening paren). But
the canonical pass sees that line as being inside 1 paren depth, so it
re-indents to `1 * 4 = 4 spaces`:

```r
format_tokens <- function (code, indent = 4L, wrap = "paren",
    expand_if = FALSE, brace_style = "kr",
```

This is by design -- the depth-based indent was chosen over paren-column
alignment for general code because paren alignment caused idempotency problems
(wrapping changes column positions, which changes alignment, which changes
wrapping...). But function definitions are the one exception where paren
alignment is the desired style, so `reformat_function_defs` needs to run *last*
to have the final word.

The fix: re-apply `reformat_function_defs` after the final canonical pass so it
overwrites the depth-based indent for function signatures specifically.

## The 2326-line file and iteration budgets

A code formatter that can't fully format its own source because the file is too
big for its own iteration budget is not a great look.

The `iteration_budget()` function was introduced to prevent expensive
parse/rewrite loops from timing out or producing non-idempotent output on very
large files. For files over 250 lines, it disabled structural rewrites like
`reformat_function_defs` entirely (0 iterations).

This meant `format_parsed.R` (2326 lines) only got base token formatting --
spacing, indentation, assignment conversion -- but no function signature
reformatting, no brace insertion, no call collapsing.

The fix was embarrassingly simple: break up the giant file into 6 files (largest
637 lines), then raise the thresholds to match. The old aggressive budgets were
a workaround for having one enormous file, not a principled design choice.

## Conservative transforms and the parse safety net

Every structural rewrite pass (collapsing calls, adding braces, wrapping long
lines, reformatting function definitions) is wrapped in `apply_if_parseable()`.
If a transform produces unparseable output, the original code is silently kept.

This means the formatter can be aggressive about *attempting* rewrites while
being safe about *applying* them. The parse gate catches cases where token
manipulation produces invalid R code -- something that's hard to prevent
statically when you're manipulating a token stream rather than an AST.

Combined with the iteration budget (which limits how many one-at-a-time
rewrites happen per pass), this gives a two-layer defense: don't try too many
things, and roll back the ones that break.

## Stress test results

Tested against 100 popular CRAN packages (~3700 R files). After the fixes:
- 46 previously-failing files (parse errors + idempotency failures) all pass
- 99.1% file-level success rate on the 30 hardest packages
- 100% success on regression test suite (small/fast packages)
