<!--
%\VignetteIndexEntry{R Core Style Analysis}
%\VignetteEngine{simplermarkdown::mdweave_to_html}
%\VignetteEncoding{UTF-8}
-->
---
title: "R Core Style Analysis"
---

# R Core Style Analysis

This vignette documents the analysis of actual R source code that informed
the formatting choices in rformat. We analyzed every `.R` file from the 22
packages that ship with R: 14 base packages and 8 recommended packages.

## Method

We downloaded the R 4.5.2 source tarball and extracted the R/ directories
from all base packages. For recommended packages (codetools, lattice, MASS,
Matrix, mgcv, nlme, nnet, survival) we used the CRAN source tarballs. We
parsed each file with `getParseData()` and tallied formatting conventions
across all function definitions.

An earlier version of this analysis used `deparse()` output from 5 packages.
That approach shows *reconstructed* formatting, not the original source. The
numbers below come from the actual source files.

## Results: Base R (14 packages)

612 files, 172,810 lines, 8,426 function definitions.

### Brace style

| Style | Count | % |
|-------|------:|--:|
| K&R (same line) | 2,985 | 52.9 |
| Allman (own line) | 2,655 | 47.1 |
| No braces (one-liner) | 2,786 | — |

Base R is genuinely mixed. Neither style dominates.

### Indentation

| Style | Lines | % |
|-------|------:|--:|
| Space-indented | 116,187 | 89.0 |
| Tab-indented | 14,328 | 11.0 |

Most common space indents: 4 (40,412), 8 (24,324), 12 (14,257), 16 (7,054).
The 4-space pattern is dominant.

### Space after `function`

| Style | Count | % |
|-------|------:|--:|
| `function(` | 8,076 | 95.8 |
| `function (` | 350 | 4.2 |

The no-space form is overwhelmingly dominant. The space form appears in a few
well-known functions (`lm`, `lapply`, `glm`) but is the exception, not the rule.

### Function signatures

| Style | Count | % |
|-------|------:|--:|
| Single-line | 7,553 | 89.6 |
| Multi-line | 873 | 10.4 |

Of the 873 multi-line signatures:

| Continuation style | Count |
|--------------------|------:|
| Paren-aligned | 699 |
| Tab-indented | 144 |
| Other | 30 |

Paren alignment is the clear convention for multi-line signatures (80%).

## Results: Base + Recommended (22 packages)

920 files, 263,807 lines, 12,306 function definitions.

Adding the 8 recommended packages barely shifts the percentages:

| Metric | Base 14 | All 22 |
|--------|--------:|-------:|
| K&R braces | 52.9% | 53.6% |
| Allman braces | 47.1% | 46.4% |
| `function(` | 95.8% | 96.5% |
| Space-indented | 89.0% | 89.9% |
| Paren-aligned cont. | 80.1% | 78.6% |

The recommended packages follow the same conventions as base.

## Source examples

Example from `stats/lm.R` — one of the minority that uses `function (`:

```
lm <- function (formula, data, subset, weights, na.action,
		method = "qr", model = TRUE, x = FALSE, y = FALSE,
		qr = TRUE, singular.ok = TRUE, contrasts = NULL,
		offset, ...)
{
    ret.x <- x
    ret.y <- y
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

## Key findings

1. **Brace style is mixed** — roughly 53/47 K&R vs Allman. Neither is "the"
   R Core style. Individual packages are often internally consistent, but
   there is no project-wide convention.
2. **`function(` without space** is the norm (96%). The `function (` form
   that appears in textbooks and style guides is actually rare in the source.
3. **4-space indentation** is dominant (89% space-indented, 4 being the most
   common width), with 11% tab-indented lines.
4. **Paren-aligned continuation** is the clear choice for multi-line
   signatures (80%).
5. **Arguments start on the same line** as `function(` — R Core never puts
   the first argument on a new line.
6. **Closing `)` on same line as last argument** — no dangling paren style.

## R Developer Guide recommendation

The [R Dev Guide PR #256](https://github.com/r-devel/rdevguide/pull/256)
recommends these EditorConfig settings:

- **Indent size:** 4 spaces
- **Tab width:** 8 (for display)
- **Use spaces, not tabs**

This codifies the majority practice. The tab-indented code is historical.

## rformat defaults

rformat adopts the conventions where R Core is consistent (4-space indent,
paren-aligned continuation, arguments on same line) and makes opinionated
choices where R Core is mixed or at odds with modern practice:

| Convention | R Core source | rformat default | Option to match R Core |
|------------|---------------|-----------------|----------------------|
| Brace style | ~53% K&R, ~47% Allman | K&R | `brace_style = "allman"` |
| `function` spacing | 96% no space | space | — |
| Indentation | 89% spaces, 11% tabs | 4 spaces | `indent = "\t"` |
| Continuation | 80% paren-aligned | paren-aligned | `wrap = "fixed"` |

The `function (` spacing follows the convention established by R's own
`deparse()` output and widely adopted by R style guides, even though the
source files themselves mostly omit the space.
