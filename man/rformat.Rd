% tinyrox says don't edit this manually, but it can't stop you!
\name{rformat}
\alias{rformat}
\title{Format R Code}
\usage{
rformat(code, indent = 4L, line_limit = 80L, wrap = "paren",
        brace_style = "kr", control_braces = FALSE, expand_if = FALSE,
        else_same_line = TRUE, function_space = FALSE)
}
\arguments{
\item{code}{Character string of R code to format.}

\item{indent}{Indentation per level: integer for spaces (default 4), or
character string for literal indent (e.g., `"\\t\\t"` for vintage R Core style).}

\item{line_limit}{Maximum line length before wrapping (default 80).}

\item{wrap}{Continuation style for long function signatures: `"paren"`
(default) aligns to opening parenthesis, `"fixed"` uses 8-space indent.}

\item{brace_style}{Brace placement for function definitions: `"kr"` (default)
puts opening brace on same line as `) \{`, `"allman"` puts it on a new line.}

\item{control_braces}{If TRUE, add braces to bare one-line control flow
bodies (e.g., `if (x) y` becomes `if (x) \{ y \}`). Default FALSE
matches R Core source code where 59\% of control flow bodies are bare.}

\item{expand_if}{Expand inline if-else to multi-line (default FALSE).}

\item{else_same_line}{If TRUE (default), fix `\}\\nelse` to `\} else \{` on
the same line. Matches R Core source code where 70\% use same-line else.}

\item{function_space}{If TRUE, add space before `(` in function definitions:
`function (x)` instead of `function(x)`. Default FALSE matches 96\% of
R Core source code.}
}
\value{
Formatted code as a character string.
}
\description{
Format R code string according to base R style conventions.
}
\examples{
# Basic formatting: spacing around operators
rformat("x<-1+2")

# Add braces to bare control-flow bodies
rformat("if(x>0) y<-1", control_braces = TRUE)

# Expand inline if-else to multi-line
rformat("x <- if (a) b else c", expand_if = TRUE)

# Wrap long function signatures (default: paren-aligned)
long_sig <- "f <- function(alpha, beta, gamma, delta, epsilon, zeta, eta, theta, iota, kappa) {\n    1\n}"
cat(rformat(long_sig), sep = "\n")

# Wrap with fixed 8-space continuation indent
cat(rformat(long_sig, wrap = "fixed"), sep = "\n")

# Allman brace style
rformat("f <- function(x) { x }", brace_style = "allman")
}
