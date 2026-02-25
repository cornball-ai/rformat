% tinyrox says don't edit this manually, but it can't stop you!
\name{rformat}
\alias{rformat}
\title{Format R Code}
\usage{
rformat(
  code,
  indent = 4L,
  wrap = "paren",
  expand_if = FALSE,
  brace_style = "kr",
  line_limit = 80L,
  function_space = FALSE,
  control_braces = FALSE,
  else_same_line = TRUE
)
}
\arguments{
\item{code}{Character string of R code to format.}

\item{indent}{Indentation per level: integer for spaces (default 4), or
character string for literal indent (e.g., `"\\t\\t"` for vintage R Core style).}

\item{wrap}{Continuation style for long function signatures: `"paren"`
(default) aligns to opening parenthesis, `"fixed"` uses 8-space indent.}

\item{expand_if}{Expand inline if-else to multi-line (default FALSE).}

\item{brace_style}{Brace placement for function definitions: `"kr"` (default)
puts opening brace on same line as `) \{`, `"allman"` puts it on a new line.}

\item{line_limit}{Maximum line length before wrapping (default 80).}

\item{function_space}{If TRUE, add space before `(` in function definitions:
`function (x)` instead of `function(x)`. Default FALSE matches 96\% of
R Core source code.}

\item{control_braces}{If TRUE, add braces to bare one-line control flow
bodies (e.g., `if (x) y` becomes `if (x) \{ y \}`). Default FALSE
matches R Core source code where 59\% of control flow bodies are bare.}

\item{else_same_line}{If TRUE (default), fix `\}\nelse` to `\} else \{` on
the same line. Matches R Core source code where 70\% use same-line else.}
}
\value{
Formatted code as a character string.
}
\description{
Format R code string according to base R style conventions.
}
\examples{
rformat("x<-1+2")
rformat("if(x>0){y<-1}")
# Expand inline if-else
rformat("x <- if (a) b else c", expand_if = TRUE)
# Allman brace style (legacy)
rformat("f <- function(x) { x }", brace_style = "allman")
}
