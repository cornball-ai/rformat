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
  brace_style = "kr"
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
