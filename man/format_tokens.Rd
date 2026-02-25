% tinyrox says don't edit this manually, but it can't stop you!
\name{format_tokens}
\alias{format_tokens}
\title{Format R Code Using Token-Based Parsing}
\usage{
format_tokens(
  code,
  indent = 4L,
  wrap = "paren",
  expand_if = FALSE,
  brace_style = "kr",
  line_limit = 80L,
  function_space = FALSE,
  control_braces = FALSE
)
}
\arguments{
\item{code}{Character string of R code.}

\item{indent}{Integer for spaces (default 4), or character string for
literal indent (e.g., `"\\t\\t"` for vintage R Core style).}

\item{wrap}{Continuation style: `"paren"` (default) aligns to opening
parenthesis, `"fixed"` uses 8-space indent.}

\item{expand_if}{Expand inline if-else to multi-line (default FALSE).}

\item{brace_style}{Brace placement: `"kr"` (same line) or `"allman"` (new line).}

\item{line_limit}{Maximum line length before wrapping (default 80).}

\item{function_space}{If TRUE, add space before `(` in function definitions.}

\item{control_braces}{If TRUE, add braces to bare one-line control flow bodies.}
}
\value{
Formatted code as character string.
}
\description{
Internal function to format R code using getParseData tokens.
Calculates proper indentation based on nesting depth.
}
\keyword{internal}
