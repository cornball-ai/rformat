% tinyrox says don't edit this manually, but it can't stop you!
\name{reformat_one_function}
\alias{reformat_one_function}
\title{Reformat One Function Definition}
\usage{
reformat_one_function(
  code,
  wrap = "paren",
  brace_style = "kr",
  line_limit = 80L
)
}
\arguments{
\item{code}{Code string.}

\item{wrap}{Continuation style: `"paren"` aligns to opening parenthesis,
`"fixed"` uses 8-space indent.}

\item{brace_style}{Brace placement: `"kr"` (same line) or `"allman"` (new line).}

\item{line_limit}{Maximum line length before wrapping (default 80).}
}
\value{
Modified code or NULL if no changes.
}
\description{
Uses R Core continuation style: args on one line if they fit,
otherwise wrap with continuation indent.
}
\keyword{internal}
