% tinyrox says don't edit this manually, but it can't stop you!
\name{collapse_calls}
\alias{collapse_calls}
\title{Collapse Multi-Line Function Calls}
\usage{
collapse_calls(code)
}
\arguments{
\item{code}{Formatted code string.}
}
\value{
Code with collapsed function calls.
}
\description{
Collapses function calls spanning multiple lines into a single line.
Long lines are re-wrapped by `wrap_long_calls()` afterward. For example:
\preformatted{c(x,
y,
z
)}
becomes `c(x, y, z)`.
}
\keyword{internal}
