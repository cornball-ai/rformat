% tinyrox says don't edit this manually, but it can't stop you!
\name{wrap_one_long_operator}
\alias{wrap_one_long_operator}
\title{Wrap One Long Line at an Operator}
\usage{
wrap_one_long_operator(code, line_limit = 80L)
}
\arguments{
\item{code}{Code string.}

\item{line_limit}{Maximum line length (default 80).}
}
\value{
Modified code or NULL if no changes.
}
\description{
Finds the first line exceeding the limit that contains a top-level
logical operator and wraps there. The operator stays at the end of the
first line, continuation is aligned to the first operand.
}
\keyword{internal}
