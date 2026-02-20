% tinyrox says don't edit this manually, but it can't stop you!
\name{wrap_long_operators}
\alias{wrap_long_operators}
\title{Wrap Long Lines at Operators}
\usage{
wrap_long_operators(code, line_limit = 80L)
}
\arguments{
\item{code}{Formatted code string.}

\item{line_limit}{Maximum line length (default 80).}
}
\value{
Code with wrapped long lines.
}
\description{
Wraps lines exceeding the line limit at logical operators (`||`, `&&`)
with continuation aligned to the first operand.
}
\keyword{internal}
