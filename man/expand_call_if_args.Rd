% tinyrox says don't edit this manually, but it can't stop you!
\name{expand_call_if_args}
\alias{expand_call_if_args}
\title{Expand Bare If-Else Arguments in Function Calls}
\usage{
expand_call_if_args(code, line_limit = 80L)
}
\arguments{
\item{code}{Formatted code string.}

\item{line_limit}{Maximum line length (default 80).}
}
\value{
Code with expanded if-else call arguments.
}
\description{
When a function call has a bare `if (cond) expr else expr` argument
that doesn't fit on a continuation line, expand it to braced multi-line
form. Short if-else args stay inline.
}
\keyword{internal}
