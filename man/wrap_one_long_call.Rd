% tinyrox says don't edit this manually, but it can't stop you!
\name{wrap_one_long_call}
\alias{wrap_one_long_call}
\title{Wrap One Long Function Call}
\usage{
wrap_one_long_call(code, wrap = "paren", indent = 4L, line_limit = 80L)
}
\arguments{
\item{code}{Code string.}

\item{wrap}{Continuation style: `"paren"` or `"fixed"`.}

\item{indent}{Indent size (integer) or string.}

\item{line_limit}{Maximum line length (default 80).}
}
\value{
Modified code or NULL if no changes.
}
\description{
Finds the first single-line function call that exceeds the line limit
and wraps it at commas with paren-aligned continuation.
}
\keyword{internal}
