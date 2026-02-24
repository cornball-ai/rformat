% tinyrox says don't edit this manually, but it can't stop you!
\name{wrap_long_calls}
\alias{wrap_long_calls}
\title{Wrap Long Function Calls}
\usage{
wrap_long_calls(code, wrap = "paren", indent = 4L, line_limit = 80L)
}
\arguments{
\item{code}{Formatted code string.}

\item{wrap}{Continuation style: `"paren"` or `"fixed"`.}

\item{indent}{Indent size (integer) or string.}

\item{line_limit}{Maximum line length (default 80).}
}
\value{
Code with wrapped long calls.
}
\description{
Wraps function call lines that exceed the line limit by breaking at commas.
Continuation lines are aligned to the opening parenthesis.
}
\keyword{internal}
