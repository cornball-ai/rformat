% tinyrox says don't edit this manually, but it can't stop you!
\name{expand_one_call_if_arg}
\alias{expand_one_call_if_arg}
\title{Expand One Bare If-Else Argument in a Function Call}
\usage{
expand_one_call_if_arg(code, line_limit = 80L)
}
\arguments{
\item{code}{Code string.}

\item{line_limit}{Maximum line length (default 80).}
}
\value{
Modified code or NULL if no changes.
}
\description{
Finds the first bare if-else argument inside a function call on a line
exceeding the line limit and expands it to braced multi-line form.
}
\keyword{internal}
