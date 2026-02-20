% tinyrox says don't edit this manually, but it can't stop you!
\name{collapse_one_call}
\alias{collapse_one_call}
\title{Collapse One Multi-Line Function Call}
\usage{
collapse_one_call(code)
}
\arguments{
\item{code}{Code string.}
}
\value{
Modified code or NULL if no changes.
}
\description{
Finds the first multi-line function call that can fit on one line
and collapses it. Skips calls containing comments.
}
\keyword{internal}
