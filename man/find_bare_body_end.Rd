% tinyrox says don't edit this manually, but it can't stop you!
\name{find_bare_body_end}
\alias{find_bare_body_end}
\title{Check if a Body Token Range is a Complete Statement}
\usage{
find_bare_body_end(terms, body_start)
}
\arguments{
\item{terms}{Enriched terminal DataFrame.}

\item{body_start}{Integer row index where the bare body begins.}
}
\value{
Integer row index of the last token in the bare body.
}
\description{
Returns FALSE if the body has unclosed parens/brackets or ends
with an operator that expects a continuation (assignment, binary ops).
}
\keyword{internal}
