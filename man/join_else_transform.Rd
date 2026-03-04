% tinyrox says don't edit this manually, but it can't stop you!
\name{join_else_transform}
\alias{join_else_transform}
\title{Join Else to Preceding Close Brace}
\usage{
join_else_transform(terms)
}
\arguments{
\item{terms}{Enriched terminal DataFrame.}
}
\value{
Updated DataFrame.
}
\description{
AST transform that moves ELSE tokens (and any following opening brace)
to the same output line as the preceding `\}`. Skips if a COMMENT
exists between `\}` and `else`.
}
\keyword{internal}
