% tinyrox says don't edit this manually, but it can't stop you!
\name{needs_space}
\alias{needs_space}
\title{Determine If Space Needed Between Tokens}
\usage{
needs_space(prev, tok, prev_prev = NULL)
}
\arguments{
\item{prev}{Previous token (data frame row).}

\item{tok}{Current token (data frame row).}

\item{prev_prev}{Token before prev (data frame row or NULL), for unary detection.}
}
\value{
Logical.
}
\description{
Determine If Space Needed Between Tokens
}
\keyword{internal}
