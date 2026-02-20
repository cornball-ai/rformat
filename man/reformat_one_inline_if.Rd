% tinyrox says don't edit this manually, but it can't stop you!
\name{reformat_one_inline_if}
\alias{reformat_one_inline_if}
\title{Reformat One Inline If-Else Statement}
\usage{
reformat_one_inline_if(code, line_limit = 0L)
}
\arguments{
\item{code}{Code string.}

\item{line_limit}{Only expand if-else on lines exceeding this limit.
Use 0 to expand all inline if-else.}
}
\value{
Modified code or NULL if no changes.
}
\description{
Reformat One Inline If-Else Statement
}
\keyword{internal}
