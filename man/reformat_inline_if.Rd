% tinyrox says don't edit this manually, but it can't stop you!
\name{reformat_inline_if}
\alias{reformat_inline_if}
\title{Reformat Inline If-Else Statements}
\usage{
reformat_inline_if(code, line_limit = 0L)
}
\arguments{
\item{code}{Formatted code string.}

\item{line_limit}{Only expand if-else on lines exceeding this limit.
Use 0 to expand all inline if-else.}
}
\value{
Code with reformatted inline if-else.
}
\description{
Expands inline if-else to multi-line format per style guide:
x <- if (cond) a else b
becomes:
if (cond) \{
x <- a
\} else \{
x <- b
\}
}
\keyword{internal}
