% tinyrox says don't edit this manually, but it can't stop you!
\name{token_indent_level}
\alias{token_indent_level}
\title{Compute Indent Level for a Token}
\usage{
token_indent_level(terms, idx)
}
\arguments{
\item{terms}{Enriched terminal DataFrame.}

\item{idx}{Index of the token (must be first on its line for indent).}
}
\value{
Integer indent level.
}
\description{
Returns the depth-based indent level that should apply to a token's line.
For closing tokens (`\}`, `)`, `]`), the indent is one less than the token's
own nesting level (they outdent to match their opening counterpart).
}
\keyword{internal}
