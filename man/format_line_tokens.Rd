% tinyrox says don't edit this manually, but it can't stop you!
\name{format_line_tokens}
\alias{format_line_tokens}
\title{Format Tokens on a Single Line}
\usage{
format_line_tokens(tokens, prev_token = NULL, prev_prev_token = NULL)
}
\arguments{
\item{tokens}{Data frame of tokens for one line.}

\item{prev_token}{Optional token to treat as the previous token when
formatting a token subset (e.g., suffix after a collapsed call).}

\item{prev_prev_token}{Optional token before prev_token for unary detection.}
}
\value{
Formatted line content (no leading whitespace).
}
\description{
Format Tokens on a Single Line
}
\keyword{internal}
