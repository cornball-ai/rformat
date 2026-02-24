% tinyrox says don't edit this manually, but it can't stop you!
\name{find_token_pos_in_formatted}
\alias{find_token_pos_in_formatted}
\title{Find Token Position in Formatted Line Output}
\usage{
find_token_pos_in_formatted(tokens, idx)
}
\arguments{
\item{tokens}{Data frame of tokens for one line (ordered by col1).}

\item{idx}{Index into `tokens` of the target token.}
}
\value{
1-based character position of that token in the formatted output.
}
\description{
Computes the 1-based character position where the token at index `idx`
starts in the output of `format_line_tokens(tokens)`. This replays the
spacing logic to determine the exact output column.
}
\keyword{internal}
