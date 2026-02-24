% tinyrox says don't edit this manually, but it can't stop you!
\name{compute_nesting}
\alias{compute_nesting}
\title{Compute Nesting Depth Per Line}
\usage{
compute_nesting(terminals, n_lines)
}
\arguments{
\item{terminals}{Terminal token data frame from `getParseData()`,
ordered by `line1, col1`.}

\item{n_lines}{Number of source lines.}
}
\value{
Named list with `line_indent`, `line_end_brace`,
  `line_end_paren`, `line_end_pab` (all integer vectors of length
  `n_lines`).
}
\description{
Shared function used by `format_tokens` and wrap passes to compute
identical depth-based indent levels from the parse tree.
}
\keyword{internal}
