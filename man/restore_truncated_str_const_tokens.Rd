% tinyrox says don't edit this manually, but it can't stop you!
\name{restore_truncated_str_const_tokens}
\alias{restore_truncated_str_const_tokens}
\title{Restore Truncated String Constant Token Text}
\usage{
restore_truncated_str_const_tokens(terminals, orig_lines)
}
\arguments{
\item{terminals}{Terminal token data frame from `getParseData()`.}

\item{orig_lines}{Original source lines.}
}
\value{
`terminals` with long `STR_CONST` text restored.
}
\description{
`utils::getParseData()` truncates long `STR_CONST` token text. Reconstruct the
original literal from source lines so token-based rewrite passes can round-trip
long strings without introducing parse-invalid placeholders.
}
\keyword{internal}
