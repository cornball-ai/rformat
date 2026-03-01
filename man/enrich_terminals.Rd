% tinyrox says don't edit this manually, but it can't stop you!
\name{enrich_terminals}
\alias{enrich_terminals}
\title{Enrich Terminal Tokens for AST-Based Formatting}
\usage{
enrich_terminals(pd, orig_lines)
}
\arguments{
\item{pd}{Parse data from `getParseData()`.}

\item{orig_lines}{Original source lines (split by newline).}
}
\value{
Enriched terminal-token DataFrame with added columns:
  `out_line`, `out_order`, `out_text`, `brace_depth`, `paren_depth`,
  `pab`, `nesting_level`.
}
\description{
Parses code and returns an enriched terminal-token DataFrame with
per-token nesting state and output metadata. This is the foundation
of the parse-once architecture: parse once, enrich once, transform
the DataFrame through all passes, serialize to text once at the end.
}
\keyword{internal}
