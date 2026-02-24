% tinyrox says don't edit this manually, but it can't stop you!
\name{split_toplevel}
\alias{split_toplevel}
\title{Split Code into Top-Level Expressions}
\usage{
split_toplevel(code)
}
\arguments{
\item{code}{Character string of R code.}
}
\value{
List of `list(text = "...", is_expr = TRUE/FALSE)` pairs.
}
\description{
Parses code to find top-level expressions, returning a list of chunks.
Each chunk is either an expression (code string) or an inter-expression
gap (comments, blank lines). Chunks concatenate back to the original.
}
\keyword{internal}
