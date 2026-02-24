% tinyrox says don't edit this manually, but it can't stop you!
\name{compute_indent_at_col}
\alias{compute_indent_at_col}
\title{Compute Indent at a Column Position}
\usage{
compute_indent_at_col(nesting, line_toks, line_num, break_col)
}
\arguments{
\item{nesting}{Result from compute_nesting().}

\item{line_toks}{Tokens on the line.}

\item{line_num}{Line number.}

\item{break_col}{Column position to stop at (inclusive).}
}
\value{
Integer indent level.
}
\description{
Walks tokens on a line up to a given column, tracking braces and parens
exactly as compute_nesting does. Returns the indent level that a
hypothetical continuation line would receive.
}
\keyword{internal}
