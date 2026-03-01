% tinyrox says don't edit this manually, but it can't stop you!
\name{ast_line_width}
\alias{ast_line_width}
\title{Compute Display Width of an Output Line}
\usage{
ast_line_width(terms, line_num, indent_str)
}
\arguments{
\item{terms}{Enriched terminal DataFrame (sorted by out_line, out_order).}

\item{line_num}{The output line number.}

\item{indent_str}{Indent string (e.g., `"    "` for 4 spaces).}
}
\value{
Display width of the line.
}
\description{
Sums token text widths plus inter-token spaces for a given output line.
}
\keyword{internal}
