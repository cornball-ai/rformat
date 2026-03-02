% tinyrox says don't edit this manually, but it can't stop you!
\name{line_index_width}
\alias{line_index_width}
\title{Compute Display Width Using Line Index}
\usage{
line_index_width(terms, lidx, line_num, indent_str)
}
\arguments{
\item{terms}{Enriched terminal DataFrame.}

\item{lidx}{Line index from `build_line_index()`.}

\item{line_num}{The output line number.}

\item{indent_str}{Indent string.}
}
\value{
Display width of the line.
}
\description{
Like `ast_line_width()` but uses a pre-built line index for O(1) lookup.
}
\keyword{internal}
