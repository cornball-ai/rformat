% tinyrox says don't edit this manually, but it can't stop you!
\name{build_line_index}
\alias{build_line_index}
\title{Build Line Index}
\usage{
build_line_index(terms)
}
\arguments{
\item{terms}{Enriched terminal DataFrame.}
}
\value{
Named list where names are line numbers (as strings) and values
  are integer vectors of row indices.
}
\description{
Creates a named list mapping output line numbers to row indices in the
terms DataFrame. Avoids repeated `which(terms$out_line == ln)` scans.
}
\keyword{internal}
