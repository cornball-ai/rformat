% tinyrox says don't edit this manually, but it can't stop you!
\name{renumber_lines}
\alias{renumber_lines}
\title{Renumber Output Lines Sequentially}
\usage{
renumber_lines(terms)
}
\arguments{
\item{terms}{Enriched terminal DataFrame.}
}
\value{
Updated DataFrame with renumbered `out_line`.
}
\description{
After transforms that insert or remove lines, renumber `out_line` so
values are sequential starting from 1, preserving relative order and
gaps for blank lines.
}
\keyword{internal}
