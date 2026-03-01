% tinyrox says don't edit this manually, but it can't stop you!
\name{make_token}
\alias{make_token}
\title{Create a Synthetic Token Row}
\usage{
make_token(token, text, out_line, out_order, parent = 0L)
}
\arguments{
\item{token}{Token type string (e.g., `"'\{'"`, `"'\}'"`, `"','"`)}

\item{text}{Token text (e.g., `"\{"`, `"\}"`, `","`)}

\item{out_line}{Target output line.}

\item{out_order}{Sort order within the line.}

\item{parent}{Parent node ID (default 0).}
}
\value{
Single-row data frame.
}
\description{
Helper to build a single token row for insertion.
}
\keyword{internal}
