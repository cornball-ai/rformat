% tinyrox says don't edit this manually, but it can't stop you!
\name{recompute_nesting}
\alias{recompute_nesting}
\title{Recompute Nesting State After Structural Changes}
\usage{
recompute_nesting(terms)
}
\arguments{
\item{terms}{Enriched terminal DataFrame.}
}
\value{
Updated DataFrame with refreshed nesting columns.
}
\description{
Re-walks terminals and refreshes `brace_depth`, `paren_depth`, `pab`,
and `nesting_level` columns. Call after brace insertion, token removal,
or any structural transform.
}
\keyword{internal}
