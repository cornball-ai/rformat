% tinyrox says don't edit this manually, but it can't stop you!
\name{col_to_charpos}
\alias{col_to_charpos}
\title{Convert Tab-Expanded Column to Character Position}
\usage{
col_to_charpos(line, col)
}
\arguments{
\item{line}{A single line of text.}

\item{col}{Tab-expanded column position (1-based).}
}
\value{
Character position (1-based) in the string.
}
\description{
R's getParseData() reports columns with tabs expanded to 8-column tab stops.
This function converts such a column back to a character position for use
with substring().
}
\keyword{internal}
