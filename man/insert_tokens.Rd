% tinyrox says don't edit this manually, but it can't stop you!
\name{insert_tokens}
\alias{insert_tokens}
\title{Insert Synthetic Tokens into the DataFrame}
\usage{
insert_tokens(terms, new_rows)
}
\arguments{
\item{terms}{Enriched terminal DataFrame.}

\item{new_rows}{Data frame of new tokens to insert. Must have at minimum:
`token`, `out_text`, `out_line`, `out_order`. Other columns will be
filled with defaults.}
}
\value{
Updated DataFrame with new rows appended.
}
\description{
Adds new token rows (e.g., for brace insertion). New tokens get unique IDs
starting from `max(existing_id) + 1`.
}
\keyword{internal}
