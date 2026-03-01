% tinyrox says don't edit this manually, but it can't stop you!
\name{serialize_tokens}
\alias{serialize_tokens}
\title{Serialize Enriched Tokens to Formatted Code}
\usage{
serialize_tokens(terms, indent_str, wrap = "paren", line_limit = 80L)
}
\arguments{
\item{terms}{Enriched terminal DataFrame.}

\item{indent_str}{Indent string (e.g., `"    "` for 4 spaces).}

\item{wrap}{Continuation style: `"paren"` or `"fixed"`.}

\item{line_limit}{Maximum line length.}
}
\value{
Formatted code string.
}
\description{
Converts the enriched terminal DataFrame to a formatted code string.
This is the final step: tokens are emitted in `(out_line, out_order)`
order with proper indentation and spacing.
}
\keyword{internal}
