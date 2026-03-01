% tinyrox says don't edit this manually, but it can't stop you!
\name{wrap_long_operators}
\alias{wrap_long_operators}
\title{Wrap Long Lines at Operators (AST Version)}
\usage{
wrap_long_operators(terms, indent_str, line_limit = 80L)
}
\arguments{
\item{terms}{Enriched terminal DataFrame.}

\item{indent_str}{Indent string.}

\item{line_limit}{Maximum line length.}
}
\value{
Updated DataFrame.
}
\description{
Finds overlong lines and breaks them after logical operators (`||`, `&&`,
`|`, `&`). Continuation lines get depth-based indentation.
}
\keyword{internal}
