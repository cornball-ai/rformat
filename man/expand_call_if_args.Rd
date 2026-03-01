% tinyrox says don't edit this manually, but it can't stop you!
\name{expand_call_if_args}
\alias{expand_call_if_args}
\title{Expand Bare If-Else in Function Call Arguments (AST Version)}
\usage{
expand_call_if_args(terms, indent_str = "    ", line_limit = 80L)
}
\arguments{
\item{terms}{Enriched terminal DataFrame.}

\item{indent_str}{Indent string.}

\item{line_limit}{Maximum line width.}
}
\value{
Updated DataFrame.
}
\description{
Finds bare `if (cond) expr else expr` arguments inside function calls
on overlong lines and expands them to braced multi-line form.
}
\keyword{internal}
