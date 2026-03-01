% tinyrox says don't edit this manually, but it can't stop you!
\name{wrap_long_calls}
\alias{wrap_long_calls}
\title{Wrap Long Function Calls at Commas (AST Version)}
\usage{
wrap_long_calls(terms, indent_str, wrap = "paren", line_limit = 80L)
}
\arguments{
\item{terms}{Enriched terminal DataFrame.}

\item{indent_str}{Indent string.}

\item{wrap}{Continuation style: `"paren"` or `"fixed"`.}

\item{line_limit}{Maximum line length.}
}
\value{
Updated DataFrame.
}
\description{
Finds single-line function calls on overlong lines and wraps them at
commas. Continuation lines get depth-based indentation (or paren-aligned
if `wrap = "paren"`).
}
\keyword{internal}
