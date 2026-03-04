% tinyrox says don't edit this manually, but it can't stop you!
\name{join_else_transform}
\alias{join_else_transform}
\title{Join Else to Preceding Close Brace}
\usage{
join_else_transform(terms, indent_str, line_limit)
}
\arguments{
\item{terms}{Enriched terminal DataFrame.}

\item{indent_str}{Indent string for line width calculation.}

\item{line_limit}{Maximum line width.}
}
\value{
Updated DataFrame.
}
\description{
AST transform that moves ELSE tokens (and any following tokens on the
same line, like `if` in `else if`) to the same output line as the
preceding `\}`. Skips if a COMMENT exists between `\}` and `else`,
or if joining would exceed the line limit.
}
\keyword{internal}
