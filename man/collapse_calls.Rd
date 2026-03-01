% tinyrox says don't edit this manually, but it can't stop you!
\name{collapse_calls}
\alias{collapse_calls}
\title{Collapse Multi-Line Calls (AST Version)}
\usage{
collapse_calls(terms, indent_str, line_limit = 80L)
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
Finds multi-line parenthesized groups (function calls, control flow
conditions) that would fit on one line and collapses them by setting
all tokens' `out_line` to the opening line.
}
\keyword{internal}
