% tinyrox says don't edit this manually, but it can't stop you!
\name{reformat_inline_if}
\alias{reformat_inline_if}
\title{Reformat Inline If-Else Assignments (AST Version)}
\usage{
reformat_inline_if(terms, indent_str = "    ", line_limit = 0L)
}
\arguments{
\item{terms}{Enriched terminal DataFrame.}

\item{indent_str}{Indent string.}

\item{line_limit}{Maximum line width. Use 0 to expand all.}
}
\value{
Updated DataFrame.
}
\description{
Finds `var <- if (cond) true_expr else false_expr` patterns and
expands them to braced multi-line form with duplicated assignment:
if (cond) \{
var <- true_expr
\} else \{
var <- false_expr
\}
}
\keyword{internal}
