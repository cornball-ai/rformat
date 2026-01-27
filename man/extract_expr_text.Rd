% tinyrox says don't edit this manually, but it can't stop you!
\name{extract_expr_text}
\alias{extract_expr_text}
\title{Extract Expression Text from Source Lines}
\usage{
extract_expr_text(lines, tokens, target_indent)
}
\arguments{
\item{lines}{Source code lines.}

\item{tokens}{Token data frame for the expression.}

\item{target_indent}{Target indentation string for continuation lines.}
}
\value{
Expression text with first line unindented, continuation lines re-indented.
}
\description{
Extract original text for a multi-line expression and re-indent it.
}
\keyword{internal}
