% tinyrox says don't edit this manually, but it can't stop you!
\name{format_pipeline}
\alias{format_pipeline}
\title{Apply Formatting Pipeline to a Code Fragment}
\usage{
format_pipeline(code, indent, wrap, expand_if, brace_style, line_limit)
}
\arguments{
\item{code}{Character string of R code.}

\item{indent}{Indent size or string.}

\item{wrap}{Continuation style.}

\item{expand_if}{Expand inline if-else.}

\item{brace_style}{Brace placement style.}

\item{line_limit}{Maximum line length.}
}
\value{
Formatted code string.
}
\description{
Runs collapse, brace, wrap, function-def, and inline-if passes on a
single code string. Designed to operate on individual top-level
expressions so each gets the full iteration budget.
}
\keyword{internal}
