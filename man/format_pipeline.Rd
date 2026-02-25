% tinyrox says don't edit this manually, but it can't stop you!
\name{format_pipeline}
\alias{format_pipeline}
\title{Apply Formatting Pipeline to a Code Fragment}
\usage{
format_pipeline(
  code,
  indent,
  wrap,
  expand_if,
  brace_style,
  line_limit,
  function_space = FALSE,
  control_braces = FALSE
)
}
\arguments{
\item{code}{Character string of R code.}

\item{indent}{Indent size or string.}

\item{wrap}{Continuation style.}

\item{expand_if}{Expand inline if-else.}

\item{brace_style}{Brace placement style.}

\item{line_limit}{Maximum line length.}

\item{function_space}{If TRUE, add space before `(` in function definitions.}

\item{control_braces}{If TRUE, add braces to bare one-line control flow bodies.}
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
