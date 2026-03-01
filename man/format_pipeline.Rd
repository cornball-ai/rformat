% tinyrox says don't edit this manually, but it can't stop you!
\name{format_pipeline}
\alias{format_pipeline}
\title{AST-Based Format Pipeline}
\usage{
format_pipeline(code, indent, wrap, expand_if, brace_style, line_limit,
                function_space = FALSE, control_braces = FALSE)
}
\arguments{
\item{code}{Code string for one top-level expression.}

\item{indent}{Indent string or integer.}

\item{wrap}{Continuation style: `"paren"` or `"fixed"`.}

\item{expand_if}{Whether to expand all inline if-else.}

\item{brace_style}{`"kr"` or `"allman"`.}

\item{line_limit}{Maximum line length.}

\item{function_space}{Add space after `function`.}

\item{control_braces}{Control brace mode.}
}
\value{
Formatted code string.
}
\description{
Single-pass pipeline: parse once, enrich the terminal DataFrame, run
all transforms as DataFrame operations, serialize to text once.
}
\keyword{internal}
