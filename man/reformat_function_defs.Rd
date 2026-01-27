% tinyrox says don't edit this manually, but it can't stop you!
\name{reformat_function_defs}
\alias{reformat_function_defs}
\title{Reformat Function Definitions}
\usage{
reformat_function_defs(code, wrap = "paren", brace_style = "kr")
}
\arguments{
\item{code}{Formatted code string.}

\item{wrap}{Continuation style: `"paren"` or `"fixed"`.}

\item{brace_style}{Brace placement: `"kr"` (same line) or `"allman"` (new line).}
}
\value{
Code with reformatted function definitions.
}
\description{
Ensures function definitions follow style guide:
- Short signatures on one line
- Long signatures wrap with continuation indent
- Brace on same line (K&R) or own line (Allman)
}
\keyword{internal}
