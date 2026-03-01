% tinyrox says don't edit this manually, but it can't stop you!
\name{reformat_function_defs}
\alias{reformat_function_defs}
\title{Reformat Function Definitions (AST Version)}
\usage{
reformat_function_defs(terms, indent_str = "    ", wrap = "paren",
                       brace_style = "kr", line_limit = 80L,
                       function_space = FALSE)
}
\arguments{
\item{terms}{Enriched terminal DataFrame.}

\item{indent_str}{Indent string (e.g., `"    "`).}

\item{wrap}{Continuation style: `"paren"` or `"fixed"`.}

\item{brace_style}{`"kr"` or `"allman"`.}

\item{line_limit}{Maximum line length.}

\item{function_space}{Whether to add space after `function`.}
}
\value{
Updated DataFrame.
}
\description{
Rewrites named function signatures to fit within the line limit.
Short signatures go on one line; long ones wrap at commas with
paren-aligned or fixed continuation indent. Operates on the
DataFrame directly, avoiding the serialize/re-parse cycle that
caused idempotency oscillation.
}
\keyword{internal}
