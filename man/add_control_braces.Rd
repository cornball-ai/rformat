% tinyrox says don't edit this manually, but it can't stop you!
\name{add_control_braces}
\alias{add_control_braces}
\title{Add Control Braces (AST Version)}
\usage{
add_control_braces(terms, mode = "single", indent_str = "    ", line_limit = 80L)
}
\arguments{
\item{terms}{Enriched terminal DataFrame.}

\item{mode}{Control brace mode.}

\item{indent_str}{Indent string (for line width calculations).}

\item{line_limit}{Maximum line width.}
}
\value{
Updated DataFrame.
}
\description{
Finds bare control flow bodies (if/for/while/repeat without braces)
and transforms them according to the specified mode.
Modes:
- `TRUE` / `"single"`: Add braces, keep on one line if short enough.
- `"multi"`: Add braces, force multi-line.
- `"next_line"`: Move same-line body to next line (no braces).
- `"same_line"`: Move next-line body to same line; strip single-stmt braces.
}
\keyword{internal}
