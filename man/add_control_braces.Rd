% tinyrox says don't edit this manually, but it can't stop you!
\name{add_control_braces}
\alias{add_control_braces}
\title{Add Braces to One-Liner Control Flow}
\usage{
add_control_braces(code)
}
\arguments{
\item{code}{Code string.}
}
\value{
Code with braces added.
}
\description{
Adds braces to single-statement `if`, `for`, `while` bodies:
`if (x) y` becomes `if (x) \{ y \}`.
Does not modify `if`/`else` used as expressions (value-producing).
}
\keyword{internal}
