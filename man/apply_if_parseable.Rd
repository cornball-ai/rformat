% tinyrox says don't edit this manually, but it can't stop you!
\name{apply_if_parseable}
\alias{apply_if_parseable}
\title{Apply Transform Only If Output Parses}
\usage{
apply_if_parseable(code, fn, ...)
}
\arguments{
\item{code}{Code string.}

\item{fn}{Transform function taking `code` as first argument.}

\item{...}{Additional arguments passed to `fn`.}
}
\value{
Transformed code if parseable, otherwise original code.
}
\description{
Apply Transform Only If Output Parses
}
\keyword{internal}
