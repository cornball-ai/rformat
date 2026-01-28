% tinyrox says don't edit this manually, but it can't stop you!
\name{rformat_file}
\alias{rformat_file}
\title{Format R File}
\usage{
rformat_file(
  path,
  output = NULL,
  dry_run = FALSE,
  indent = 4L,
  wrap = "paren",
  expand_if = FALSE,
  brace_style = "kr"
)
}
\arguments{
\item{path}{Path to R file.}

\item{output}{Optional output path. If NULL, overwrites input file.}

\item{dry_run}{If TRUE, return formatted code without writing.}

\item{indent}{Indentation per level: integer for spaces (default 4), or
character string for literal indent (e.g., `"\\t\\t"` for vintage R Core style).}

\item{wrap}{Continuation style for long function signatures: `"paren"`
(default) aligns to opening parenthesis, `"fixed"` uses 8-space indent.}

\item{expand_if}{Expand inline if-else to multi-line (default FALSE).}

\item{brace_style}{Brace placement for function definitions: `"kr"` (default)
puts opening brace on same line as `) \{`, `"allman"` puts it on a new line.}
}
\value{
Invisibly returns formatted code.
}
\description{
Format an R file in place or write to a new file.
}
\examples{
# Format a file (dry run to see result without writing)
f <- tempfile(fileext = ".R")
writeLines("x<-1+2", f)
rformat_file(f, dry_run = TRUE)

# Format and overwrite
rformat_file(f)
readLines(f)
unlink(f)
}
