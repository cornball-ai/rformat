% tinyrox says don't edit this manually, but it can't stop you!
\name{rformat_file}
\alias{rformat_file}
\title{Format R File}
\usage{
rformat_file(path, output = NULL, dry_run = FALSE, indent = 4L,
             line_limit = 80L, wrap = "paren", brace_style = "kr",
             control_braces = FALSE, expand_if = FALSE, else_same_line = TRUE,
             function_space = FALSE)
}
\arguments{
\item{path}{Path to R file.}

\item{output}{Optional output path. If NULL, overwrites input file.}

\item{dry_run}{If TRUE, return formatted code without writing.}

\item{indent}{Indentation per level: integer for spaces (default 4), or
character string for literal indent (e.g., `"\\t\\t"` for vintage R Core style).}

\item{line_limit}{Maximum line length before wrapping (default 80).}

\item{wrap}{Continuation style for long function signatures: `"paren"`
(default) aligns to opening parenthesis, `"fixed"` uses 8-space indent.}

\item{brace_style}{Brace placement for function definitions: `"kr"` (default)
puts opening brace on same line as `) \{`, `"allman"` puts it on a new line.}

\item{control_braces}{If TRUE, add braces to bare one-line control flow
bodies. Default FALSE matches R Core majority style.}

\item{expand_if}{Expand inline if-else to multi-line (default FALSE).}

\item{else_same_line}{If TRUE (default), fix `\}\nelse` to `\} else \{`.}

\item{function_space}{If TRUE, add space before `(` in function definitions:
`function (x)` instead of `function(x)`. Default FALSE matches 96\% of
R Core source code.}
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
