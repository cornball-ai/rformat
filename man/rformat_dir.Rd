% tinyrox says don't edit this manually, but it can't stop you!
\name{rformat_dir}
\alias{rformat_dir}
\title{Format R Files in Directory}
\usage{
rformat_dir(path = ".", recursive = TRUE, dry_run = FALSE, indent = 4L,
            line_limit = 80L, wrap = "paren", brace_style = "kr",
            control_braces = FALSE, expand_if = FALSE, else_same_line = TRUE,
            function_space = FALSE)
}
\arguments{
\item{path}{Path to directory.}

\item{recursive}{If TRUE, process subdirectories.}

\item{dry_run}{If TRUE, report changes without writing.}

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

\item{else_same_line}{If TRUE (default), fix `\}\\nelse` to `\} else \{`.}

\item{function_space}{If TRUE, add space before `(` in function definitions:
`function (x)` instead of `function(x)`. Default FALSE matches 96\% of
R Core source code.}
}
\value{
Invisibly returns vector of modified file paths.
}
\description{
Format all R files in a directory.
}
\examples{
# Format all R files in a directory (dry run)
d <- tempfile()
dir.create(d)
writeLines("x<-1", file.path(d, "test.R"))
rformat_dir(d, dry_run = TRUE)

# Format and overwrite
rformat_dir(d)
unlink(d, recursive = TRUE)
}
