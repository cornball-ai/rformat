#' Format R Code
#'
#' Format R code string according to base R style conventions.
#'
#' @param code Character string of R code to format.
#' @param indent Indentation per level: integer for spaces (default 4), or
#'   character string for literal indent (e.g., `"\\t\\t"` for vintage R Core style).
#' @param wrap Continuation style for long function signatures: `"paren"`
#'   (default) aligns to opening parenthesis, `"fixed"` uses 8-space indent.
#' @param expand_if Expand inline if-else to multi-line (default FALSE).
#' @param brace_style Brace placement for function definitions: `"kr"` (default)
#'   puts opening brace on same line as `) {`, `"allman"` puts it on a new line.
#' @param line_limit Maximum line length before wrapping (default 80).
#' @param function_space If TRUE, add space before `(` in function definitions:
#'   `function (x)` instead of `function(x)`. Default FALSE matches 96% of
#'   R Core source code.
#' @param control_braces If TRUE, add braces to bare one-line control flow
#'   bodies (e.g., `if (x) y` becomes `if (x) { y }`). Default FALSE
#'   matches R Core source code where 59% of control flow bodies are bare.
#' @param else_same_line If TRUE (default), fix `}\nelse` to `} else {` on
#'   the same line. Matches R Core source code where 70% use same-line else.
#' @return Formatted code as a character string.
#' @export
#' @examples
#' rformat("x<-1+2")
#' rformat("if(x>0){y<-1}")
#' # Expand inline if-else
#' rformat("x <- if (a) b else c", expand_if = TRUE)
#' # Allman brace style (legacy)
#' rformat("f <- function(x) { x }", brace_style = "allman")
rformat <- function (code, indent = 4L, wrap = "paren", expand_if = FALSE,
                     brace_style = "kr", line_limit = 80L,
                     function_space = FALSE, control_braces = FALSE,
                     else_same_line = TRUE) {
    if (!is.character(code)) {
        stop("`code` must be a character string")
    }

    # Only normalize `} else` when needed for parsing. Applying this
    # unconditionally can destabilize already-valid nested if/else layouts.
    parsed_ok <- !is.null(tryCatch(parse(text = code, keep.source = TRUE),
                                   error = function(e) NULL))
    if (!parsed_ok && else_same_line) {
        code <- fix_else_placement(code)
    }

    formatted <- format_tokens(code, indent = indent, wrap = wrap,
                               expand_if = expand_if,
                               brace_style = brace_style,
                               line_limit = line_limit,
                               function_space = function_space,
                               control_braces = control_braces)

    format_blank_lines(formatted)
}

#' Format R File
#'
#' Format an R file in place or write to a new file.
#'
#' @param path Path to R file.
#' @param output Optional output path. If NULL, overwrites input file.
#' @param dry_run If TRUE, return formatted code without writing.
#' @param indent Indentation per level: integer for spaces (default 4), or
#'   character string for literal indent (e.g., `"\\t\\t"` for vintage R Core style).
#' @param wrap Continuation style for long function signatures: `"paren"`
#'   (default) aligns to opening parenthesis, `"fixed"` uses 8-space indent.
#' @param expand_if Expand inline if-else to multi-line (default FALSE).
#' @param brace_style Brace placement for function definitions: `"kr"` (default)
#'   puts opening brace on same line as `) {`, `"allman"` puts it on a new line.
#' @param line_limit Maximum line length before wrapping (default 80).
#' @param function_space If TRUE, add space before `(` in function definitions:
#'   `function (x)` instead of `function(x)`. Default FALSE matches 96% of
#'   R Core source code.
#' @param control_braces If TRUE, add braces to bare one-line control flow
#'   bodies. Default FALSE matches R Core majority style.
#' @param else_same_line If TRUE (default), fix `}\nelse` to `} else {`.
#' @return Invisibly returns formatted code.
#' @export
#' @examples
#' # Format a file (dry run to see result without writing)
#' f <- tempfile(fileext = ".R")
#' writeLines("x<-1+2", f)
#' rformat_file(f, dry_run = TRUE)
#'
#' # Format and overwrite
#' rformat_file(f)
#' readLines(f)
#' unlink(f)
rformat_file <- function (path, output = NULL, dry_run = FALSE, indent = 4L,
                          wrap = "paren", expand_if = FALSE,
                          brace_style = "kr", line_limit = 80L,
                          function_space = FALSE, control_braces = FALSE,
                          else_same_line = TRUE) {
    if (!file.exists(path)) {
        stop("File not found: ", path)
    }

    code <- paste(readLines(path, warn = FALSE), collapse = "\n")
    formatted <- rformat(code, indent = indent, wrap = wrap,
                         expand_if = expand_if, brace_style = brace_style,
                         line_limit = line_limit,
                         function_space = function_space,
                         control_braces = control_braces,
                         else_same_line = else_same_line)

    if (!dry_run) {
        if (is.null(output)) {
            out_path <- path
        } else {
            out_path <- output
        }
        writeLines(formatted, out_path)
    }

    invisible(formatted)
}

#' Format R Files in Directory
#'
#' Format all R files in a directory.
#'
#' @param path Path to directory.
#' @param recursive If TRUE, process subdirectories.
#' @param dry_run If TRUE, report changes without writing.
#' @param indent Indentation per level: integer for spaces (default 4), or
#'   character string for literal indent (e.g., `"\\t\\t"` for vintage R Core style).
#' @param wrap Continuation style for long function signatures: `"paren"`
#'   (default) aligns to opening parenthesis, `"fixed"` uses 8-space indent.
#' @param expand_if Expand inline if-else to multi-line (default FALSE).
#' @param brace_style Brace placement for function definitions: `"kr"` (default)
#'   puts opening brace on same line as `) {`, `"allman"` puts it on a new line.
#' @param line_limit Maximum line length before wrapping (default 80).
#' @param function_space If TRUE, add space before `(` in function definitions:
#'   `function (x)` instead of `function(x)`. Default FALSE matches 96% of
#'   R Core source code.
#' @param control_braces If TRUE, add braces to bare one-line control flow
#'   bodies. Default FALSE matches R Core majority style.
#' @param else_same_line If TRUE (default), fix `}\nelse` to `} else {`.
#' @return Invisibly returns vector of modified file paths.
#' @export
#' @examples
#' # Format all R files in a directory (dry run)
#' d <- tempfile()
#' dir.create(d)
#' writeLines("x<-1", file.path(d, "test.R"))
#' rformat_dir(d, dry_run = TRUE)
#'
#' # Format and overwrite
#' rformat_dir(d)
#' unlink(d, recursive = TRUE)
rformat_dir <- function (path = ".", recursive = TRUE, dry_run = FALSE,
                         indent = 4L, wrap = "paren", expand_if = FALSE,
                         brace_style = "kr", line_limit = 80L,
                         function_space = FALSE, control_braces = FALSE,
                         else_same_line = TRUE) {
    if (!dir.exists(path)) {
        stop("Directory not found: ", path)
    }

    files <- list.files(path, pattern = "\\.[Rr]$", full.names = TRUE,
                        recursive = recursive)

    modified <- character(0)

    for (f in files) {
        original <- paste(readLines(f, warn = FALSE), collapse = "\n")
        formatted <- rformat(original, indent = indent, wrap = wrap,
                             expand_if = expand_if, brace_style = brace_style,
                             line_limit = line_limit,
                             function_space = function_space,
                             control_braces = control_braces,
                             else_same_line = else_same_line)

        if (formatted != original) {
            modified <- c(modified, f)
            if (!dry_run) {
                writeLines(formatted, f)
                message("Formatted: ", f)
            } else {
                message("Would format: ", f)
            }
        }
    }

    invisible(modified)
}

