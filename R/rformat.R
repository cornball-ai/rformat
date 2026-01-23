#' Format R Code
#'
#' Format R code string according to base R style conventions.
#'
#' @param code Character string of R code to format.
#' @param indent Number of spaces per indentation level (default 4, R Core style).
#' @return Formatted code as a character string.
#' @export
#' @examples
#' rformat("x<-1+2")
#' rformat("if(x>0){y<-1}")
rformat <- function(
  code,
  indent = 4L)
{
    if (!is.character(code)) {
        stop("`code` must be a character string")
    }

    formatted <- format_tokens(code, indent = indent)
    format_blank_lines(formatted)
}

#' Format R File
#'
#' Format an R file in place or write to a new file.
#'
#' @param path Path to R file.
#' @param output Optional output path. If NULL, overwrites input file.
#' @param dry_run If TRUE, return formatted code without writing.
#' @param indent Number of spaces per indentation level (default 4, R Core style).
#' @return Invisibly returns formatted code.
#' @export
rformat_file <- function(
  path,
  output = NULL,
  dry_run = FALSE,
  indent = 4L)
{
    if (!file.exists(path)) {
        stop("File not found: ", path)
    }

    code <- paste(readLines(path, warn = FALSE), collapse = "\n")
    formatted <- rformat(code, indent = indent)

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
#' @param indent Number of spaces per indentation level (default 4, R Core style).
#' @return Invisibly returns vector of modified file paths.
#' @export
rformat_dir <- function(
  path = ".",
  recursive = TRUE,
  dry_run = FALSE,
  indent = 4L)
{
    if (!dir.exists(path)) {
        stop("Directory not found: ", path)
    }

    files <- list.files(
        path,
        pattern = "\\.[Rr]$",
        full.names = TRUE,
        recursive = recursive
    )

    modified <- character(0)

    for (f in files) {
        original <- paste(readLines(f, warn = FALSE), collapse = "\n")
        formatted <- rformat(original, indent = indent)

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

