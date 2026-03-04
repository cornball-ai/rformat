#' Join Else to Preceding Close Brace
#'
#' AST transform that moves ELSE tokens (and any following tokens on the
#' same line, like `if` in `else if`) to the same output line as the
#' preceding `}`. Skips if a COMMENT exists between `}` and `else`,
#' or if joining would exceed the line limit.
#'
#' @param terms Enriched terminal DataFrame.
#' @param indent_str Indent string for line width calculation.
#' @param line_limit Maximum line width.
#' @return Updated DataFrame.
#' @keywords internal
join_else_transform <- function(terms, indent_str, line_limit) {
    terms <- terms[order(terms$out_line, terms$out_order),]
    terms$out_order <- seq_len(nrow(terms))
    n <- nrow(terms)
    if (n < 2L) return(terms)

    # Find all ELSE tokens
    else_idx <- which(terms$token == "ELSE")
    if (length(else_idx) == 0L) return(terms)

    for (ei in else_idx) {
        else_line <- terms$out_line[ei]

        # Walk backwards to find preceding non-comment token
        rbrace_idx <- NA_integer_
        has_comment <- FALSE
        j <- ei - 1L
        while (j >= 1L) {
            if (terms$token[j] == "COMMENT") {
                has_comment <- TRUE
                j <- j - 1L
                next
            }
            if (terms$token[j] == "'}'") {
                rbrace_idx <- j
            }
            break
        }

        # Skip if no preceding }, or comment between } and else
        if (is.na(rbrace_idx) || has_comment) next

        rbrace_line <- terms$out_line[rbrace_idx]
        if (rbrace_line == else_line) next  # already on same line

        # Check if joining would exceed line_limit
        rbrace_width <- ast_line_width(terms, rbrace_line, indent_str)
        else_width <- ast_line_width(terms, else_line, indent_str)
        if (rbrace_width + 1L + else_width > line_limit) next

        # Move ELSE and all following tokens on the same line to the } line
        # This handles "else if (y) {" — moving else, if, (, y, ), { together
        k <- ei
        while (k <= n && terms$out_line[k] == else_line) {
            terms$out_line[k] <- rbrace_line
            k <- k + 1L
        }
    }

    terms
}
