#' Collapse Multi-Line Function Calls
#'
#' Collapses function calls spanning multiple lines into a single line.
#' Long lines are re-wrapped by `wrap_long_calls()` afterward. For example:
#' \preformatted{c(x,
#'   y,
#'   z
#' )}
#' becomes `c(x, y, z)`.
#'
#' @param code Formatted code string.
#' @return Code with collapsed function calls.
#' @keywords internal
collapse_calls <- function (code) {
    changed <- TRUE
    max_iterations <- iteration_budget(code, 100L, mode = "collapse")

    while (changed && max_iterations > 0) {
        max_iterations <- max_iterations - 1
        changed <- FALSE

        result <- collapse_one_call(code)
        if (!is.null(result)) {
            code <- result
            changed <- TRUE
        }
    }

    code
}

#' Collapse One Multi-Line Function Call
#'
#' Finds the first multi-line function call that can fit on one line
#' and collapses it. Skips calls containing comments.
#'
#' @param code Code string.
#' @return Modified code or NULL if no changes.
#' @keywords internal
collapse_one_call <- function (code) {
    parsed <- tryCatch(
        parse(text = code, keep.source = TRUE),
        error = function (e) NULL
    )

    if (is.null(parsed)) {
        return(NULL)
    }

    pd <- getParseData(parsed)
    if (is.null(pd) || nrow(pd) == 0) {
        return(NULL)
    }

    terminals <- pd[pd$terminal,]
    terminals <- terminals[order(terminals$line1, terminals$col1),]

    lines <- strsplit(code, "\n", fixed = TRUE)[[1]]

    # Find multi-line parenthesized groups:
    # 1. Function calls: SYMBOL_FUNCTION_CALL followed by '('
    # 2. Control flow: IF/FOR/WHILE followed by '('
    target_tokens <- c("SYMBOL_FUNCTION_CALL", "IF", "FOR", "WHILE")
    target_indices <- which(terminals$token %in% target_tokens)

    for (ci in target_indices) {
        # Next token should be '('
        open_idx <- ci + 1
        if (open_idx > nrow(terminals)) { next }
        if (terminals$token[open_idx] != "'('") { next }

        open_line <- terminals$line1[open_idx]

        # Find matching ')'
        paren_depth <- 1
        close_idx <- open_idx + 1
        while (close_idx <= nrow(terminals) && paren_depth > 0) {
            if (terminals$token[close_idx] == "'('") {
                paren_depth <- paren_depth + 1
            } else if (terminals$token[close_idx] == "')'") {
                paren_depth <- paren_depth - 1
            }
            if (paren_depth > 0) { close_idx <- close_idx + 1 }
        }

        if (close_idx > nrow(terminals)) { next }

        close_line <- terminals$line1[close_idx]

        # Only process multi-line groups
        if (close_line == open_line) { next }

        # Skip if any token in the group is a comment
        inner_tokens <- terminals[seq(open_idx, close_idx),]
        if (any(inner_tokens$token == "COMMENT")) { next }

        # Skip if the group contains a FUNCTION definition or braces
        if (any(inner_tokens$token == "FUNCTION")) { next }
        if (any(inner_tokens$token == "'{'")) { next }

        # Build collapsed text from tokens
        call_tokens <- terminals[seq(ci, close_idx),]
        collapsed <- format_line_tokens(call_tokens)

        # Get prefix: everything before the function name on its line
        func_line <- terminals$line1[ci]
        func_col <- terminals$col1[ci]
        if (func_col > 1) {
            prefix <- substring(lines[func_line], 1,
                col_to_charpos(lines[func_line], func_col - 1))
        } else {
            prefix <- ""
        }

        full_line <- paste0(prefix, collapsed)

        # Check if there are tokens after the closing paren on its line
        # that need to be appended (e.g., trailing comma, closing paren of outer call)
        after_close <- terminals[terminals$line1 == close_line &
            terminals$col1 > terminals$col1[close_idx],]
        suffix <- ""
        if (nrow(after_close) > 0) {
            last_call_tok <- call_tokens[nrow(call_tokens),]
            prev_prev_tok <- NULL
            if (nrow(call_tokens) >= 2) {
                prev_prev_tok <- call_tokens[nrow(call_tokens) - 1,]
            }
            suffix <- format_line_tokens(after_close,
                prev_token = last_call_tok,
                prev_prev_token = prev_prev_tok)
        }

        full_line <- paste0(full_line, suffix)

        # Also check if there are tokens before the function call on func_line
        # that aren't part of the prefix (i.e., code tokens before the call)
        before_call <- terminals[terminals$line1 == func_line &
            terminals$col1 < func_col,]

        # Replace the lines
        if (func_line > 1) {
            pre <- lines[seq_len(func_line - 1)]
        } else {
            pre <- character(0)
        }
        if (close_line < length(lines)) {
            post <- lines[seq(close_line + 1, length(lines))]
        } else {
            post <- character(0)
        }
        new_code_lines <- c(pre, full_line, post)

        result <- paste(new_code_lines, collapse = "\n")
        if (!grepl("\n$", result) && nchar(result) > 0) {
            result <- paste0(result, "\n")
        }

        return(result)
    }

    NULL
}
