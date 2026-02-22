#' Wrap Long Function Calls
#'
#' Wraps function call lines that exceed the line limit by breaking at commas.
#' Continuation lines are aligned to the opening parenthesis.
#'
#' @param code Formatted code string.
#' @param line_limit Maximum line length (default 80).
#' @return Code with wrapped long calls.
#' @keywords internal
wrap_long_calls <- function (code, line_limit = 80L) {
    changed <- TRUE
    max_iterations <- iteration_budget(code, 100L, mode = "wrap")

    while (changed && max_iterations > 0) {
        max_iterations <- max_iterations - 1
        changed <- FALSE

        result <- wrap_one_long_call(code, line_limit = line_limit)
        if (!is.null(result)) {
            code <- result
            changed <- TRUE
        }
    }

    code
}

#' Wrap One Long Function Call
#'
#' Finds the first single-line function call that exceeds the line limit
#' and wraps it at commas with paren-aligned continuation.
#'
#' @param code Code string.
#' @param line_limit Maximum line length (default 80).
#' @return Modified code or NULL if no changes.
#' @keywords internal
wrap_one_long_call <- function (code, line_limit = 80L) {
    parsed <- tryCatch(parse(text = code, keep.source = TRUE),
                       error = function (e) NULL)

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

    # Find function calls on lines that exceed the limit
    call_indices <- which(terminals$token == "SYMBOL_FUNCTION_CALL")

    for (ci in call_indices) {
        open_idx <- ci + 1
        if (open_idx > nrow(terminals)) { next }
        if (terminals$token[open_idx] != "'('") { next }

        call_line <- terminals$line1[ci]

        # Only consider single-line calls on lines that are too long
        if (display_width(lines[call_line]) <= line_limit) { next }

        # Skip lines with semicolons (multiple statements on one line)
        line_toks <- terminals[terminals$line1 == call_line,]
        if (any(line_toks$token == "';'")) { next }

        # Skip calls nested inside another wrappable call's parens on the
        # same line (wrap the outermost call, not inner ones). But allow
        # wrapping when the outer parens are just operators like !is.null().
        func_col <- terminals$col1[ci]
        before <- line_toks[line_toks$col1 < func_col,]
        paren_depth_before <- sum(before$token == "'('") -
        sum(before$token == "')'")
        if (paren_depth_before > 0) {
            # Check if any outer single-line call on this line has a
            # comma at depth 1 (i.e., it's a wrappable multi-arg call
            # that hasn't been wrapped yet). If so, skip this inner
            # call — the outer one should be wrapped first.
            outer_has_comma <- FALSE
            outer_calls <- which(line_toks$token == "SYMBOL_FUNCTION_CALL" &
                line_toks$col1 < func_col)
            for (oc in outer_calls) {
                # Find this outer call's open paren in the full terminals
                oc_global <- which(terminals$line1 == call_line &
                    terminals$col1 == line_toks$col1[oc] &
                    terminals$token == "SYMBOL_FUNCTION_CALL")
                if (length(oc_global) == 0) { next }
                oc_open <- oc_global[1] + 1
                if (oc_open > nrow(terminals)) { next }
                if (terminals$token[oc_open] != "'('") { next }
                # Find matching close paren
                od <- 1
                oc_close <- oc_open + 1
                while (oc_close <= nrow(terminals) && od > 0) {
                    if (terminals$token[oc_close] == "'('") { od <- od + 1 }
                    if (terminals$token[oc_close] == "')'") { od <- od - 1 }
                    if (od > 0) { oc_close <- oc_close + 1 }
                }
                # Only skip if the outer call is single-line (not yet wrapped)
                if (oc_close > nrow(terminals)) { next }
                if (terminals$line1[oc_close] != call_line) { next }
                oc_depth <- 0
                for (ot in seq(oc + 1, nrow(line_toks))) {
                    otok <- line_toks$token[ot]
                    if (otok == "'('") { oc_depth <- oc_depth + 1 }
                    if (otok == "')'") { oc_depth <- oc_depth - 1 }
                    if (oc_depth <= 0) { break }
                    if (otok == "','" && oc_depth == 1) {
                        outer_has_comma <- TRUE
                        break
                    }
                }
                if (outer_has_comma) { break }
            }
            if (outer_has_comma) { next }
        }

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

        # Only wrap single-line calls
        if (terminals$line1[close_idx] != call_line) { next }

        # Skip calls with embedded braces (multi-line function bodies, etc.)
        call_tokens <- terminals[seq(open_idx, close_idx),]
        if (any(call_tokens$token %in% c("'{'", "'}'"))) {
            next
        }

        # Need at least one comma at depth 1 to have something to wrap
        has_comma <- FALSE
        for (k in seq(open_idx + 1, close_idx - 1)) {
            if (terminals$token[k] == "','" &&
                terminals$line1[k] == call_line) {
                # Check this comma is at depth 1 (not inside nested parens/brackets)
                depth <- 0
                for (m in seq(open_idx + 1, k)) {
                    if (terminals$token[m] == "'('") { depth <- depth + 1 }
                    if (terminals$token[m] == "')'") { depth <- depth - 1 }
                    if (terminals$token[m] == "'['") { depth <- depth + 1 }
                    if (terminals$token[m] == "']'") { depth <- depth - 1 }
                    if (terminals$token[m] == "LBB") { depth <- depth + 2 }
                    if (terminals$token[m] == "']]'") { depth <- depth - 2 }
                }
                if (depth == 0) {
                    has_comma <- TRUE
                    break
                }
            }
        }
        if (!has_comma) { next }

        # Collect arguments as groups of tokens between commas at depth 1
        args <- list()
        current_arg_tokens <- list()
        depth <- 0

        for (k in seq(open_idx + 1, close_idx - 1)) {
            tok <- terminals[k,]
            if (tok$token == "'('") { depth <- depth + 1 }
            if (tok$token == "')'") { depth <- depth - 1 }
            if (tok$token == "'['") { depth <- depth + 1 }
            if (tok$token == "']'") { depth <- depth - 1 }
            if (tok$token == "LBB") { depth <- depth + 2 }
            if (tok$token == "']]'") { depth <- depth - 2 }

            if (tok$token == "','" && depth == 0) {
                if (length(current_arg_tokens) > 0) {
                    arg_df <- do.call(rbind,
                                      lapply(current_arg_tokens, as.data.frame))
                    args[[length(args) + 1]] <- format_line_tokens(arg_df)
                }
                current_arg_tokens <- list()
            } else {
                current_arg_tokens[[length(current_arg_tokens) + 1]] <- tok
            }
        }
        # Last argument
        if (length(current_arg_tokens) > 0) {
            arg_df <- do.call(rbind, lapply(current_arg_tokens, as.data.frame))
            args[[length(args) + 1]] <- format_line_tokens(arg_df)
        }

        if (length(args) < 2) { next }

        # Get prefix (everything before the function name)
        # Use col_to_charpos to handle tab-expanded columns from getParseData
        func_col <- terminals$col1[ci]
        line_content <- lines[call_line]
        if (func_col > 1) {
            char_pos <- col_to_charpos(line_content, func_col - 1)
            prefix <- substring(line_content, 1, char_pos)
        } else {
            prefix <- ""
        }

        func_name <- terminals$text[ci]

        # Continuation indent: align to opening paren column
        cont_indent <- strrep(" ", nchar(prefix) + nchar(func_name) + 1)

        # Get suffix (everything after closing paren on the same line)
        close_col <- terminals$col2[close_idx]
        char_pos_close <- col_to_charpos(line_content, close_col)
        if (char_pos_close < nchar(line_content)) {
            suffix <- substring(line_content, char_pos_close + 1)
        } else {
            suffix <- ""
        }

        # Build wrapped lines
        new_lines <- character(0)
        current_line <- paste0(prefix, func_name, "(")

        for (j in seq_along(args)) {
            if (j < length(args)) {
                arg_text <- paste0(args[[j]], ", ")
            } else {
                arg_text <- paste0(args[[j]], ")", suffix)
            }

            test_line <- paste0(current_line, arg_text)
            if (nchar(test_line) > line_limit &&
                nchar(current_line) > nchar(cont_indent)) {
                new_lines <- c(new_lines, sub(" $", "", current_line))
                current_line <- paste0(cont_indent, arg_text)
            } else {
                current_line <- test_line
            }
        }
        new_lines <- c(new_lines, sub(" $", "", current_line))

        # Only wrap if we actually split into multiple lines
        if (length(new_lines) < 2) { next }

        # If paren alignment pushed continuation lines over the limit,
        # fall back to depth-based indent (base_indent + one level)
        if (any(nchar(new_lines) > line_limit)) {
            base_indent <- sub("^(\\s*).*", "\\1", lines[call_line])
            cont_indent <- paste0(base_indent, "    ")
            new_lines <- character(0)
            current_line <- paste0(prefix, func_name, "(")
            for (j in seq_along(args)) {
                if (j < length(args)) {
                    arg_text <- paste0(args[[j]], ", ")
                } else {
                    arg_text <- paste0(args[[j]], ")", suffix)
                }
                test_line <- paste0(current_line, arg_text)
                if (nchar(test_line) > line_limit &&
                    nchar(current_line) > nchar(cont_indent)) {
                    new_lines <- c(new_lines, sub(" $", "", current_line))
                    current_line <- paste0(cont_indent, arg_text)
                } else {
                    current_line <- test_line
                }
            }
            new_lines <- c(new_lines, sub(" $", "", current_line))
            if (length(new_lines) < 2) { next }
        }

        # Replace the line
        if (call_line > 1) {
            pre <- lines[seq_len(call_line - 1)]
        } else {
            pre <- character(0)
        }
        if (call_line < length(lines)) {
            post <- lines[seq(call_line + 1, length(lines))]
        } else {
            post <- character(0)
        }
        new_code_lines <- c(pre, new_lines, post)

        result <- paste(new_code_lines, collapse = "\n")
        if (!grepl("\n$", result) && nchar(result) > 0) {
            result <- paste0(result, "\n")
        }

        return(result)
    }

    # wrap_one_long_call: no changes
    NULL
}
#' Wrap Long Lines at Operators
#'
#' Wraps lines exceeding the line limit at logical operators (`||`, `&&`)
#' with continuation aligned to the first operand.
#'
#' @param code Formatted code string.
#' @param line_limit Maximum line length (default 80).
#' @return Code with wrapped long lines.
#' @keywords internal
wrap_long_operators <- function (code, line_limit = 80L) {
    changed <- TRUE
    max_iterations <- iteration_budget(code, 100L, mode = "wrap")

    while (changed && max_iterations > 0) {
        max_iterations <- max_iterations - 1
        changed <- FALSE

        result <- wrap_one_long_operator(code, line_limit = line_limit)
        if (!is.null(result)) {
            code <- result
            changed <- TRUE
        }
    }

    code
}

#' Wrap One Long Line at an Operator
#'
#' Finds the first line exceeding the limit that contains a top-level
#' logical operator and wraps there. The operator stays at the end of the
#' first line, continuation is aligned to the first operand.
#'
#' @param code Code string.
#' @param line_limit Maximum line length (default 80).
#' @return Modified code or NULL if no changes.
#' @keywords internal
wrap_one_long_operator <- function (code, line_limit = 80L) {
    parsed <- tryCatch(parse(text = code, keep.source = TRUE),
                       error = function (e) NULL)

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

    # Operators to break at, in priority order
    break_ops <- c("OR2", "AND2", "OR", "AND")

    for (line_num in seq_along(lines)) {
        if (display_width(lines[line_num]) <= line_limit) { next }

        line_toks <- terminals[terminals$line1 == line_num,]
        if (nrow(line_toks) == 0) { next }

        # Skip lines with semicolons (multiple statements on one line)
        if (any(line_toks$token == "';'")) { next }

        # Calculate paren depth at the start of this line
        start_depth <- 0
        before_toks <- terminals[terminals$line1 < line_num,]
        for (j in seq_len(nrow(before_toks))) {
            if (before_toks$token[j] == "'('") {
                start_depth <- start_depth + 1
            }
            if (before_toks$token[j] == "')'") {
                start_depth <- start_depth - 1
            }
        }

        # Find the best operator to break at
        best_break <- NULL
        depth <- start_depth

        for (j in seq_len(nrow(line_toks))) {
            tok <- line_toks[j,]
            if (tok$token == "'('") { depth <- depth + 1 }
            if (tok$token == "')'") { depth <- depth - 1 }

            # Break at operators inside the immediate context (not deeply nested)
            if (tok$token %in% break_ops && depth <= start_depth + 1) {
                if (tok$col2 <= line_limit) {
                    best_break <- j
                }
            }
        }

        if (is.null(best_break)) { next }

        break_tok <- line_toks[best_break,]

        # First part: up to and including the operator
        first_part <- substring(lines[line_num], 1,
                                col_to_charpos(lines[line_num], break_tok$col2))

        # Continuation indent: depth-based to match format_tokens
        # (base_indent already reflects brace/paren depth from format_tokens;
        # add one indent level per unclosed bracket on this line before the break)
        base_indent <- sub("^(\\s*).*", "\\1", lines[line_num])

        bracket_depth <- 0
        for (j in seq_len(nrow(line_toks))) {
            tok_j <- line_toks[j,]
            if (tok_j$col1 > break_tok$col1) { break }
            if (tok_j$token %in% c("'('", "'['")) {
                bracket_depth <- bracket_depth + 1
            } else if (tok_j$token == "LBB") {
                bracket_depth <- bracket_depth + 2
            } else if (tok_j$token %in% c("')'", "']'")) {
                bracket_depth <- bracket_depth - 1
            } else if (tok_j$token == "']]'") {
                bracket_depth <- bracket_depth - 2
            }
        }

        cont_indent <- paste0(base_indent,
                              strrep("    ", max(0, bracket_depth)))

        # Second part: rest of line, trimmed
        rest <- substring(lines[line_num],
                          col_to_charpos(lines[line_num], break_tok$col2) + 1)
        rest <- sub("^\\s+", "", rest)

        new_lines <- c(first_part, paste0(cont_indent, rest))

        pre <- if (line_num > 1) lines[seq_len(line_num - 1)] else character(0)
        if (line_num < length(lines)) {
            post <- lines[seq(line_num + 1, length(lines))]
        } else {
            post <- character(0)
        }
        new_code_lines <- c(pre, new_lines, post)

        result <- paste(new_code_lines, collapse = "\n")
        if (!grepl("\n$", result) && nchar(result) > 0) {
            result <- paste0(result, "\n")
        }

        return(result)
    }

    NULL
}

