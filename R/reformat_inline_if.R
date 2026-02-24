reformat_inline_if <- function (code, line_limit = 0L) {
    changed <- TRUE
    max_iterations <- 100L

    while (changed && max_iterations > 0) {
        max_iterations <- max_iterations - 1
        changed <- FALSE

        result <- reformat_one_inline_if(code, line_limit = line_limit)
        if (!is.null(result)) {
            code <- result
            changed <- TRUE
        }
    }

    code
}

#' Reformat One Inline If-Else Statement
#'
#' @param code Code string.
#' @param line_limit Only expand if-else on lines exceeding this limit.
#'   Use 0 to expand all inline if-else.
#' @return Modified code or NULL if no changes.
#' @keywords internal
reformat_one_inline_if <- function (code, line_limit = 0L) {
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

    # Find assignment followed by if on the same line
    assign_tokens <- c("LEFT_ASSIGN", "EQ_ASSIGN")

    for (i in seq_len(nrow(terminals))) {
        tok <- terminals[i,]

        if (!(tok$token %in% assign_tokens)) { next }

        assign_line <- tok$line1

        # Find IF token after assignment on same line
        # Skip if there's a FUNCTION token between assignment and IF
        if_idx <- NULL
        found_function <- FALSE
        for (j in (i + 1):nrow(terminals)) {
            if (j > nrow(terminals)) { break }
            next_tok <- terminals[j,]
            if (next_tok$line1 != assign_line) { break }
            if (next_tok$token == "FUNCTION") {
                found_function <- TRUE
                break
            }
            if (next_tok$token == "IF") {
                if_idx <- j
                break
            }
        }

        if (found_function || is.null(if_idx)) { next }

        # Skip if the IF is inside unclosed parens relative to the assignment
        # e.g., x <- c(if (a) b else d, ...) should not be expanded
        paren_bal <- 0L
        for (j in (i + 1):(if_idx - 1)) {
            if (j > nrow(terminals)) { break }
            jt <- terminals$token[j]
            if (jt == "'('") { paren_bal <- paren_bal + 1L }
            if (jt == "')'") { paren_bal <- paren_bal - 1L }
        }
        if (paren_bal > 0L) { next }

        # Found pattern: assignment followed by if
        # Now find the structure: if ( cond ) true_expr else false_expr

        # Get the variable being assigned (tokens before assignment)
        var_tokens <- terminals[terminals$line1 == assign_line &
            seq_len(nrow(terminals)) < i,]
        if (nrow(var_tokens) == 0) { next }

        var_name <- format_line_tokens(var_tokens)

        # Find opening paren after IF
        open_paren_idx <- if_idx + 1
        if (open_paren_idx > nrow(terminals)) { next }
        if (terminals$token[open_paren_idx] != "'('") { next }

        # Find matching closing paren for condition
        paren_depth <- 1
        close_paren_idx <- open_paren_idx + 1
        while (close_paren_idx <= nrow(terminals) && paren_depth > 0) {
            if (terminals$token[close_paren_idx] == "'('") {
                paren_depth <- paren_depth + 1
            } else if (terminals$token[close_paren_idx] == "')'") {
                paren_depth <- paren_depth - 1
            }
            if (paren_depth > 0) { close_paren_idx <- close_paren_idx + 1 }
        }

        if (close_paren_idx > nrow(terminals)) { next }

        # Extract condition tokens
        cond_tokens <- terminals[(open_paren_idx + 1):(close_paren_idx - 1),]
        cond_text <- format_line_tokens(cond_tokens)

        # Find ELSE token
        else_idx <- NULL
        # Track nesting to find the right ELSE
        search_idx <- close_paren_idx + 1
        nest_depth <- 0
        brace_depth <- 0

        while (search_idx <= nrow(terminals)) {
            stok <- terminals[search_idx,]

            # Track brace boundaries — stop if we exit the enclosing block
            if (stok$token == "'{'") {
                brace_depth <- brace_depth + 1
            } else if (stok$token == "'}'") {
                brace_depth <- brace_depth - 1
                if (brace_depth < 0) { break }
            }

            if (stok$token == "IF") {
                nest_depth <- nest_depth + 1
            } else if (stok$token == "ELSE") {
                if (nest_depth == 0 && brace_depth == 0) {
                    else_idx <- search_idx
                    break
                } else if (nest_depth > 0) {
                    nest_depth <- nest_depth - 1
                }
            }
            search_idx <- search_idx + 1
        }

        if (is.null(else_idx)) { next }

        # Skip if-else that already has braces (already in expanded form)
        body_first <- terminals[close_paren_idx + 1,]
        if (body_first$token == "'{'") { next }

        # Extract true expression (between close_paren and else)
        true_tokens <- terminals[(close_paren_idx + 1):(else_idx - 1),]
        true_text <- format_line_tokens(true_tokens)

        # Extract false expression (after else to end of statement)
        # Need to find where the statement ends
        false_start <- else_idx + 1
        if (false_start > nrow(terminals)) { next }

        # Skip braced false body — multi-statement blocks can't be inlined
        if (terminals$token[false_start] == "'{'") { next }

        # Find end of false expression - could be end of line or next statement
        false_end <- false_start
        false_paren_depth <- 0
        false_brace_depth <- 0
        false_if_depth <- 0 # track nested if-else
        false_start_line <- terminals$line1[false_start]

        while (false_end <= nrow(terminals)) {
            ftok <- terminals[false_end,]

            # Track if-else nesting for chained else-if on same line only
            # (IF tokens on subsequent lines are new statements, not part of
            # the false expression)
            if (ftok$token == "IF" && ftok$line1 == false_start_line) {
                false_if_depth <- false_if_depth + 1
            }
            if (ftok$token == "ELSE" && ftok$line1 == false_start_line) {
                false_if_depth <- max(0, false_if_depth - 1)
            }

            # Track nesting
            prev_paren_depth <- false_paren_depth
            if (ftok$token == "'('") {
                false_paren_depth <- false_paren_depth + 1
            }
            if (ftok$token == "')'") {
                false_paren_depth <- false_paren_depth - 1
            }
            if (ftok$token == "'{'") {
                false_brace_depth <- false_brace_depth + 1
            }
            if (ftok$token == "'}'") {
                false_brace_depth <- false_brace_depth - 1
            }

            # If we just closed the outermost paren/brace, include this
            # token and stop — but not inside an if-else (condition parens)
            if (prev_paren_depth > 0 && false_paren_depth == 0 &&
                false_brace_depth == 0 && false_if_depth == 0) {
                break
            }

            # End at line break when not nested (for simple expressions)
            if (ftok$line1 > false_start_line && false_paren_depth <= 0 &&
                false_brace_depth <= 0 && false_if_depth == 0) {
                false_end <- false_end - 1
                break
            }

            false_end <- false_end + 1
        }

        if (false_end > nrow(terminals)) {
            false_end <- nrow(terminals)
        }

        # Determine line ranges for true and false expressions
        if (nrow(true_tokens) > 0) {
            true_end_line <- max(true_tokens$line1)
        } else {
            true_end_line <- assign_line
        }
        false_tokens <- terminals[false_start:false_end,]
        # Skip if branches are complex (function expressions, comments, or multi-line)
        if (any(true_tokens$token == "FUNCTION") ||
            any(false_tokens$token == "FUNCTION") ||
            any(true_tokens$token == "COMMENT") ||
            any(false_tokens$token == "COMMENT") ||
            any(true_tokens$line2 > true_tokens$line1) ||
            any(false_tokens$line2 > false_tokens$line1)) {
            next
        }
        if (nrow(false_tokens) > 0) {
            false_end_line <- max(false_tokens$line2)
        } else {
            false_end_line <- assign_line
        }

        # Skip multi-line inline if-else when line_limit > 0 (avoid breaking)
        spans_lines <- false_end_line > assign_line ||
        true_end_line > assign_line
        if (spans_lines && line_limit > 0L) {
            next
        }

        # Skip short single-line expressions when line_limit > 0
        if (line_limit > 0L && !spans_lines &&
            nchar(lines[assign_line]) <= line_limit) {
            next
        }

        # Skip chained if-else expressions (else clause starts with IF)
        # e.g., status <- if (a) "x" else if (b) "y" else "z"
        # These are a valid R idiom; don't expand unless user requested it
        if (line_limit > 0L && false_start <= nrow(terminals) &&
            terminals$token[false_start] == "IF") {
            next
        }

        # Get base indent from current line
        current_line <- lines[assign_line]
        base_indent <- sub("^(\\s*).*", "\\1", current_line)
        inner_indent <- paste0(base_indent, "    ")

        # Extract true and false expressions from tokens
        # Always use format_line_tokens (not extract_expr_text) since the
        # expression may share source lines with other parts of the if-else
        true_text <- format_line_tokens(true_tokens)
        false_text <- format_line_tokens(false_tokens)

        # Build replacement lines
        new_lines <- c(paste0(base_indent, "if (", cond_text, ") {"),
                       paste0(inner_indent, var_name, " <- ", true_text),
                       paste0(base_indent, "} else {"),
                       paste0(inner_indent, var_name, " <- ", false_text),
                       paste0(base_indent, "}"))

        # Find actual end line (could span multiple lines)
        end_line <- false_end_line

        # Replace lines
        pre <- if (assign_line > 1) lines[1:(assign_line - 1)] else character(0)
        if (end_line < length(lines)) {
            post <- lines[(end_line + 1):length(lines)]
        } else {
            post <- character(0)
        }
        new_code_lines <- c(pre, new_lines, post)

        result <- paste(new_code_lines, collapse = "\n")
        if (!grepl("\n$", result)) {
            result <- paste0(result, "\n")
        }

        return(result)
    }

    NULL
}

