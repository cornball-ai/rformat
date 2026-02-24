# Package-level format options (avoids threading through 15+ call sites)
.fmt_opts <- new.env(parent = emptyenv())
.fmt_opts$function_space <- FALSE

#' Format R Code Using Token-Based Parsing
#'
#' Internal function to format R code using getParseData tokens.
#' Calculates proper indentation based on nesting depth.
#'
#' @param code Character string of R code.
#' @param indent Integer for spaces (default 4), or character string for
#'   literal indent (e.g., `"\\t\\t"` for vintage R Core style).
#' @param wrap Continuation style: `"paren"` (default) aligns to opening
#'   parenthesis, `"fixed"` uses 8-space indent.
#' @param expand_if Expand inline if-else to multi-line (default FALSE).
#' @param brace_style Brace placement: `"kr"` (same line) or `"allman"` (new line).
#' @param line_limit Maximum line length before wrapping (default 80).
#' @param function_space If TRUE, add space before `(` in function definitions.
#' @return Formatted code as character string.
#' @importFrom utils getParseData
#' @keywords internal
format_tokens <- function (code, indent = 4L, wrap = "paren",
                           expand_if = FALSE, brace_style = "kr",
                           line_limit = 80L, function_space = FALSE) {
    .fmt_opts$function_space <- function_space
    # Parse with source tracking
    parsed <- tryCatch(parse(text = code, keep.source = TRUE),
                       error = function(e) NULL)

    if (is.null(parsed)) {
        warning("Could not parse code, returning unchanged")
        return(code)
    }

    pd <- getParseData(parsed)

    if (is.null(pd) || nrow(pd) == 0) {
        return(code)
    }

    # Get terminal tokens
    terminals <- pd[pd$terminal,]
    terminals <- terminals[order(terminals$line1, terminals$col1),]

    # Split original into lines for comment preservation
    orig_lines <- strsplit(code, "\n", fixed = TRUE)[[1]]

    terminals <- restore_truncated_str_const_tokens(terminals, orig_lines)

    # Compute nesting-based indent levels for each line
    max_line <- max(c(terminals$line1, length(orig_lines)))
    nesting <- compute_nesting(terminals, max_line)
    line_indent <- nesting$line_indent
    line_end_brace <- nesting$line_end_brace
    line_end_paren <- nesting$line_end_paren
    line_end_pab <- nesting$line_end_pab

    # Build output
    if (is.character(indent)) {
        indent_str <- indent
    } else {
        indent_str <- strrep(" ", indent)
    }
    out_lines <- character(length(orig_lines))
    # Track open call-paren output columns for paren-aligned continuation.
    # Stack of character positions (1-based) of '(' in output lines.
    out_call_paren_stack <- integer(0)
    # Per-line: output column of innermost open call '(' after processing
    out_call_paren_col <- integer(length(orig_lines))
    # Track brace depth relative to the innermost call paren.
    # We only paren-align when no braces have opened since the call '('.
    # Stack of brace depths at each call-paren open; current brace depth.
    out_brace_depth <- 0L
    out_brace_at_call_open <- integer(0) # brace_depth when each call ( opened

    # Track lines that are internal to multi-line tokens (e.g., multi-line strings)
    # A line should be skipped if:
    # 1. It's within the span of a multi-line token (between line1+1 and line2)
    # 2. AND no tokens START on that line (line1 == line_num)
    # This handles the case where a multi-line string ends on the same line as
    # other tokens (e.g., closing parenthesis)
    multiline_covered <- logical(length(orig_lines))
    for (i in seq_len(nrow(terminals))) {
        tok <- terminals[i,]
        if (tok$line2 > tok$line1) {
            # This token spans multiple lines - mark continuation lines
            for (ln in (tok$line1 + 1):tok$line2) {
                if (ln <= length(multiline_covered)) {
                    multiline_covered[ln] <- TRUE
                }
            }
        }
    }

    for (line_num in seq_along(orig_lines)) {
        line <- orig_lines[line_num]

        # Check if any tokens START on this line
        line_tokens <- terminals[terminals$line1 == line_num,]

        # Skip lines that are purely internal to multi-line tokens
        # (covered by a multi-line token AND no tokens start here)
        if (multiline_covered[line_num] && nrow(line_tokens) == 0) {
            out_lines[line_num] <- NA_character_
            next
        }

        if (grepl("^\\s*$", line)) {
            out_lines[line_num] <- ""
            next
        }

        if (nrow(line_tokens) == 0) {
            out_lines[line_num] <- line
            next
        }

        # Paren-aligned continuation: use output column of innermost
        # open call '(' when no braces have opened since that call
        cont_call_indent <- 0L
        inside_call_brace <- length(out_brace_at_call_open) > 0 &&
        out_brace_depth > out_brace_at_call_open[length(out_brace_at_call_open)]
        if (wrap == "paren" && !inside_call_brace &&
            length(out_call_paren_stack) > 0) {
            open_calls <- out_call_paren_stack[out_call_paren_stack > 0L]
            if (length(open_calls) > 0) {
                first_tok <- line_tokens$token[1]
                if (!first_tok %in% c("')'", "']'", "']]'", "IF", "FOR",
                                      "WHILE", "REPEAT", "ELSE", "'}'")) {
                    cont_call_indent <- open_calls[length(open_calls)]
                }
            }
        }

        if (nrow(line_tokens) == 1 && line_tokens$token[1] == "COMMENT") {
            if (grepl("^#'", trimws(line))) {
                out_lines[line_num] <- trimws(line)
            } else {
                if (line_num <= max_line) {
                    line_level <- line_indent[line_num]
                } else {
                    line_level <- 0
                }
                depth_prefix <- paste0(rep(indent_str, line_level),
                                       collapse = "")
                source_indent <- sub("\\S.*", "", line)
                if (nzchar(source_indent) && source_indent != depth_prefix &&
                    !grepl("\t", source_indent, fixed = TRUE)) {
                    out_lines[line_num] <- paste0(source_indent, trimws(line))
                } else {
                    out_lines[line_num] <- paste0(depth_prefix, trimws(line))
                }
            }
            next
        }

        if (line_num <= max_line) {
            line_level <- line_indent[line_num]
        } else {
            line_level <- 0
        }
        formatted <- format_line_tokens(line_tokens)
        depth_prefix <- paste0(rep(indent_str, line_level), collapse = "")
        if (cont_call_indent > 0L && cont_call_indent <= line_limit %/% 2L) {
            paren_prefix <- strrep(" ", cont_call_indent)
            # Preserve depth-based indent from wrap passes: if the source
            # line already has exactly the depth-based prefix, keep it
            # rather than switching to paren-aligned (which wrap passes
            # don't produce, causing idempotency drift).
            source_indent <- sub("\\S.*", "", line)
            if (source_indent == depth_prefix && depth_prefix != paren_prefix) {
                line_prefix <- depth_prefix
            } else {
                line_prefix <- paren_prefix
            }
        } else {
            line_prefix <- depth_prefix
        }

        out_lines[line_num] <- paste0(line_prefix, formatted)

        # Update output call-paren stack using this line's tokens.
        # Compute each token's output column from the formatted line.
        prefix_len <- nchar(line_prefix)
        for (ti in seq_len(nrow(line_tokens))) {
            tt <- line_tokens$token[ti]
            if (tt == "'('") {
                # Is this a call paren? Check if preceded by SYMBOL_FUNCTION_CALL
                is_call <- ti > 1L &&
                line_tokens$token[ti - 1L] == "SYMBOL_FUNCTION_CALL"
                if (!is_call && ti == 1L) {
                    # Check the last token from the previous line
                    # (already in the global terminals order)
                    global_idx <- which(terminals$line1 == line_num &
                        terminals$col1 == line_tokens$col1[ti])
                    if (length(global_idx) > 0 && global_idx[1] > 1L) {
                        prev_tok <- terminals$token[global_idx[1] - 1L]
                        is_call <- prev_tok == "SYMBOL_FUNCTION_CALL"
                    }
                }
                # Compute output column: prefix + position in formatted string
                # The formatted string has tokens in order; find this token's
                # position by rebuilding up to this point
                tok_output_col <- prefix_len +
                find_token_pos_in_formatted(line_tokens, ti)
                out_call_paren_stack <- c(out_call_paren_stack,
                    if (is_call) {
                        tok_output_col
                    } else {
                        0L
                    })
                out_brace_at_call_open <- c(out_brace_at_call_open,
                    out_brace_depth)
            } else if (tt == "')'") {
                if (length(out_call_paren_stack) > 0) {
                    out_call_paren_stack <-
                    out_call_paren_stack[-length(out_call_paren_stack)]
                    out_brace_at_call_open <-
                    out_brace_at_call_open[-length(out_brace_at_call_open)]
                }
            } else if (tt == "'{'") {
                out_brace_depth <- out_brace_depth + 1L
            } else if (tt == "'}'") {
                out_brace_depth <- max(0L, out_brace_depth - 1L)
            }
        }
    }

    # Filter out NA lines (multi-line token continuations)
    out_lines <- out_lines[!is.na(out_lines)]
    result <- paste(out_lines, collapse = "\n")
    if (!grepl("\n$", result) && nchar(result) > 0) {
        result <- paste0(result, "\n")
    }

    # Split into top-level expressions and format each independently.
    # Each expression gets the full iteration budget since it's small.
    chunks <- split_toplevel(result)
    parts <- character(length(chunks))
    for (i in seq_along(chunks)) {
        if (chunks[[i]]$is_expr) {
            parts[i] <- format_pipeline(chunks[[i]]$text, indent, wrap,
                                        expand_if, brace_style, line_limit,
                                        function_space)
        } else {
            parts[i] <- chunks[[i]]$text
        }
    }
    result <- paste(parts, collapse = "\n")
    if (!grepl("\n$", result) && nchar(result) > 0) {
        result <- paste0(result, "\n")
    }
    result
}

#' Compute Nesting Depth Per Line
#'
#' Shared function used by `format_tokens` and wrap passes to compute
#' identical depth-based indent levels from the parse tree.
#'
#' @param terminals Terminal token data frame from `getParseData()`,
#'   ordered by `line1, col1`.
#' @param n_lines Number of source lines.
#' @return Named list with `line_indent`, `line_end_brace`,
#'   `line_end_paren`, `line_end_pab` (all integer vectors of length
#'   `n_lines`).
#' @keywords internal
compute_nesting <- function (terminals, n_lines) {
    brace_depth <- 0L
    paren_depth <- 0L
    paren_at_brace <- integer(0)

    line_end_brace <- integer(n_lines)
    line_end_paren <- integer(n_lines)
    line_end_pab <- integer(n_lines)
    line_pab_after_close <- integer(n_lines)

    for (i in seq_len(nrow(terminals))) {
        tok <- terminals[i,]
        ln <- tok$line1

        if (tok$token == "'{'") {
            is_ctrl_brace <- FALSE
            if (i >= 2L) {
                pt <- terminals$token[i - 1L]
                if (pt == "ELSE" || pt == "REPEAT") {
                    is_ctrl_brace <- TRUE
                } else if (pt == "')'") {
                    pd2 <- 1L
                    k <- i - 2L
                    while (k >= 1L && pd2 > 0L) {
                        if (terminals$token[k] == "')'") { pd2 <- pd2 + 1L }
                        if (terminals$token[k] == "'('") { pd2 <- pd2 - 1L }
                        if (pd2 > 0L) { k <- k - 1L }
                    }
                    if (k >= 2L &&
                        terminals$token[k - 1L] %in% c("IF", "FOR", "WHILE")) {
                        is_ctrl_brace <- TRUE
                    }
                }
            }
            if (is_ctrl_brace) {
                enclosing_pab <- if (length(paren_at_brace) > 0) {
                    paren_at_brace[length(paren_at_brace)]
                } else {
                    0L
                }
                paren_at_brace <- c(paren_at_brace, enclosing_pab)
            } else {
                paren_at_brace <- c(paren_at_brace, paren_depth)
            }
            brace_depth <- brace_depth + 1L
        } else if (tok$token == "'}'") {
            brace_depth <- max(0L, brace_depth - 1L)
            if (length(paren_at_brace) > 0) {
                paren_at_brace <- paren_at_brace[-length(paren_at_brace)]
            }
            line_pab_after_close[ln] <- if (length(paren_at_brace) > 0) {
                paren_at_brace[length(paren_at_brace)]
            } else {
                0L
            }
        } else if (tok$token %in% c("'('", "'['", "LBB")) {
            paren_depth <- paren_depth + if (tok$token == "LBB") 2L else 1L
        } else if (tok$token %in% c("')'", "']'")) {
            paren_depth <- max(0L, paren_depth - 1L)
        }

        line_end_brace[ln] <- brace_depth
        line_end_paren[ln] <- paren_depth
        line_end_pab[ln] <- if (length(paren_at_brace) > 0) {
            paren_at_brace[length(paren_at_brace)]
        } else {
            0L
        }
    }

    # Fill gaps (comment/blank lines inherit from previous)
    for (ln in seq_len(n_lines)) {
        if (ln > 1 && line_end_brace[ln] == 0 && line_end_paren[ln] == 0) {
            if (nrow(terminals[terminals$line1 == ln,]) == 0) {
                line_end_brace[ln] <- line_end_brace[ln - 1]
                line_end_paren[ln] <- line_end_paren[ln - 1]
                line_end_pab[ln] <- line_end_pab[ln - 1]
            }
        }
    }

    # Calculate indent at START of each line
    line_indent <- integer(n_lines)
    for (ln in seq_len(n_lines)) {
        if (ln == 1) {
            line_indent[ln] <- 0L
        } else {
            prev_brace <- line_end_brace[ln - 1]
            prev_paren <- line_end_paren[ln - 1]

            line_tokens <- terminals[terminals$line1 == ln,]
            if (nrow(line_tokens) > 0 && line_tokens$token[1] == "'}'") {
                prev_brace <- max(0L, prev_brace - 1L)
            }
            if (nrow(line_tokens) > 0 &&
                line_tokens$token[1] %in% c("')'", "']'")) {
                prev_paren <- max(0L, prev_paren - 1L)
            }

            prev_pab <- line_end_pab[ln - 1]
            if (nrow(line_tokens) > 0 && line_tokens$token[1] == "'}'") {
                prev_pab <- line_pab_after_close[ln]
            }
            line_indent[ln] <- prev_brace + max(0L, prev_paren - prev_pab)
        }
    }

    list(line_indent = line_indent, line_end_brace = line_end_brace,
         line_end_paren = line_end_paren, line_end_pab = line_end_pab)
}

#' Compute Indent at a Column Position
#'
#' Walks tokens on a line up to a given column, tracking braces and parens
#' exactly as compute_nesting does. Returns the indent level that a
#' hypothetical continuation line would receive.
#'
#' @param nesting Result from compute_nesting().
#' @param line_toks Tokens on the line.
#' @param line_num Line number.
#' @param break_col Column position to stop at (inclusive).
#' @return Integer indent level.
#' @keywords internal
compute_indent_at_col <- function (nesting, line_toks, line_num, break_col) {
    # Start from state at end of previous line
    if (line_num > 1L) {
        brace_depth <- nesting$line_end_brace[line_num - 1L]
        paren_depth <- nesting$line_end_paren[line_num - 1L]
        pab <- nesting$line_end_pab[line_num - 1L]
    } else {
        brace_depth <- 0L
        paren_depth <- 0L
        pab <- 0L
    }
    pab_stack <- if (brace_depth > 0L) rep(pab, brace_depth) else integer(0)

    for (j in seq_len(nrow(line_toks))) {
        tok_j <- line_toks[j,]
        if (tok_j$col1 > break_col) { break }
        if (tok_j$token == "'{'") {
            pab_stack <- c(pab_stack, paren_depth)
            brace_depth <- brace_depth + 1L
        } else if (tok_j$token == "'}'") {
            brace_depth <- max(0L, brace_depth - 1L)
            if (length(pab_stack) > 0L) {
                pab_stack <- pab_stack[-length(pab_stack)]
            }
        } else if (tok_j$token %in% c("'('", "'['")) {
            paren_depth <- paren_depth + 1L
        } else if (tok_j$token == "LBB") {
            paren_depth <- paren_depth + 2L
        } else if (tok_j$token %in% c("')'", "']'")) {
            paren_depth <- max(0L, paren_depth - 1L)
        } else if (tok_j$token == "']]'") {
            paren_depth <- max(0L, paren_depth - 2L)
        }
    }

    cur_pab <- if (length(pab_stack) > 0L) {
        pab_stack[length(pab_stack)]
    } else {
        0L
    }
    brace_depth + max(0L, paren_depth - cur_pab)
}

#' Format Tokens on a Single Line
#'
#' @param tokens Data frame of tokens for one line.
#' @param prev_token Optional token to treat as the previous token when
#'   formatting a token subset (e.g., suffix after a collapsed call).
#' @param prev_prev_token Optional token before prev_token for unary detection.
#' @return Formatted line content (no leading whitespace).
#' @keywords internal
format_line_tokens <- function (tokens, prev_token = NULL,
                                prev_prev_token = NULL) {
    if (nrow(tokens) == 0) {
        return("")
    }

    tokens <- tokens[order(tokens$line1, tokens$col1),]
    parts <- character(nrow(tokens))
    prev <- prev_token
    prev_prev <- prev_prev_token

    for (i in seq_len(nrow(tokens))) {
        tok <- tokens[i,]

        # Convert = assignment to <-
        tok_text <- tok$text
        if (tok$token == "EQ_ASSIGN") {
            tok_text <- "<-"
        }

        if (!is.null(prev)) {
            if (needs_space(prev, tok, prev_prev)) {
                parts[i] <- paste0(" ", tok_text)
            } else {
                parts[i] <- tok_text
            }
        } else {
            parts[i] <- tok_text
        }

        prev_prev <- prev
        prev <- tok
    }

    paste(parts, collapse = "")
}

#' Find Token Position in Formatted Line Output
#'
#' Computes the 1-based character position where the token at index `idx`
#' starts in the output of `format_line_tokens(tokens)`. This replays the
#' spacing logic to determine the exact output column.
#'
#' @param tokens Data frame of tokens for one line (ordered by col1).
#' @param idx Index into `tokens` of the target token.
#' @return 1-based character position of that token in the formatted output.
#' @keywords internal
find_token_pos_in_formatted <- function (tokens, idx) {
    pos <- 1L
    prev <- NULL
    prev_prev <- NULL

    for (i in seq_len(nrow(tokens))) {
        tok <- tokens[i,]
        tok_text <- tok$text
        if (tok$token == "EQ_ASSIGN") {
            tok_text <- "<-"
        }

        if (!is.null(prev)) {
            if (needs_space(prev, tok, prev_prev)) {
                pos <- pos + 1L
            }
        }

        if (i == idx) {
            return(pos)
        }

        pos <- pos + nchar(tok_text)
        prev_prev <- prev
        prev <- tok
    }

    pos
}

#' Restore Truncated String Constant Token Text
#'
#' `utils::getParseData()` truncates long `STR_CONST` token text. Reconstruct the
#' original literal from source lines so token-based rewrite passes can round-trip
#' long strings without introducing parse-invalid placeholders.
#'
#' @param terminals Terminal token data frame from `getParseData()`.
#' @param orig_lines Original source lines.
#' @return `terminals` with long `STR_CONST` text restored.
#' @keywords internal
restore_truncated_str_const_tokens <- function (terminals, orig_lines) {
    if (is.null(terminals) || nrow(terminals) == 0) {
        return(terminals)
    }

    idx <- which(terminals$token == "STR_CONST" &
        grepl("^\\[\\d+ chars quoted", terminals$text))
    if (length(idx) == 0) {
        return(terminals)
    }

    for (i in idx) {
        tok <- terminals[i,]
        if (tok$line1 == tok$line2) {
            terminals$text[i] <- substring(orig_lines[tok$line1],
                col_to_charpos(orig_lines[tok$line1], tok$col1),
                col_to_charpos(orig_lines[tok$line1], tok$col2))
        } else {
            parts <- c(
                       substring(orig_lines[tok$line1],
                                 col_to_charpos(orig_lines[tok$line1],
                        tok$col1)),
                if (tok$line2 > tok$line1 + 1) {
                    orig_lines[(tok$line1 + 1):(tok$line2 - 1)]
                },
                       substring(orig_lines[tok$line2], 1,
                                 col_to_charpos(orig_lines[tok$line2],
                        tok$col2))
            )
            terminals$text[i] <- paste(parts, collapse = "\n")
        }
    }

    terminals
}

#' Determine If Space Needed Between Tokens
#'
#' @param prev Previous token (data frame row).
#' @param tok Current token (data frame row).
#' @param prev_prev Token before prev (data frame row or NULL), for unary detection.
#' @return Logical.
#' @keywords internal
needs_space <- function (prev, tok, prev_prev = NULL) {
    p <- prev$token
    t <- tok$token

    # Always space before comments
    if (t == "COMMENT") {
        return(TRUE)
    }

    binary_ops <- c("LEFT_ASSIGN", "EQ_ASSIGN", "RIGHT_ASSIGN", "EQ_SUB",
                    "EQ_FORMALS", "AND", "OR", "AND2", "OR2", "GT", "LT", "GE",
                    "LE", "EQ", "NE", "'+'", "'-'", "'*'", "'/'", "'^'",
                    "SPECIAL", "'~'")

    if (p %in% c("'('", "'['", "LBB")) {
        return(FALSE)
    }

    if (t %in% c("')'", "']'", "']]'", "','")) {
        return(FALSE)
    }

    if (p == "')'") {
        if (t %in% c("'['", "LBB", "'('", "'$'", "'@'", "':'")) {
            return(FALSE)
        }
        return(TRUE)
    }

    if (t == "'('" && p == "SYMBOL_FUNCTION_CALL") {
        return(FALSE)
    }

    if (p == "'!'") {
        return(FALSE)
    }

    if (p %in% c("'$'", "'@'", "':'") || t %in% c("'$'", "'@'", "':'")) {
        return(FALSE)
    }

    if (p == "NS_GET" || t == "NS_GET" || p == "NS_GET_INT" ||
        t == "NS_GET_INT") {
        return(FALSE)
    }

    if (p %in% c("IF", "ELSE", "FOR", "WHILE", "REPEAT", "IN")) {
        return(TRUE)
    }

    # Space before IN and ELSE keywords
    if (t %in% c("IN", "ELSE")) {
        return(TRUE)
    }

    if (t == "'('" && p == "FUNCTION") {
        return(.fmt_opts$function_space)
    }

    if (t == "'{'") {
        return(TRUE)
    }

    if (p == "'{'") {
        return(TRUE)
    }

    if (t == "'}'") {
        return(TRUE)
    }

    if (p == "'}'") {
        return(TRUE)
    }

    # Detect unary minus/plus: no space after - or + when it's unary
    if (p %in% c("'-'", "'+'")) {
        if (is.null(prev_prev)) {
            # First real token is -/+, it's unary
            return(FALSE)
        }
        pp <- prev_prev$token
        unary_context <- c("'('", "'['", "LBB", "','", "'{'", "LEFT_ASSIGN",
                           "EQ_ASSIGN", "RIGHT_ASSIGN", "EQ_SUB", "EQ_FORMALS",
                           "AND", "OR", "AND2", "OR2", "GT", "LT", "GE", "LE",
                           "EQ", "NE", "'+'", "'-'", "'*'", "'/'", "'^'",
                           "SPECIAL", "'~'", "'!'", "IF", "ELSE", "FOR",
                           "WHILE", "REPEAT", "IN", "RETURN", "NEXT", "BREAK")
        if (pp %in% unary_context) {
            return(FALSE)
        }
    }

    if (t %in% binary_ops || p %in% binary_ops) {
        return(TRUE)
    }

    if (p %in% c("','", "';'")) {
        return(TRUE)
    }

    if (p %in% c("NEXT", "BREAK", "RETURN")) {
        return(TRUE)
    }

    symbols <- c("SYMBOL", "SYMBOL_FUNCTION_CALL", "SYMBOL_FORMALS",
                 "SYMBOL_SUB", "SYMBOL_PACKAGE", "NUM_CONST", "STR_CONST",
                 "NULL_CONST", "SPECIAL")

    if (p %in% symbols && t %in% symbols) {
        return(TRUE)
    }

    FALSE
}

#' Convert Tab-Expanded Column to Character Position
#'
#' R's getParseData() reports columns with tabs expanded to 8-column tab stops.
#' This function converts such a column back to a character position for use
#' with substring().
#'
#' @param line A single line of text.
#' @param col Tab-expanded column position (1-based).
#' @return Character position (1-based) in the string.
#' @keywords internal
col_to_charpos <- function (line, col) {
    if (!grepl("\t", line, fixed = TRUE)) {
        return(col)
    }
    chars <- strsplit(line, "")[[1]]
    display_col <- 0L
    for (i in seq_along(chars)) {
        if (chars[i] == "\t") {
            display_col <- display_col + (8L - (display_col %% 8L))
        } else {
            display_col <- display_col + 1L
        }
        if (display_col >= col) {
            return(i)
        }
    }
    length(chars)
}

#' Get Tab-Expanded Line Length
#'
#' Returns the display width of a line, with tabs expanded to 8-column stops.
#'
#' @param line A single line of text.
#' @return Display width of the line.
#' @keywords internal
display_width <- function (line) {
    if (!grepl("\t", line, fixed = TRUE)) {
        return(nchar(line))
    }
    chars <- strsplit(line, "")[[1]]
    col <- 0L
    for (ch in chars) {
        if (ch == "\t") {
            col <- col + (8L - (col %% 8L))
        } else {
            col <- col + 1L
        }
    }
    col
}

#' Extract Expression Text from Source Lines
#'
#' Extract original text for a multi-line expression and re-indent it.
#'
#' @param lines Source code lines.
#' @param tokens Token data frame for the expression.
#' @param target_indent Target indentation string for continuation lines.
#' @return Expression text with first line unindented, continuation lines re-indented.
#' @keywords internal
extract_expr_text <- function (lines, tokens, target_indent) {
    if (nrow(tokens) == 0) { return("") }

    # Get line range
    start_line <- min(tokens$line1)
    end_line <- max(tokens$line2) # Use line2 for end position

    # Single line - just use format_line_tokens
    if (start_line == end_line) {
        return(format_line_tokens(tokens))
    }

    # Multi-line - extract from source
    # First line: from first token to end of line
    first_tok <- tokens[tokens$line1 == start_line,][1,]
    first_line_text <- substring(lines[start_line],
                                 col_to_charpos(lines[start_line],
            first_tok$col1))

    # Middle lines: full line content, re-indented
    result_lines <- first_line_text

    if (end_line > start_line) {
        for (ln in (start_line + 1):end_line) {
            line_text <- lines[ln]
            # Remove existing indentation and add target indent
            trimmed <- sub("^\\s*", "", line_text)
            # Add extra indent for continuation (2 more spaces)
            result_lines <- c(result_lines,
                              paste0("\n", target_indent, "  ", trimmed))
        }
    }

    paste(result_lines, collapse = "")
}

#' Split Code into Top-Level Expressions
#'
#' Parses code to find top-level expressions, returning a list of chunks.
#' Each chunk is either an expression (code string) or an inter-expression
#' gap (comments, blank lines). Chunks concatenate back to the original.
#'
#' @param code Character string of R code.
#' @return List of `list(text = "...", is_expr = TRUE/FALSE)` pairs.
#' @keywords internal
split_toplevel <- function (code) {
    parsed <- tryCatch(parse(text = code, keep.source = TRUE),
                       error = function(e) NULL)

    if (is.null(parsed)) {
        return(list(list(text = code, is_expr = FALSE)))
    }

    pd <- getParseData(parsed)
    if (is.null(pd) || nrow(pd) == 0) {
        return(list(list(text = code, is_expr = FALSE)))
    }

    # Find top-level expr nodes (R's parser uses both "expr" and
    # "expr_or_assign_or_help" for top-level statements)
    top_exprs <- pd[pd$parent == 0L & pd$token %in%
        c("expr", "expr_or_assign_or_help"),]
    if (nrow(top_exprs) == 0) {
        return(list(list(text = code, is_expr = FALSE)))
    }

    lines <- strsplit(code, "\n", fixed = TRUE)[[1]]
    n_lines <- length(lines)
    chunks <- list()
    prev_end <- 0L

    for (k in seq_len(nrow(top_exprs))) {
        expr_start <- top_exprs$line1[k]
        expr_end <- top_exprs$line2[k]

        # Gap before this expression
        if (expr_start > prev_end + 1L) {
            gap_lines <- lines[(prev_end + 1L):(expr_start - 1L)]
            chunks[[length(chunks) + 1L]] <- list(
                text = paste(gap_lines, collapse = "\n"), is_expr = FALSE)
        }

        # The expression itself
        expr_lines <- lines[expr_start:expr_end]
        chunks[[length(chunks) + 1L]] <- list(
            text = paste(expr_lines, collapse = "\n"), is_expr = TRUE)

        prev_end <- expr_end
    }

    # Trailing gap after last expression
    if (prev_end < n_lines) {
        gap_lines <- lines[(prev_end + 1L):n_lines]
        chunks[[length(chunks) + 1L]] <- list(
            text = paste(gap_lines, collapse = "\n"), is_expr = FALSE)
    }

    chunks
}

#' Apply Formatting Pipeline to a Code Fragment
#'
#' Runs collapse, brace, wrap, function-def, and inline-if passes on a
#' single code string. Designed to operate on individual top-level
#' expressions so each gets the full iteration budget.
#'
#' @param code Character string of R code.
#' @param indent Indent size or string.
#' @param wrap Continuation style.
#' @param expand_if Expand inline if-else.
#' @param brace_style Brace placement style.
#' @param line_limit Maximum line length.
#' @param function_space If TRUE, add space before `(` in function definitions.
#' @return Formatted code string.
#' @keywords internal
format_pipeline <- function (code, indent, wrap, expand_if, brace_style,
                             line_limit, function_space = FALSE) {
    # Collapse multi-line calls that fit on one line
    code <- apply_if_parseable(code, collapse_calls)

    # Add braces to one-liner control flow (before wrapping, so bare
    # bodies move to their own lines before line-length decisions)
    code <- apply_if_parseable(code, add_control_braces)

    # Expand bare if-else arguments in overlong calls before wrapping,
    # so the braced form is stable and wrap passes see clean lines
    code <- apply_if_parseable(code, expand_call_if_args,
                               line_limit = line_limit)

    # Wrap long lines at operators (||, &&), then at commas
    code <- apply_if_parseable(code, wrap_long_operators, indent = indent,
                               line_limit = line_limit)
    code <- apply_if_parseable(code, wrap_long_calls, wrap = wrap,
                               indent = indent, line_limit = line_limit)

    # Reformat function definitions
    code <- apply_if_parseable(code, reformat_function_defs, wrap = wrap,
                               brace_style = brace_style,
                               line_limit = line_limit)
    # Function-def rewrites can expose bare one-line control flow.
    code <- apply_if_parseable(code, add_control_braces)

    # Reformat inline if-else to multi-line
    # Always expand long lines; optionally expand all
    code <- apply_if_parseable(code, reformat_inline_if, line_limit = if (expand_if) {
            0L
        } else {
            line_limit
        })
    # Inline-if expansion can expose new bare control flow
    code <- apply_if_parseable(code, add_control_braces)

    # Final expansion of bare if-else call args
    code <- apply_if_parseable(code, expand_call_if_args,
                               line_limit = line_limit)

    # Final wrap pass: earlier passes may have produced new long lines.
    # Operator wrap runs both before and after call wrap because call
    # wrapping can leave operator+comment tails that exceed the limit.
    code <- apply_if_parseable(code, wrap_long_operators, indent = indent,
                               line_limit = line_limit)
    code <- apply_if_parseable(code, wrap_long_calls, wrap = wrap,
                               indent = indent, line_limit = line_limit)
    code <- apply_if_parseable(code, wrap_long_operators, indent = indent,
                               line_limit = line_limit)
    code
}

#' Format Blank Lines
#'
#' Normalize blank lines between code blocks.
#'
#' @param code Code string.
#' @return Code with normalized blank lines.
#' @keywords internal
format_blank_lines <- function (code) {
    # Remove trailing whitespace from each line
    code <- gsub(" +\n", "\n", code)
    # Remove blank lines after opening brace
    code <- gsub("\\{\n\\s*\n", "{\n", code)
    # Collapse multiple blank lines to one
    code <- gsub("\n{3,}", "\n\n", code)
    # Remove trailing newlines (keep one)
    code <- gsub("\n+$", "\n", code)
    code
}

#' Parse Gate for Transform Passes
#'
#' @param code Code string.
#' @return TRUE if code parses, FALSE otherwise.
#' @keywords internal
is_parseable_code <- function (code) {
    !is.null(tryCatch(parse(text = code, keep.source = TRUE),
                      error = function(e) NULL))
}

#' Apply Transform Only If Output Parses
#'
#' @param code Code string.
#' @param fn Transform function taking `code` as first argument.
#' @param ... Additional arguments passed to `fn`.
#' @return Transformed code if parseable, otherwise original code.
#' @keywords internal
apply_if_parseable <- function (code, fn, ...) {
    updated <- tryCatch(fn(code, ...), error = function(e) code)

    if (!is.character(updated) || length(updated) != 1L) {
        return(code)
    }

    if (!identical(updated, code) && !is_parseable_code(updated)) {
        return(code)
    }

    updated
}

