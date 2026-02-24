reformat_function_defs <- function (code, wrap = "paren", brace_style = "kr",
                                    line_limit = 80L) {
    # Process one function at a time, re-parsing each time
    # to handle line number changes
    changed <- TRUE
    max_iterations <- 200L

    while (changed && max_iterations > 0) {
        max_iterations <- max_iterations - 1
        changed <- FALSE

        result <- reformat_one_function(code, wrap = wrap,
                                        brace_style = brace_style,
                                        line_limit = line_limit)
        if (!is.null(result)) {
            code <- result
            changed <- TRUE
        }
    }

    code
}

#' Reformat One Function Definition
#'
#' Uses R Core continuation style: args on one line if they fit,
#' otherwise wrap with continuation indent.
#'
#' @param code Code string.
#' @param wrap Continuation style: `"paren"` aligns to opening parenthesis,
#'   `"fixed"` uses 8-space indent.
#' @param brace_style Brace placement: `"kr"` (same line) or `"allman"` (new line).
#' @param line_limit Maximum line length before wrapping (default 80).
#' @return Modified code or NULL if no changes.
#' @keywords internal
reformat_one_function <- function (code, wrap = "paren", brace_style = "kr",
                                   line_limit = 80L) {
    parsed <- tryCatch(parse(text = code, keep.source = TRUE),
                       error = function(e) NULL)

    if (is.null(parsed)) {
        return(NULL)
    }

    pd <- getParseData(parsed)
    if (is.null(pd) || nrow(pd) == 0) {
        return(NULL)
    }

    terminals <- pd[pd$terminal,]
    terminals <- terminals[order(terminals$line1, terminals$col1),]

    func_indices <- which(terminals$token == "FUNCTION")

    if (length(func_indices) == 0) {
        return(NULL)
    }

    lines <- strsplit(code, "\n", fixed = TRUE)[[1]]

    # Find first function that needs reformatting
    for (fi in func_indices) {
        func_tok <- terminals[fi,]
        func_line <- func_tok$line1

        # Only rewrite function definitions assigned to a name. Anonymous
        # function literals in defaults/calls are too unstable here.
        prev_idx <- fi - 1L
        while (prev_idx >= 1L && terminals$token[prev_idx] == "COMMENT") {
            prev_idx <- prev_idx - 1L
        }
        if (prev_idx >= 1L) {
            prev_tok <- terminals$token[prev_idx]
        } else {
            prev_tok <- NA_character_
        }
        if (!(prev_tok %in% c("LEFT_ASSIGN", "EQ_ASSIGN", "RIGHT_ASSIGN"))) {
            next
        }

        # Find the opening ( after function
        next_idx <- fi + 1
        if (next_idx > nrow(terminals)) { next }
        if (terminals$token[next_idx] != "'('") { next }

        # Find matching closing )
        paren_depth <- 1
        close_idx <- next_idx + 1
        while (close_idx <= nrow(terminals) && paren_depth > 0) {
            if (terminals$token[close_idx] == "'('") {
                paren_depth <- paren_depth + 1
            } else if (terminals$token[close_idx] == "')'") {
                paren_depth <- paren_depth - 1
            }
            if (paren_depth > 0) { close_idx <- close_idx + 1 }
        }

        if (close_idx > nrow(terminals)) { next }

        close_paren_line <- terminals$line1[close_idx]

        # Check if there's a { after the )
        has_brace <- close_idx + 1 <= nrow(terminals) &&
        terminals$token[close_idx + 1] == "'{'"
        if (has_brace) {
            brace_line <- terminals$line1[close_idx + 1]
        } else {
            brace_line <- NA
        }

        # Find what comes before 'function' on the line using token position
        func_line_content <- lines[func_line]
        base_indent <- sub("^(\\s*).*", "\\1", func_line_content)
        # func_tok$col1 is the 1-based column position of "function"
        if (func_tok$col1 > 1) {
            prefix <- substring(func_line_content, 1,
                                col_to_charpos(func_line_content,
                    func_tok$col1 - 1))
        } else {
            prefix <- ""
        }

        # Collect all formals with their defaults
        formal_texts <- character(0)
        i <- next_idx + 1
        while (i < close_idx) {
            tok <- terminals[i,]

            if (tok$token == "SYMBOL_FORMALS") {
                formal_name <- tok$text
                formal_text <- formal_name

                # Check for default value (EQ_FORMALS followed by value)
                if (i + 1 < close_idx &&
                    terminals$token[i + 1] == "EQ_FORMALS") {
                    formal_text <- paste0(formal_name, " = ")
                    i <- i + 2

                    # Collect the default value tokens
                    value_tokens <- list()
                    value_paren_depth <- 0

                    while (i < close_idx) {
                        vtok <- terminals[i,]
                        # Skip comments inside formals (e.g. # type: bool)
                        if (vtok$token == "COMMENT") {
                            i <- i + 1
                            next
                        }
                        if (vtok$token == "'('") {
                            value_paren_depth <- value_paren_depth + 1
                        }
                        if (vtok$token == "')'") {
                            if (value_paren_depth == 0) { break }
                            value_paren_depth <- value_paren_depth - 1
                        }
                        if (vtok$token == "','" && value_paren_depth == 0) {
                            break
                        }

                        value_tokens[[length(value_tokens) + 1]] <- vtok
                        i <- i + 1
                    }

                    # Format value tokens properly
                    if (length(value_tokens) > 0) {
                        value_df <- do.call(rbind,
                            lapply(value_tokens, as.data.frame))
                        formal_text <- paste0(formal_text,
                            format_line_tokens(value_df))
                    }
                }

                formal_texts <- c(formal_texts, formal_text)
            }

            i <- i + 1
        }

        # Build single-line signature: prefix + function (arg1, arg2, ...)
        single_line_sig <- paste0(prefix, "function (",
                                  paste(formal_texts, collapse = ", "), ")")

        # Account for " {" suffix when using K&R brace style
        if (has_brace && brace_style == "kr") {
            sig_limit <- line_limit - 2L
        } else {
            sig_limit <- line_limit
        }

        # Check if single line fits
        if (nchar(single_line_sig) <= sig_limit) {
            # Single line style
            new_lines <- single_line_sig
        } else {
            # Continuation style - wrap at commas
            if (wrap == "fixed") {
                cont_indent <- strrep(" ", 8L)
            } else {
                # Align to opening paren
                cont_indent <- strrep(" ", nchar(prefix) + nchar("function ("))
            }
            new_lines <- character(0)
            current_line <- paste0(prefix, "function (")

            for (j in seq_along(formal_texts)) {
                arg_with_comma <- if (j < length(formal_texts)) {
                    paste0(formal_texts[j], ", ")
                } else {
                    paste0(formal_texts[j], ")")
                }

                # Would adding this arg exceed the limit?
                test_line <- paste0(current_line, arg_with_comma)
                if (nchar(test_line) > sig_limit &&
                    nchar(current_line) > nchar(cont_indent)) {
                    # Wrap: save current line (remove trailing space if any)
                    new_lines <- c(new_lines, sub(" $", "", current_line))
                    current_line <- paste0(cont_indent, arg_with_comma)
                } else {
                    current_line <- test_line
                }
            }
            # Add final line
            new_lines <- c(new_lines, sub(" $", "", current_line))
        }

        # Check if there's a function body without braces (inline body)
        has_inline_body <- FALSE
        inline_body <- ""
        if (!has_brace && close_idx + 1 <= nrow(terminals)) {
            # Find end of inline body expression by tracking depth
            body_end_idx <- close_idx + 1
            body_depth <- 0
            while (body_end_idx <= nrow(terminals)) {
                btok <- terminals[body_end_idx,]
                if (btok$token %in% c("'('", "'['", "'[['", "'{'")) {
                    body_depth <- body_depth + 1
                } else if (btok$token == "LBB") {
                    body_depth <- body_depth + 2
                } else if (btok$token %in% c("')'", "']'", "'}'")) {
                    body_depth <- body_depth - 1
                    if (body_depth < 0) {
                        break # exiting enclosing context
                    }
                }
                if (btok$token == "','" && body_depth == 0) {
                    break # comma in enclosing call
                }
                if (body_depth == 0 && body_end_idx + 1 <= nrow(terminals)) {
                    next_tok <- terminals[body_end_idx + 1,]
                    if (next_tok$line1 > btok$line1) {
                        # Line break at depth 0 — check for continuation
                        cont_tokens <- c("'+'", "'-'", "'*'", "'/'", "'^'",
                            "SPECIAL", "AND", "OR", "AND2", "OR2", "GT", "LT",
                            "GE", "LE", "EQ", "NE", "LEFT_ASSIGN", "EQ_ASSIGN",
                            "'~'", "PIPE")
                        if (!(next_tok$token %in% cont_tokens)) { break }
                    }
                }
                body_end_idx <- body_end_idx + 1
            }
            if (body_end_idx > nrow(terminals)) {
                body_end_idx <- nrow(terminals)
            }
            # Exclude the breaking token (comma or closing bracket)
            if (body_end_idx > close_idx + 1 ||
                (body_end_idx == close_idx + 1 && body_depth >= 0 &&
                    terminals$token[body_end_idx] != "','")) {
                body_tokens <- terminals[(close_idx + 1):body_end_idx,]
                # Exclude trailing comma/close if we stopped at one
                if (body_depth < 0 || terminals$token[body_end_idx] == "','") {
                    body_end_idx <- body_end_idx - 1
                    body_tokens <- terminals[(close_idx + 1):body_end_idx,]
                }
                # Skip if inline body is complex (multi-line or contains braces/control)
                if (any(body_tokens$line2 > body_tokens$line1) ||
                    any(body_tokens$token %in% c("'{'", "'}'", "IF", "ELSE"))) {
                    next
                }
                has_inline_body <- TRUE
                inline_body <- format_line_tokens(body_tokens)
            }
        }

        # Add brace or inline body
        if (has_brace) {
            brace_tok <- terminals[close_idx + 1,]
            if (brace_style == "kr") {
                # K&R: brace on same line as closing paren
                new_lines[length(new_lines)] <- paste0(
                    new_lines[length(new_lines)], " {")
            } else {
                # Allman: brace on its own line
                new_lines <- c(new_lines, paste0(base_indent, "{"))
            }
            # Preserve body content after { on the same line
            brace_rest <- substring(lines[brace_tok$line1],
                                    col_to_charpos(lines[brace_tok$line1],
                    brace_tok$col2) + 1)
            if (nzchar(trimws(brace_rest))) {
                new_lines[length(new_lines)] <- paste0(
                    new_lines[length(new_lines)], brace_rest)
            }
        } else if (has_inline_body) {
            # Append inline body and any suffix (e.g., ", other_args)" in outer call)
            body_suffix <- ""
            if (body_end_idx <= nrow(terminals)) {
                last_body_line <- terminals$line1[body_end_idx]
                last_body_col <- terminals$col2[body_end_idx]
                rest_of_line <- substring(lines[last_body_line],
                    last_body_col + 1)
                if (nzchar(rest_of_line)) {
                    body_suffix <- rest_of_line
                }
            }
            new_lines[length(new_lines)] <- paste0(
                new_lines[length(new_lines)], " ", inline_body, body_suffix)
        }

        # Check if reformatting is actually needed
        end_line <- close_paren_line
        if (has_brace) {
            end_line <- brace_line
        } else if (has_inline_body) {
            end_line <- terminals$line1[body_end_idx]
        }

        old_lines <- lines[func_line:end_line]
        if (identical(old_lines, new_lines)) { next }

        # Replace the lines and return
        if (func_line > 1) {
            pre <- lines[seq_len(func_line - 1)]
        } else {
            pre <- character(0)
        }
        if (end_line < length(lines)) {
            post <- lines[seq(end_line + 1, length(lines))]
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

    # No function needed reformatting
    NULL
}

