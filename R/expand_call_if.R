#' Expand Bare If-Else Arguments in Function Calls
#'
#' When a function call has a bare `if (cond) expr else expr` argument
#' that doesn't fit on a continuation line, expand it to braced multi-line
#' form. Short if-else args stay inline.
#'
#' @param code Formatted code string.
#' @param line_limit Maximum line length (default 80).
#' @return Code with expanded if-else call arguments.
#' @keywords internal
expand_call_if_args <- function (code, line_limit = 80L) {
    changed <- TRUE
    max_iterations <- 100L

    while (changed && max_iterations > 0) {
        max_iterations <- max_iterations - 1
        changed <- FALSE

        result <- expand_one_call_if_arg(code, line_limit = line_limit)
        if (!is.null(result)) {
            code <- result
            changed <- TRUE
        }
    }

    code
}

#' Expand One Bare If-Else Argument in a Function Call
#'
#' Finds the first bare if-else argument inside a function call on a line
#' exceeding the line limit and expands it to braced multi-line form.
#'
#' @param code Code string.
#' @param line_limit Maximum line length (default 80).
#' @return Modified code or NULL if no changes.
#' @keywords internal
expand_one_call_if_arg <- function (code, line_limit = 80L) {
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

    lines <- strsplit(code, "\n", fixed = TRUE)[[1]]

    # Find IF tokens on overlong lines
    if_indices <- which(terminals$token == "IF")

    for (ii in if_indices) {
        if_line <- terminals$line1[ii]
        if (if_line > length(lines)) { next }
        if (code_width(lines[if_line]) <= line_limit) { next }

        # Track full nesting state before the IF token to match
        # format_tokens indentation logic
        paren_depth_at_if <- 0L
        bracket_depth_at_if <- 0L
        brace_depth_at_if <- 0L
        paren_at_brace <- integer(0)
        before_toks <- terminals[seq_len(ii - 1),]
        for (j in seq_len(nrow(before_toks))) {
            bt <- before_toks$token[j]
            if (bt == "'{'") {
                # Control-flow braces inherit enclosing pab (matching
                # compute_nesting); non-control braces capture paren_depth.
                is_ctrl <- FALSE
                if (j >= 2L) {
                    pt <- before_toks$token[j - 1L]
                    if (pt == "ELSE" || pt == "REPEAT") {
                        is_ctrl <- TRUE
                    } else if (pt == "')'") {
                        pd2 <- 1L
                        k2 <- j - 2L
                        while (k2 >= 1L && pd2 > 0L) {
                            if (before_toks$token[k2] == "')'") {
                                pd2 <- pd2 + 1L
                            }
                            if (before_toks$token[k2] == "'('") {
                                pd2 <- pd2 - 1L
                            }
                            if (pd2 > 0L) { k2 <- k2 - 1L }
                        }
                        if (k2 >= 2L &&
                            before_toks$token[k2 - 1L] %in% c("IF", "FOR",
                                "WHILE")) {
                            is_ctrl <- TRUE
                        }
                    }
                }
                if (is_ctrl) {
                    enc_pab <- if (length(paren_at_brace) > 0) {
                        paren_at_brace[length(paren_at_brace)]
                    } else {
                        0L
                    }
                    paren_at_brace <- c(paren_at_brace, enc_pab)
                } else {
                    paren_at_brace <- c(paren_at_brace,
                                        paren_depth_at_if + bracket_depth_at_if)
                }
                brace_depth_at_if <- brace_depth_at_if + 1L
            } else if (bt == "'}'") {
                brace_depth_at_if <- max(0L, brace_depth_at_if - 1L)
                if (length(paren_at_brace) > 0) {
                    paren_at_brace <- paren_at_brace[-length(paren_at_brace)]
                }
            } else if (bt == "'('") {
                paren_depth_at_if <- paren_depth_at_if + 1L
            } else if (bt == "')'") {
                paren_depth_at_if <- paren_depth_at_if - 1L
            } else if (bt == "'['") {
                bracket_depth_at_if <- bracket_depth_at_if + 1L
            } else if (bt == "']'") {
                bracket_depth_at_if <- bracket_depth_at_if - 1L
            } else if (bt == "LBB") {
                bracket_depth_at_if <- bracket_depth_at_if + 2L
            } else if (bt == "']]'") {
                bracket_depth_at_if <- bracket_depth_at_if - 2L
            }
        }
        if (paren_depth_at_if < 1L) { next }

        # Skip if-else inside function formal parens — reformat_one_function
        # owns signature layout and would collapse the expansion back,
        # causing an expand/collapse oscillation.
        in_formals <- FALSE
        scan_depth <- 0L
        for (j in rev(seq_len(nrow(before_toks)))) {
            bt <- before_toks$token[j]
            if (bt == "')'") {
                scan_depth <- scan_depth + 1L
            } else if (bt == "'('") {
                if (scan_depth == 0L) {
                    # Found the innermost enclosing open paren
                    if (j > 1L && before_toks$token[j - 1L] == "FUNCTION") {
                        in_formals <- TRUE
                    }
                    break
                }
                scan_depth <- scan_depth - 1L
            }
        }
        if (in_formals) { next }

        # Verify it's a bare if-else (no '{' after condition close-paren)
        open_paren_idx <- ii + 1
        if (open_paren_idx > nrow(terminals)) { next }
        if (terminals$token[open_paren_idx] != "'('") { next }

        # Find matching closing paren for condition
        pd_depth <- 1L
        close_paren_idx <- open_paren_idx + 1
        while (close_paren_idx <= nrow(terminals) && pd_depth > 0) {
            if (terminals$token[close_paren_idx] == "'('") {
                pd_depth <- pd_depth + 1L
            } else if (terminals$token[close_paren_idx] == "')'") {
                pd_depth <- pd_depth - 1L
            }
            if (pd_depth > 0) { close_paren_idx <- close_paren_idx + 1 }
        }
        if (close_paren_idx > nrow(terminals)) { next }

        # Check body starts without brace (bare if-else)
        body_first_idx <- close_paren_idx + 1
        if (body_first_idx > nrow(terminals)) { next }
        if (terminals$token[body_first_idx] == "'{'") { next }

        # Find matching ELSE using nest/brace tracking
        else_idx <- NULL
        search_idx <- close_paren_idx + 1
        nest_depth <- 0L
        brace_depth <- 0L

        while (search_idx <= nrow(terminals)) {
            stok <- terminals[search_idx,]
            if (stok$token == "'{'") {
                brace_depth <- brace_depth + 1L
            } else if (stok$token == "'}'") {
                brace_depth <- brace_depth - 1L
                if (brace_depth < 0L) { break }
            }
            if (stok$token == "IF") {
                nest_depth <- nest_depth + 1L
            } else if (stok$token == "ELSE") {
                if (nest_depth == 0L && brace_depth == 0L) {
                    else_idx <- search_idx
                    break
                } else if (nest_depth > 0L) {
                    nest_depth <- nest_depth - 1L
                }
            }
            search_idx <- search_idx + 1
        }

        if (is.null(else_idx)) { next }

        # Extract condition, true_expr, false_expr tokens
        cond_tokens <- terminals[(open_paren_idx + 1):(close_paren_idx - 1),]
        true_tokens <- terminals[(close_paren_idx + 1):(else_idx - 1),]
        false_start <- else_idx + 1
        if (false_start > nrow(terminals)) { next }

        # Skip if false body starts with brace
        if (terminals$token[false_start] == "'{'") { next }

        # Find end of false expression
        false_end <- false_start
        false_paren_depth <- 0L
        false_brace_depth <- 0L
        false_if_depth <- 0L
        false_start_line <- terminals$line1[false_start]

        while (false_end <= nrow(terminals)) {
            ftok <- terminals[false_end,]

            if (ftok$token == "IF" && ftok$line1 == false_start_line) {
                false_if_depth <- false_if_depth + 1L
            }
            if (ftok$token == "ELSE" && ftok$line1 == false_start_line) {
                false_if_depth <- max(0L, false_if_depth - 1L)
            }

            prev_paren_depth <- false_paren_depth
            if (ftok$token %in% c("'('", "'['")) {
                false_paren_depth <- false_paren_depth + 1L
            }
            if (ftok$token == "LBB") {
                false_paren_depth <- false_paren_depth + 2L
            }
            if (ftok$token == "','") {
                # Comma at depth 0 = next argument of enclosing call
                if (false_paren_depth == 0L && false_brace_depth == 0L &&
                    false_if_depth == 0L) {
                    false_end <- false_end - 1
                    break
                }
            }
            if (ftok$token %in% c("')'", "']'")) {
                # Stop if we close a paren that was open before the if-else
                if (false_paren_depth == 0L && false_brace_depth == 0L &&
                    false_if_depth == 0L) {
                    false_end <- false_end - 1
                    break
                }
                false_paren_depth <- false_paren_depth - 1L
            }
            if (ftok$token == "']]'") {
                false_paren_depth <- false_paren_depth - 2L
            }
            if (ftok$token == "'{'") {
                false_brace_depth <- false_brace_depth + 1L
            }
            if (ftok$token == "'}'") {
                false_brace_depth <- false_brace_depth - 1L
            }

            if (prev_paren_depth > 0L && false_paren_depth == 0L &&
                false_brace_depth == 0L && false_if_depth == 0L) {
                # Don't break if next token is [ or [[ (indexing the result);
                # let the loop continue to process the [ normally so
                # false_paren_depth gets incremented.
                next_ok <- false_end + 1 <= nrow(terminals) &&
                terminals$token[false_end + 1] %in% c("'['", "LBB")
                if (!next_ok) { break }
            }

            if (ftok$line1 > false_start_line && false_paren_depth <= 0L &&
                false_brace_depth <= 0L && false_if_depth == 0L) {
                false_end <- false_end - 1
                break
            }

            false_end <- false_end + 1
        }

        if (false_end > nrow(terminals)) {
            false_end <- nrow(terminals)
        }
        if (false_end < false_start) { next }

        false_tokens <- terminals[false_start:false_end,]

        # Skip if branches are complex (FUNCTION, COMMENT, or multi-line tokens)
        if (any(true_tokens$token == "FUNCTION") ||
            any(false_tokens$token == "FUNCTION") ||
            any(true_tokens$token == "COMMENT") ||
            any(false_tokens$token == "COMMENT") ||
            any(true_tokens$line2 > true_tokens$line1) ||
            any(false_tokens$line2 > false_tokens$line1)) {
            next
        }

        # Skip if the if-else spans multiple source lines already
        if_end_line <- max(false_tokens$line2)
        if (if_end_line > if_line) { next }

        # Format the parts
        cond_text <- format_line_tokens(cond_tokens)
        true_text <- format_line_tokens(true_tokens)
        false_text <- format_line_tokens(false_tokens)

        # Build the line prefix (everything before the IF token)
        if_col <- terminals$col1[ii]
        line_content <- lines[if_line]
        if (if_col > 1) {
            char_pos <- col_to_charpos(line_content, if_col - 1)
            before_if <- substring(line_content, 1, char_pos)
        } else {
            before_if <- ""
        }

        # Find the suffix (tokens after the false expression on the same line)
        # These are closing parens, commas, etc. from the enclosing call
        last_false_col <- terminals$col2[false_end]
        char_pos_end <- col_to_charpos(line_content, last_false_col)
        if (char_pos_end < nchar(line_content)) {
            suffix <- substring(line_content, char_pos_end + 1)
        } else {
            suffix <- ""
        }

        # We need to find the comma before the if-else to break the line there.
        # Look backwards from the IF for a comma at depth 0 relative to the
        # enclosing call.
        # First, find where to break: right before the IF token.
        # The before_if string already has everything before IF on this line.
        # We want to break at the last comma in before_if (if any),
        # putting the if-else on a new continuation line.

        # Calculate indentation matching format_tokens depth logic:
        # format_tokens uses: brace_depth + max(0, paren_depth - paren_at_brace)
        pab <- if (length(paren_at_brace) > 0) {
            paren_at_brace[length(paren_at_brace)]
        } else {
            0L
        }
        total_paren <- paren_depth_at_if + bracket_depth_at_if
        # if/} else {/}) all get the same level: the call continuation indent
        if_level <- brace_depth_at_if + max(0L, total_paren - pab)
        if_indent <- strrep("    ", if_level)
        # Body inside control-flow braces: pab inherits enclosing scope's pab
        # (not paren_depth), so paren contribution is preserved:
        # body_level = (brace_depth + 1) + max(0, total_paren - enclosing_pab)
        body_level <- brace_depth_at_if + 1L + max(0L, total_paren - pab)
        body_indent <- strrep("    ", body_level)

        # Find the last comma before the if in before_if
        # We trim trailing space from before_if to find where to break
        before_trimmed <- sub("\\s+$", "", before_if)

        # Check if before_if ends with a comma (after trimming)
        # indicating this if-else is a separate argument
        has_comma_before <- grepl(",\\s*$", before_if)

        if (has_comma_before) {
            # Break at the comma: keep everything up to and including comma
            # on the first line, put if-else on next line
            first_line <- sub(",\\s*$", ",", before_if)

            new_lines <- c(first_line,
                           paste0(if_indent, "if (", cond_text, ") {"),
                           paste0(body_indent, true_text),
                           paste0(if_indent, "} else {"),
                           paste0(body_indent, false_text),
                           paste0(if_indent, "}", suffix))
        } else {
            # No comma before — the if-else is the first argument or
            # the only argument. Put it on the same line prefix.
            new_lines <- c(paste0(before_if, "if (", cond_text, ") {"),
                           paste0(body_indent, true_text),
                           paste0(if_indent, "} else {"),
                           paste0(body_indent, false_text),
                           paste0(if_indent, "}", suffix))
        }

        # Replace the line
        if (if_line > 1) {
            pre <- lines[seq_len(if_line - 1)]
        } else {
            pre <- character(0)
        }
        if (if_line < length(lines)) {
            post <- lines[seq(if_line + 1, length(lines))]
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

