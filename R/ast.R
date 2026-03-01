#' Enrich Terminal Tokens for AST-Based Formatting
#'
#' Parses code and returns an enriched terminal-token DataFrame with
#' per-token nesting state and output metadata. This is the foundation
#' of the parse-once architecture: parse once, enrich once, transform
#' the DataFrame through all passes, serialize to text once at the end.
#'
#' @param pd Parse data from `getParseData()`.
#' @param orig_lines Original source lines (split by newline).
#' @return Enriched terminal-token DataFrame with added columns:
#'   `out_line`, `out_order`, `out_text`, `brace_depth`, `paren_depth`,
#'   `pab`, `nesting_level`.
#' @keywords internal
enrich_terminals <- function (pd, orig_lines) {
    terms <- pd[pd$terminal, ]
    terms <- terms[order(terms$line1, terms$col1), ]
    terms <- restore_truncated_str_const_tokens(terms, orig_lines)

    n <- nrow(terms)
    if (n == 0L) return(terms)

    # Per-token nesting state (state BEFORE processing this token)
    terms$brace_depth <- integer(n)
    terms$paren_depth <- integer(n)
    terms$pab <- integer(n)

    # Output metadata
    terms$out_line <- terms$line1
    terms$out_order <- seq_len(n)
    terms$out_text <- terms$text
    # Convert = assignment to <-
    eq_idx <- which(terms$token == "EQ_ASSIGN")
    if (length(eq_idx) > 0L) terms$out_text[eq_idx] <- "<-"

    # Walk tokens and compute nesting
    brace_depth <- 0L
    paren_depth <- 0L
    pab_stack <- integer(0)

    for (i in seq_len(n)) {
        # Record state BEFORE this token
        cur_pab <- if (length(pab_stack) > 0L) {
            pab_stack[length(pab_stack)]
        } else {
            0L
        }
        terms$brace_depth[i] <- brace_depth
        terms$paren_depth[i] <- paren_depth
        terms$pab[i] <- cur_pab

        # Update state AFTER this token
        tok <- terms$token[i]
        if (tok == "'{'") {
            is_ctrl <- FALSE
            if (i >= 2L) {
                pt <- terms$token[i - 1L]
                if (pt == "ELSE" || pt == "REPEAT") {
                    is_ctrl <- TRUE
                } else if (pt == "')'") {
                    pd2 <- 1L
                    k <- i - 2L
                    while (k >= 1L && pd2 > 0L) {
                        if (terms$token[k] == "')'") pd2 <- pd2 + 1L
                        if (terms$token[k] == "'('") pd2 <- pd2 - 1L
                        if (pd2 > 0L) k <- k - 1L
                    }
                    if (k >= 2L &&
                        terms$token[k - 1L] %in% c("IF", "FOR", "WHILE")) {
                        is_ctrl <- TRUE
                    }
                }
            }
            if (is_ctrl) {
                enc_pab <- if (length(pab_stack) > 0L) {
                    pab_stack[length(pab_stack)]
                } else {
                    0L
                }
                pab_stack <- c(pab_stack, enc_pab)
            } else {
                pab_stack <- c(pab_stack, paren_depth)
            }
            brace_depth <- brace_depth + 1L
        } else if (tok == "'}'") {
            brace_depth <- max(0L, brace_depth - 1L)
            if (length(pab_stack) > 0L) {
                pab_stack <- pab_stack[-length(pab_stack)]
            }
        } else if (tok %in% c("'('", "'['")) {
            paren_depth <- paren_depth + 1L
        } else if (tok == "LBB") {
            paren_depth <- paren_depth + 2L
        } else if (tok %in% c("')'", "']'")) {
            paren_depth <- max(0L, paren_depth - 1L)
        } else if (tok == "']]'") {
            paren_depth <- max(0L, paren_depth - 2L)
        }
    }

    # Compute nesting level for each token
    terms$nesting_level <- terms$brace_depth +
        pmax(0L, terms$paren_depth - terms$pab)

    terms
}

#' Recompute Nesting State After Structural Changes
#'
#' Re-walks terminals and refreshes `brace_depth`, `paren_depth`, `pab`,
#' and `nesting_level` columns. Call after brace insertion, token removal,
#' or any structural transform.
#'
#' @param terms Enriched terminal DataFrame.
#' @return Updated DataFrame with refreshed nesting columns.
#' @keywords internal
recompute_nesting <- function (terms) {
    n <- nrow(terms)
    if (n == 0L) return(terms)

    # Re-sort by output position
    terms <- terms[order(terms$out_line, terms$out_order), ]

    brace_depth <- 0L
    paren_depth <- 0L
    pab_stack <- integer(0)

    for (i in seq_len(n)) {
        cur_pab <- if (length(pab_stack) > 0L) {
            pab_stack[length(pab_stack)]
        } else {
            0L
        }
        terms$brace_depth[i] <- brace_depth
        terms$paren_depth[i] <- paren_depth
        terms$pab[i] <- cur_pab

        tok <- terms$token[i]
        if (tok == "'{'") {
            is_ctrl <- FALSE
            if (i >= 2L) {
                pt <- terms$token[i - 1L]
                if (pt == "ELSE" || pt == "REPEAT") {
                    is_ctrl <- TRUE
                } else if (pt == "')'") {
                    pd2 <- 1L
                    k <- i - 2L
                    while (k >= 1L && pd2 > 0L) {
                        if (terms$token[k] == "')'") pd2 <- pd2 + 1L
                        if (terms$token[k] == "'('") pd2 <- pd2 - 1L
                        if (pd2 > 0L) k <- k - 1L
                    }
                    if (k >= 2L &&
                        terms$token[k - 1L] %in% c("IF", "FOR", "WHILE")) {
                        is_ctrl <- TRUE
                    }
                }
            }
            if (is_ctrl) {
                enc_pab <- if (length(pab_stack) > 0L) {
                    pab_stack[length(pab_stack)]
                } else {
                    0L
                }
                pab_stack <- c(pab_stack, enc_pab)
            } else {
                pab_stack <- c(pab_stack, paren_depth)
            }
            brace_depth <- brace_depth + 1L
        } else if (tok == "'}'") {
            brace_depth <- max(0L, brace_depth - 1L)
            if (length(pab_stack) > 0L) {
                pab_stack <- pab_stack[-length(pab_stack)]
            }
        } else if (tok %in% c("'('", "'['")) {
            paren_depth <- paren_depth + 1L
        } else if (tok == "LBB") {
            paren_depth <- paren_depth + 2L
        } else if (tok %in% c("')'", "']'")) {
            paren_depth <- max(0L, paren_depth - 1L)
        } else if (tok == "']]'") {
            paren_depth <- max(0L, paren_depth - 2L)
        }
    }

    terms$nesting_level <- terms$brace_depth +
        pmax(0L, terms$paren_depth - terms$pab)
    terms
}

#' Compute Indent Level for a Token
#'
#' Returns the depth-based indent level that should apply to a token's line.
#' For closing tokens (`}`, `)`, `]`), the indent is one less than the token's
#' own nesting level (they outdent to match their opening counterpart).
#'
#' @param terms Enriched terminal DataFrame.
#' @param idx Index of the token (must be first on its line for indent).
#' @return Integer indent level.
#' @keywords internal
token_indent_level <- function (terms, idx) {
    tok <- terms$token[idx]
    level <- terms$nesting_level[idx]
    # Closing tokens outdent
    if (tok %in% c("'}'", "')'", "']'", "']]'")) {
        level <- max(0L, level - if (tok == "']]'") 2L else 1L)
    }
    level
}

#' Compute Display Width of an Output Line
#'
#' Sums token text widths plus inter-token spaces for a given output line.
#'
#' @param terms Enriched terminal DataFrame (sorted by out_line, out_order).
#' @param line_num The output line number.
#' @param indent_str Indent string (e.g., `"    "` for 4 spaces).
#' @return Display width of the line.
#' @keywords internal
ast_line_width <- function (terms, line_num, indent_str) {
    idx <- which(terms$out_line == line_num)
    if (length(idx) == 0L) return(0L)

    line_toks <- terms[idx, ]
    # Indent for first token
    first_level <- token_indent_level(terms, idx[1])
    prefix_width <- nchar(indent_str) * first_level

    # Token widths + spaces
    width <- prefix_width
    prev <- NULL
    prev_prev <- NULL
    for (i in seq_len(nrow(line_toks))) {
        tok <- line_toks[i, ]
        if (!is.null(prev) && needs_space(prev, tok, prev_prev)) {
            width <- width + 1L
        }
        width <- width + nchar(tok$out_text)
        prev_prev <- prev
        prev <- tok
    }
    width
}

#' Serialize Enriched Tokens to Formatted Code
#'
#' Converts the enriched terminal DataFrame to a formatted code string.
#' This is the final step: tokens are emitted in `(out_line, out_order)`
#' order with proper indentation and spacing.
#'
#' @param terms Enriched terminal DataFrame.
#' @param indent_str Indent string (e.g., `"    "` for 4 spaces).
#' @param wrap Continuation style: `"paren"` or `"fixed"`.
#' @param line_limit Maximum line length.
#' @return Formatted code string.
#' @keywords internal
serialize_tokens <- function (terms, indent_str, wrap = "paren",
                              line_limit = 80L) {
    if (nrow(terms) == 0L) return("\n")

    # Sort by output position
    terms <- terms[order(terms$out_line, terms$out_order), ]

    # Group tokens by out_line
    lines_out <- split(seq_len(nrow(terms)), terms$out_line)
    line_nums <- as.integer(names(lines_out))

    # Track call-paren stack for paren-aligned continuation
    call_paren_stack <- integer(0)  # output column of each open call (
    funcdef_paren_stack <- logical(0)  # TRUE if paren is from FUNCTION def
    brace_at_call <- integer(0)     # brace_depth when each call ( opened
    cur_brace_depth <- 0L

    result_lines <- list()
    prev_line_num <- 0L

    for (li in seq_along(line_nums)) {
        ln <- line_nums[li]
        idx <- lines_out[[li]]
        line_toks <- terms[idx, ]

        # Insert blank lines for gaps in out_line numbering
        if (prev_line_num > 0L && ln > prev_line_num + 1L) {
            for (gap in seq_len(ln - prev_line_num - 1L)) {
                result_lines[[length(result_lines) + 1L]] <- ""
            }
        }
        prev_line_num <- ln

        # Compute indent
        first_level <- token_indent_level(terms, idx[1])
        depth_prefix <- strrep(indent_str, first_level)

        # Paren-aligned continuation
        line_prefix <- depth_prefix
        has_active_paren <- length(call_paren_stack) > 0L &&
            any(call_paren_stack > 0L)
        if (has_active_paren && (wrap == "paren" ||
            any(funcdef_paren_stack))) {
            inside_brace <- length(brace_at_call) > 0L &&
                cur_brace_depth > brace_at_call[length(brace_at_call)]
            if (!inside_brace) {
                open_cols <- call_paren_stack[call_paren_stack > 0L]
                if (length(open_cols) > 0L) {
                    first_tok <- line_toks$token[1]
                    if (!first_tok %in% c("')'", "']'", "']]'", "IF", "FOR",
                                          "WHILE", "REPEAT", "ELSE", "'}'")) {
                        paren_col <- open_cols[length(open_cols)]
                        if (paren_col <= line_limit %/% 2L) {
                            paren_prefix <- strrep(" ", paren_col)
                            if (nchar(paren_prefix) > nchar(depth_prefix)) {
                                line_prefix <- paren_prefix
                            }
                        }
                    }
                }
            }
        }

        # Build line content
        parts <- character(nrow(line_toks))
        prev <- NULL
        prev_prev <- NULL
        for (i in seq_len(nrow(line_toks))) {
            tok <- line_toks[i, ]
            if (!is.null(prev) && needs_space(prev, tok, prev_prev)) {
                parts[i] <- paste0(" ", tok$out_text)
            } else {
                parts[i] <- tok$out_text
            }
            prev_prev <- prev
            prev <- tok
        }
        content <- paste(parts, collapse = "")
        full_line <- trimws(paste0(line_prefix, content), "right")
        result_lines[[length(result_lines) + 1L]] <- full_line

        # Account for multi-line tokens (e.g., multi-line strings)
        # that consume extra output lines beyond the out_line number
        extra_newlines <- sum(nchar(gsub("[^\n]", "", line_toks$out_text)))
        if (extra_newlines > 0L)
            prev_line_num <- prev_line_num + extra_newlines

        # Update call-paren stack
        prefix_len <- nchar(line_prefix)
        pos <- prefix_len + 1L  # 1-based position in output
        prev <- NULL
        prev_prev <- NULL
        for (i in seq_len(nrow(line_toks))) {
            tok <- line_toks[i, ]
            if (!is.null(prev) && needs_space(prev, tok, prev_prev)) {
                pos <- pos + 1L
            }
            tt <- tok$token
            if (tt == "'('") {
                is_call <- FALSE
                is_funcdef <- FALSE
                if (i > 1L) {
                    ptok <- line_toks$token[i - 1L]
                    is_call <- ptok == "SYMBOL_FUNCTION_CALL"
                    is_funcdef <- ptok == "FUNCTION"
                }
                if (!is_call && !is_funcdef && i == 1L && li > 1L) {
                    prev_idx <- lines_out[[li - 1L]]
                    if (length(prev_idx) > 0L) {
                        ptok <- terms$token[prev_idx[length(prev_idx)]]
                        is_call <- ptok == "SYMBOL_FUNCTION_CALL"
                        is_funcdef <- ptok == "FUNCTION"
                    }
                }
                paren_pos <- if (is_call) {
                    pos
                } else if (is_funcdef) {
                    # Funcdef: paren-align in paren mode, 8-space in fixed
                    if (wrap == "fixed") 8L else pos
                } else {
                    0L
                }
                call_paren_stack <- c(call_paren_stack, paren_pos)
                funcdef_paren_stack <- c(funcdef_paren_stack, is_funcdef)
                brace_at_call <- c(brace_at_call, cur_brace_depth)
            } else if (tt == "')'") {
                if (length(call_paren_stack) > 0L) {
                    call_paren_stack <-
                        call_paren_stack[-length(call_paren_stack)]
                    funcdef_paren_stack <-
                        funcdef_paren_stack[-length(funcdef_paren_stack)]
                    brace_at_call <-
                        brace_at_call[-length(brace_at_call)]
                }
            } else if (tt == "'{'") {
                cur_brace_depth <- cur_brace_depth + 1L
            } else if (tt == "'}'") {
                cur_brace_depth <- max(0L, cur_brace_depth - 1L)
            }
            pos <- pos + nchar(tok$out_text)
            prev_prev <- prev
            prev <- tok
        }
    }

    result <- paste(unlist(result_lines), collapse = "\n")
    if (!grepl("\n$", result) && nchar(result) > 0L) {
        result <- paste0(result, "\n")
    }
    result
}

#' Insert Synthetic Tokens into the DataFrame
#'
#' Adds new token rows (e.g., for brace insertion). New tokens get unique IDs
#' starting from `max(existing_id) + 1`.
#'
#' @param terms Enriched terminal DataFrame.
#' @param new_rows Data frame of new tokens to insert. Must have at minimum:
#'   `token`, `out_text`, `out_line`, `out_order`. Other columns will be
#'   filled with defaults.
#' @return Updated DataFrame with new rows appended.
#' @keywords internal
insert_tokens <- function (terms, new_rows) {
    if (nrow(new_rows) == 0L) return(terms)

    # Assign unique IDs
    max_id <- max(as.integer(rownames(terms)))
    new_ids <- seq(max_id + 1L, max_id + nrow(new_rows))

    # Fill missing columns with defaults
    for (col in names(terms)) {
        if (!col %in% names(new_rows)) {
            new_rows[[col]] <- if (is.integer(terms[[col]])) {
                0L
            } else if (is.logical(terms[[col]])) {
                FALSE
            } else if (is.character(terms[[col]])) {
                ""
            } else {
                NA
            }
        }
    }
    new_rows$terminal <- TRUE
    new_rows$text <- new_rows$out_text
    rownames(new_rows) <- as.character(new_ids)

    # Only keep columns that exist in terms
    new_rows <- new_rows[, names(terms), drop = FALSE]

    rbind(terms, new_rows)
}

#' Create a Synthetic Token Row
#'
#' Helper to build a single token row for insertion.
#'
#' @param token Token type string (e.g., `"'{'"`, `"'}'"`, `"','"`)
#' @param text Token text (e.g., `"{"`, `"}"`, `","`)
#' @param out_line Target output line.
#' @param out_order Sort order within the line.
#' @param parent Parent node ID (default 0).
#' @return Single-row data frame.
#' @keywords internal
make_token <- function (token, text, out_line, out_order, parent = 0L) {
    data.frame(
        token = token,
        out_text = text,
        text = text,
        out_line = out_line,
        out_order = out_order,
        line1 = 0L, col1 = 0L, line2 = 0L, col2 = 0L,
        terminal = TRUE,
        parent = parent,
        id = 0L,
        stringsAsFactors = FALSE
    )
}

#' Renumber Lines to Remove Gaps
#'
#' Compacts `out_line` values so there are no empty-line gaps.
#' Lines are renumbered sequentially (1, 2, 3, ...) preserving their
#' relative order.
#'
#' @param terms Enriched terminal DataFrame.
#' @return Updated DataFrame with compacted line numbers.
#' @keywords internal
renumber_lines <- function (terms) {
    used <- sort(unique(terms$out_line))
    if (length(used) == 0L) return(terms)
    mapping <- match(terms$out_line, used)
    terms$out_line <- mapping
    terms
}

#' Collapse Multi-Line Calls (AST Version)
#'
#' Finds multi-line parenthesized groups (function calls, control flow
#' conditions) that would fit on one line and collapses them by setting
#' all tokens' `out_line` to the opening line.
#'
#' @param terms Enriched terminal DataFrame.
#' @param indent_str Indent string.
#' @param line_limit Maximum line length.
#' @return Updated DataFrame.
#' @keywords internal
collapse_calls <- function (terms, indent_str, line_limit = 80L) {
    changed <- TRUE
    max_iter <- 100L

    while (changed && max_iter > 0L) {
        max_iter <- max_iter - 1L
        changed <- FALSE

        terms <- terms[order(terms$out_line, terms$out_order), ]
        target <- c("SYMBOL_FUNCTION_CALL", "IF", "FOR", "WHILE")
        target_idx <- which(terms$token %in% target)

        for (ci in target_idx) {
            # Next token should be (
            open_idx <- ci + 1L
            if (open_idx > nrow(terms)) next
            if (terms$token[open_idx] != "'('") next

            open_line <- terms$out_line[open_idx]

            # Find matching )
            depth <- 1L
            close_idx <- open_idx + 1L
            while (close_idx <= nrow(terms) && depth > 0L) {
                if (terms$token[close_idx] == "'('") depth <- depth + 1L
                else if (terms$token[close_idx] == "')'") depth <- depth - 1L
                if (depth > 0L) close_idx <- close_idx + 1L
            }
            if (close_idx > nrow(terms)) next

            # Only multi-line groups
            close_line <- terms$out_line[close_idx]
            if (close_line == open_line) next

            # Skip if group contains comments or braces
            inner <- terms[seq(open_idx, close_idx), ]
            if (any(inner$token == "COMMENT")) next
            if (any(inner$token %in% c("'{'", "'}'"))) next

            # Collapse: move all call tokens + suffix tokens to open_line
            call_range <- seq(ci, close_idx)

            # Also identify suffix tokens (after ) on close line)
            # Only move closing brackets/parens, commas, operators, and
            # comments — not new expressions that happen to be on the
            # same line
            suffix_idx <- which(terms$out_line == close_line &
                terms$out_order > terms$out_order[close_idx])
            if (length(suffix_idx) > 0L) {
                suffix_toks <- terms$token[suffix_idx]
                # Determine which suffix tokens to keep.
                # Walk through and stop at first token that starts
                # a new, independent expression.
                n_suffix <- length(suffix_idx)
                keep_count <- 0L
                si <- 1L
                while (si <= n_suffix) {
                    st <- suffix_toks[si]
                    if (st %in% c("')'", "']'", "']]'", "','",
                        "';'", "'{'", "COMMENT", "'+'", "'-'",
                        "'*'", "'/'", "'?'", "PIPE", "SPECIAL",
                        "OR2", "AND2", "OR", "AND", "'~'", "EQ",
                        "NE", "GE", "LE", "GT", "LT",
                        "LEFT_ASSIGN", "EQ_ASSIGN",
                        "RIGHT_ASSIGN")) {
                        keep_count <- si
                        si <- si + 1L
                    } else if (st %in% c("'$'", "'@'")) {
                        # $ and @ need their field name too
                        keep_count <- min(si + 1L, n_suffix)
                        si <- si + 2L
                    } else if (st == "'('") {
                        # call on result — keep until matching )
                        call_depth <- 1L
                        si <- si + 1L
                        while (si <= n_suffix && call_depth > 0L) {
                            ist <- suffix_toks[si]
                            if (ist == "'('") call_depth <- call_depth + 1L
                            if (ist == "')'") call_depth <- call_depth - 1L
                            si <- si + 1L
                        }
                        keep_count <- si - 1L
                    } else if (st %in% c("'['", "LBB")) {
                        # indexing — keep until matching ]
                        idx_depth <- 1L
                        si <- si + 1L
                        while (si <= n_suffix && idx_depth > 0L) {
                            ist <- suffix_toks[si]
                            if (ist %in% c("'['", "LBB")) {
                                idx_depth <- idx_depth + 1L
                            }
                            if (ist == "']'") idx_depth <- idx_depth - 1L
                            if (ist == "']]'") idx_depth <- idx_depth - 2L
                            si <- si + 1L
                        }
                        keep_count <- si - 1L
                    } else {
                        break
                    }
                }
                if (keep_count < n_suffix)
                    suffix_idx <- suffix_idx[seq_len(keep_count)]
            }

            # Identify remaining tokens on close_line that won't
            # move (e.g. IF body after condition collapse)
            remaining_close <- which(terms$out_line == close_line &
                terms$out_order > terms$out_order[close_idx] &
                !seq_len(nrow(terms)) %in% suffix_idx)

            # If there are remaining tokens, check if everything
            # (call + suffix + remaining) would fit on one line.
            # If not, skip collapse to avoid oscillation.
            if (length(remaining_close) > 0L) {
                all_check <- c(call_range,
                    if (length(suffix_idx) > 0L) suffix_idx,
                    remaining_close)
                saved_check <- terms$out_line[all_check]
                terms$out_line[all_check] <- open_line
                too_wide <- ast_line_width(terms, open_line,
                    indent_str) > line_limit
                terms$out_line[all_check] <- saved_check
                if (too_wide) next
            }

            all_move <- c(call_range,
                if (length(suffix_idx) > 0L) suffix_idx,
                if (length(remaining_close) > 0L) remaining_close)
            saved_lines <- terms$out_line[all_move]
            terms$out_line[all_move] <- open_line

            # Check if collapsed line exceeds line limit.
            # Only skip when there are no depth-0 commas inside
            # the call — if commas exist, wrap_long_calls
            # can re-wrap after collapse.
            if (ast_line_width(terms, open_line,
                indent_str) > line_limit) {
                has_d0_comma <- FALSE
                d0 <- 0L
                for (ck in seq(open_idx + 1L, close_idx - 1L)) {
                    ct <- terms$token[ck]
                    if (ct %in% c("'('", "'['")) d0 <- d0 + 1L
                    else if (ct == "LBB") d0 <- d0 + 2L
                    else if (ct %in% c("')'", "']'")) d0 <- d0 - 1L
                    else if (ct == "']]'") d0 <- d0 - 2L
                    if (ct == "','" && d0 == 0L) {
                        has_d0_comma <- TRUE
                        break
                    }
                }
                if (!has_d0_comma) {
                    terms$out_line[all_move] <- saved_lines
                    next
                }
            }

            # If close_line still has tokens (e.g. IF body after
            # condition collapse), move them to open_line + 1
            remaining_on_close <- which(terms$out_line == close_line)
            if (length(remaining_on_close) > 0L) {
                terms$out_line[remaining_on_close] <- open_line + 1L
                lines_freed <- close_line - open_line - 1L
            } else {
                lines_freed <- close_line - open_line
            }
            if (lines_freed > 0L) {
                later <- terms$out_line > close_line
                terms$out_line[later] <-
                    terms$out_line[later] - lines_freed
            }

            # Re-sort and reassign out_order
            terms <- terms[order(terms$out_line, terms$out_order), ]
            terms$out_order <- seq_len(nrow(terms))
            changed <- TRUE
            break  # restart scan
        }
    }

    terms
}

#' Renumber Output Lines Sequentially
#'
#' After transforms that insert or remove lines, renumber `out_line` so
#' values are sequential starting from 1, preserving relative order and
#' gaps for blank lines.
#'
#' @param terms Enriched terminal DataFrame.
#' @return Updated DataFrame with renumbered `out_line`.
#' @keywords internal
renumber_lines <- function (terms) {
    terms <- terms[order(terms$out_line, terms$out_order), ]
    old_lines <- unique(terms$out_line)
    mapping <- setNames(seq_along(old_lines), as.character(old_lines))
    terms$out_line <- as.integer(mapping[as.character(terms$out_line)])
    terms
}

#' Wrap Long Lines at Operators (AST Version)
#'
#' Finds overlong lines and breaks them after logical operators (`||`, `&&`,
#' `|`, `&`). Continuation lines get depth-based indentation.
#'
#' @param terms Enriched terminal DataFrame.
#' @param indent_str Indent string.
#' @param line_limit Maximum line length.
#' @return Updated DataFrame.
#' @keywords internal
wrap_long_operators <- function (terms, indent_str, line_limit = 80L) {
    break_ops <- c("OR2", "AND2", "OR", "AND")
    changed <- TRUE
    max_iter <- 100L

    while (changed && max_iter > 0L) {
        max_iter <- max_iter - 1L
        changed <- FALSE

        terms <- terms[order(terms$out_line, terms$out_order), ]
        out_lines <- unique(terms$out_line)

        for (ln in out_lines) {
            width <- ast_line_width(terms, ln, indent_str)
            if (width <= line_limit) next

            idx <- which(terms$out_line == ln)
            line_toks <- terms[idx, ]

            # Skip semicolons
            if (any(line_toks$token == "';'")) next

            # Find paren depth at start of line
            start_paren <- if (nrow(line_toks) > 0L) {
                line_toks$paren_depth[1]
            } else {
                0L
            }

            # Find best break operator (last one within limit at depth <= start+1)
            best_break <- NULL
            pos <- nchar(indent_str) * token_indent_level(terms, idx[1])
            prev <- NULL
            prev_prev <- NULL
            cur_paren <- start_paren
            for (j in seq_len(nrow(line_toks))) {
                tok <- line_toks[j, ]
                if (!is.null(prev) && needs_space(prev, tok, prev_prev)) {
                    pos <- pos + 1L
                }
                if (tok$token %in% c("'('", "'['")) {
                    cur_paren <- cur_paren + 1L
                } else if (tok$token == "LBB") {
                    cur_paren <- cur_paren + 2L
                } else if (tok$token %in% c("')'", "']'")) {
                    cur_paren <- cur_paren - 1L
                } else if (tok$token == "']]'") {
                    cur_paren <- cur_paren - 2L
                }
                end_pos <- pos + nchar(tok$out_text)
                if (tok$token %in% break_ops &&
                    cur_paren <= start_paren + 1L &&
                    end_pos <= line_limit) {
                    best_break <- j
                }
                pos <- end_pos
                prev_prev <- prev
                prev <- tok
            }

            if (is.null(best_break)) next

            # Split: tokens after the break go to a new line
            break_at <- best_break
            cont_level <- line_toks$nesting_level[break_at]
            # For the continuation, use the nesting level at the break point
            new_line <- ln + 1L

            # Shift all later lines up by 1
            later <- terms$out_line > ln
            terms$out_line[later] <- terms$out_line[later] + 1L

            # Move tokens after break to new line
            move_idx <- idx[(break_at + 1L):length(idx)]
            terms$out_line[move_idx] <- new_line

            # Re-sort and fix order
            terms <- terms[order(terms$out_line, terms$out_order), ]
            terms$out_order <- seq_len(nrow(terms))
            changed <- TRUE
            break
        }
    }

    terms
}

#' Wrap Long Function Calls at Commas (AST Version)
#'
#' Finds single-line function calls on overlong lines and wraps them at
#' commas. Continuation lines get depth-based indentation (or paren-aligned
#' if `wrap = "paren"`).
#'
#' @param terms Enriched terminal DataFrame.
#' @param indent_str Indent string.
#' @param wrap Continuation style: `"paren"` or `"fixed"`.
#' @param line_limit Maximum line length.
#' @return Updated DataFrame.
#' @keywords internal
wrap_long_calls <- function (terms, indent_str, wrap = "paren",
                                 line_limit = 80L) {
    changed <- TRUE
    max_iter <- 100L

    while (changed && max_iter > 0L) {
        max_iter <- max_iter - 1L
        changed <- FALSE

        terms <- terms[order(terms$out_line, terms$out_order), ]
        call_idx <- which(terms$token == "SYMBOL_FUNCTION_CALL")

        for (ci in call_idx) {
            open_idx <- ci + 1L
            if (open_idx > nrow(terms)) next
            if (terms$token[open_idx] != "'('") next

            call_line <- terms$out_line[ci]

            # Only overlong lines
            if (ast_line_width(terms, call_line, indent_str) <= line_limit) next

            # Only single-line calls
            depth <- 1L
            close_idx <- open_idx + 1L
            while (close_idx <= nrow(terms) && depth > 0L) {
                if (terms$token[close_idx] == "'('") depth <- depth + 1L
                else if (terms$token[close_idx] == "')'") depth <- depth - 1L
                if (depth > 0L) close_idx <- close_idx + 1L
            }
            if (close_idx > nrow(terms)) next
            if (terms$out_line[close_idx] != call_line) next

            # Skip calls with braces
            inner <- terms[seq(open_idx, close_idx), ]
            if (any(inner$token %in% c("'{'", "'}'"))) next

            # Skip semicolons on this line
            line_idx <- which(terms$out_line == call_line)
            line_toks <- terms[line_idx, ]
            if (any(line_toks$token == "';'")) next

            # Skip inner calls when outer call is wrappable
            func_order <- terms$out_order[ci]
            before <- line_toks[line_toks$out_order < func_order, ]
            pd_before <- sum(before$token == "'('") -
                sum(before$token == "')'")
            if (pd_before > 0L) {
                # Check if an outer call on this line has unwrapped commas
                outer_calls <- which(line_toks$token == "SYMBOL_FUNCTION_CALL" &
                    line_toks$out_order < func_order)
                skip <- FALSE
                for (oc in outer_calls) {
                    oc_row <- which(terms$out_order == line_toks$out_order[oc] &
                        terms$out_line == call_line)
                    if (length(oc_row) == 0L) next
                    oc_open <- oc_row + 1L
                    if (oc_open > nrow(terms)) next
                    if (terms$token[oc_open] != "'('") next
                    # Find matching )
                    od <- 1L
                    oc_close <- oc_open + 1L
                    while (oc_close <= nrow(terms) && od > 0L) {
                        if (terms$token[oc_close] == "'('") od <- od + 1L
                        if (terms$token[oc_close] == "')'") od <- od - 1L
                        if (od > 0L) oc_close <- oc_close + 1L
                    }
                    if (oc_close > nrow(terms)) next
                    if (terms$out_line[oc_close] != call_line) next
                    # Check for comma at depth 1
                    d2 <- 0L
                    for (ki in seq(oc_open + 1L, oc_close - 1L)) {
                        tt <- terms$token[ki]
                        if (tt == "'('") d2 <- d2 + 1L
                        if (tt == "')'") d2 <- d2 - 1L
                        if (tt == "','" && d2 == 0L) { skip <- TRUE; break }
                    }
                    if (skip) break
                }
                if (skip) next
            }

            # Need at least one depth-0 comma
            has_comma <- FALSE
            d2 <- 0L
            for (k in seq(open_idx + 1L, close_idx - 1L)) {
                tt <- terms$token[k]
                if (tt %in% c("'('", "'['")) d2 <- d2 + 1L
                if (tt == "LBB") d2 <- d2 + 2L
                if (tt %in% c("')'", "']'")) d2 <- d2 - 1L
                if (tt == "']]'") d2 <- d2 - 2L
                if (tt == "','" && d2 == 0L) { has_comma <- TRUE; break }
            }
            if (!has_comma) next

            # Collect argument groups (ranges of indices between depth-0 commas)
            # Empty args (consecutive commas) get empty integer(0) groups.
            arg_groups <- list()
            comma_indices <- integer(0)
            current_start <- open_idx + 1L
            d2 <- 0L
            for (k in seq(open_idx + 1L, close_idx - 1L)) {
                tt <- terms$token[k]
                if (tt %in% c("'('", "'['")) d2 <- d2 + 1L
                if (tt == "LBB") d2 <- d2 + 2L
                if (tt %in% c("')'", "']'")) d2 <- d2 - 1L
                if (tt == "']]'") d2 <- d2 - 2L
                if (tt == "','" && d2 == 0L) {
                    if (current_start <= k - 1L) {
                        arg_groups[[length(arg_groups) + 1L]] <-
                            seq(current_start, k - 1L)
                    } else {
                        # Empty arg (consecutive commas)
                        arg_groups[[length(arg_groups) + 1L]] <-
                            integer(0)
                    }
                    comma_indices <- c(comma_indices, k)
                    current_start <- k + 1L
                }
            }
            # Last arg
            if (current_start <= close_idx - 1L) {
                arg_groups[[length(arg_groups) + 1L]] <-
                    seq(current_start, close_idx - 1L)
            } else if (length(comma_indices) > 0L) {
                # Trailing empty arg
                arg_groups[[length(arg_groups) + 1L]] <- integer(0)
            }
            if (length(arg_groups) < 2L) next

            # Compute continuation indent
            indent_size <- nchar(indent_str)
            cont_level <- terms$nesting_level[open_idx] + 1L
            cont_width <- cont_level * indent_size

            if (wrap == "paren") {
                # Compute paren column = prefix width + func_name + "("
                prefix_idx <- which(terms$out_line == call_line &
                    terms$out_order < terms$out_order[ci])
                prefix_w <- nchar(indent_str) *
                    token_indent_level(terms, line_idx[1])
                prev <- NULL
                prev_prev <- NULL
                for (pi in prefix_idx) {
                    ptok <- terms[pi, ]
                    if (!is.null(prev) &&
                        needs_space(prev, ptok, prev_prev)) {
                        prefix_w <- prefix_w + 1L
                    }
                    prefix_w <- prefix_w + nchar(ptok$out_text)
                    prev_prev <- prev
                    prev <- ptok
                }
                # Add func name + (
                if (!is.null(prev) &&
                    needs_space(prev, terms[ci, ], prev_prev)) {
                    prefix_w <- prefix_w + 1L
                }
                paren_col <- prefix_w + nchar(terms$out_text[ci]) + 1L
                if (paren_col <= line_limit %/% 2L) {
                    cont_width <- paren_col
                }
            }

            # Greedy packing: put args on lines until they exceed limit
            # First line: prefix + func( + first args
            # Measure first-line width up to "("
            first_line_w <- 0L
            prev <- NULL
            prev_prev <- NULL
            for (fi in line_idx[line_idx <= open_idx]) {
                tok <- terms[fi, ]
                if (fi == line_idx[1]) {
                    first_line_w <- nchar(indent_str) *
                        token_indent_level(terms, fi)
                }
                if (!is.null(prev) && needs_space(prev, tok, prev_prev)) {
                    first_line_w <- first_line_w + 1L
                }
                first_line_w <- first_line_w + nchar(tok$out_text)
                prev_prev <- prev
                prev <- tok
            }

            # Two-pass approach: compute line offsets, then apply

            # Pass 1: compute arg_widths and line offsets
            arg_widths <- integer(length(arg_groups))
            for (ai in seq_along(arg_groups)) {
                arg_idx <- arg_groups[[ai]]
                aw <- 0L
                aprev <- NULL
                aprev_prev <- NULL
                for (aidx in arg_idx) {
                    atok <- terms[aidx, ]
                    if (!is.null(aprev) &&
                        needs_space(aprev, atok, aprev_prev)) {
                        aw <- aw + 1L
                    }
                    aw <- aw + nchar(atok$out_text)
                    aprev_prev <- aprev
                    aprev <- atok
                }
                arg_widths[ai] <- aw
            }

            current_w <- first_line_w
            lines_inserted <- 0L
            first_on_line <- TRUE
            arg_line_offset <- integer(length(arg_groups))

            for (ai in seq_along(arg_groups)) {
                aw <- arg_widths[ai]
                extra <- if (ai < length(arg_groups)) 2L else 1L
                space_w <- if (!first_on_line) 1L else 0L
                test_w <- current_w + space_w + aw + extra

                if (test_w > line_limit && !first_on_line) {
                    lines_inserted <- lines_inserted + 1L
                    arg_line_offset[ai] <- lines_inserted
                    current_w <- cont_width + aw + extra
                    # If even the new continuation line is over limit,
                    # force-break the next arg too
                    first_on_line <- current_w > line_limit
                } else {
                    arg_line_offset[ai] <- lines_inserted
                    current_w <- test_w
                    first_on_line <- FALSE
                }
            }

            if (lines_inserted == 0L) next

            # Pass 2: apply line assignments
            # Collect all call-internal token indices
            all_call_idx <- c(unlist(arg_groups), comma_indices, close_idx)

            # Shift everything after call_line down by lines_inserted
            later <- terms$out_line > call_line &
                !(seq_len(nrow(terms)) %in% all_call_idx)
            terms$out_line[later] <- terms$out_line[later] + lines_inserted

            # Place args and commas
            for (ai in seq_along(arg_groups)) {
                target <- call_line + arg_line_offset[ai]
                terms$out_line[arg_groups[[ai]]] <- target
                if (ai < length(arg_groups)) {
                    terms$out_line[comma_indices[ai]] <- target
                }
            }

            # Place ) on same line as last arg
            last_offset <- arg_line_offset[length(arg_groups)]
            terms$out_line[close_idx] <- call_line + last_offset

            # Move trailing tokens on call_line after close paren to
            # the close paren's line (e.g., outer comma after nested call)
            if (last_offset > 0L) {
                trailing <- terms$out_line == call_line &
                    terms$out_order > terms$out_order[close_idx] &
                    !(seq_len(nrow(terms)) %in% all_call_idx)
                terms$out_line[trailing] <-
                    call_line + last_offset
            }

            terms <- terms[order(terms$out_line, terms$out_order), ]
            terms$out_order <- seq_len(nrow(terms))
            changed <- TRUE
            break
        }
    }

    terms
}

#' Reformat Function Definitions (AST Version)
#'
#' Rewrites named function signatures to fit within the line limit.
#' Short signatures go on one line; long ones wrap at commas with
#' paren-aligned or fixed continuation indent. Operates on the
#' DataFrame directly, avoiding the serialize/re-parse cycle that
#' caused idempotency oscillation.
#'
#' @param terms Enriched terminal DataFrame.
#' @param indent_str Indent string (e.g., `"    "`).
#' @param wrap Continuation style: `"paren"` or `"fixed"`.
#' @param brace_style `"kr"` or `"allman"`.
#' @param line_limit Maximum line length.
#' @param function_space Whether to add space after `function`.
#' @return Updated DataFrame.
#' @keywords internal
reformat_function_defs <- function (terms, indent_str = "    ",
                                        wrap = "paren",
                                        brace_style = "kr",
                                        line_limit = 80L,
                                        function_space = FALSE) {
    changed <- TRUE
    max_iter <- 200L

    while (changed && max_iter > 0L) {
        max_iter <- max_iter - 1L
        changed <- FALSE

        terms <- terms[order(terms$out_line, terms$out_order), ]
        func_indices <- which(terms$token == "FUNCTION")

        for (fi in func_indices) {
            # Only rewrite named function definitions
            prev_idx <- fi - 1L
            while (prev_idx >= 1L && terms$token[prev_idx] == "COMMENT") {
                prev_idx <- prev_idx - 1L
            }
            if (prev_idx < 1L) next
            if (!(terms$token[prev_idx] %in%
                  c("LEFT_ASSIGN", "EQ_ASSIGN", "RIGHT_ASSIGN"))) next

            # Find ( after function
            open_idx <- fi + 1L
            if (open_idx > nrow(terms)) next
            if (terms$token[open_idx] != "'('") next

            # Find matching )
            depth <- 1L
            close_idx <- open_idx + 1L
            while (close_idx <= nrow(terms) && depth > 0L) {
                if (terms$token[close_idx] == "'('") depth <- depth + 1L
                else if (terms$token[close_idx] == "')'") depth <- depth - 1L
                if (depth > 0L) close_idx <- close_idx + 1L
            }
            if (close_idx > nrow(terms)) next

            # Check for { after )
            has_brace <- close_idx + 1L <= nrow(terms) &&
                terms$token[close_idx + 1L] == "'{'"
            brace_idx <- if (has_brace) close_idx + 1L else NULL

            func_line <- terms$out_line[fi]

            # Measure prefix width (tokens before 'function' on its line)
            line_idx <- which(terms$out_line == func_line)
            prefix_idx <- line_idx[line_idx < fi]
            prefix_w <- nchar(indent_str) * token_indent_level(terms, line_idx[1])
            prev <- NULL
            prev_prev <- NULL
            for (pi in prefix_idx) {
                ptok <- terms[pi, ]
                if (!is.null(prev) && needs_space(prev, ptok, prev_prev)) {
                    prefix_w <- prefix_w + 1L
                }
                prefix_w <- prefix_w + nchar(ptok$out_text)
                prev_prev <- prev
                prev <- ptok
            }
            # Add space before function keyword if there's a prefix
            if (length(prefix_idx) > 0L && !is.null(prev) &&
                needs_space(prev, terms[fi, ], prev_prev)) {
                prefix_w <- prefix_w + 1L
            }

            func_open_text <- if (function_space) "function (" else "function("
            func_open_w <- nchar(func_open_text)
            # Width up to and including "("
            open_col <- prefix_w + func_open_w

            # Collect formal argument groups
            # Each group: list of token indices for one formal (name + = + default)
            arg_groups <- list()
            comma_indices <- integer(0)
            i <- open_idx + 1L
            current_group <- integer(0)
            formal_depth <- 0L

            while (i < close_idx) {
                tok <- terms[i, ]
                if (tok$token == "COMMENT") {
                    i <- i + 1L
                    next
                }
                if (tok$token == "'('") formal_depth <- formal_depth + 1L
                if (tok$token == "')'") formal_depth <- formal_depth - 1L
                if (tok$token == "','" && formal_depth == 0L) {
                    if (length(current_group) > 0L) {
                        arg_groups[[length(arg_groups) + 1L]] <- current_group
                    }
                    comma_indices <- c(comma_indices, i)
                    current_group <- integer(0)
                    i <- i + 1L
                    next
                }
                current_group <- c(current_group, i)
                i <- i + 1L
            }
            if (length(current_group) > 0L) {
                arg_groups[[length(arg_groups) + 1L]] <- current_group
            }

            if (length(arg_groups) == 0L) next

            # Measure each arg group width
            arg_widths <- integer(length(arg_groups))
            for (ai in seq_along(arg_groups)) {
                aidx <- arg_groups[[ai]]
                aw <- 0L
                aprev <- NULL
                aprev_prev <- NULL
                for (j in aidx) {
                    atok <- terms[j, ]
                    if (!is.null(aprev) &&
                        needs_space(aprev, atok, aprev_prev)) {
                        aw <- aw + 1L
                    }
                    aw <- aw + nchar(atok$out_text)
                    aprev_prev <- aprev
                    aprev <- atok
                }
                arg_widths[ai] <- aw
            }

            # Comments or braces inside formals prevent collapse
            formal_range <- seq(open_idx + 1L, close_idx - 1L)
            has_comment <- any(terms$token[formal_range] == "COMMENT")
            if (has_comment) next
            has_braces <- any(terms$token[formal_range] == "'{'")
            if (has_braces) next

            # Single-line width: prefix + function( + arg1, arg2, ..., argN)
            single_w <- open_col +
                sum(arg_widths) +
                (length(arg_groups) - 1L) * 2L +  # ", " between args
                1L  # ")"

            # Account for " {" suffix in K&R
            sig_limit <- if (has_brace && brace_style == "kr") {
                line_limit - 2L
            } else {
                line_limit
            }

            if (single_w <= sig_limit) {
                # Everything fits on one line
                target_line <- func_line

                # Move all tokens from function to ) (and {) to func_line
                all_range <- seq(fi, close_idx)
                need_change <- any(terms$out_line[all_range] != target_line)
                if (has_brace) {
                    need_change <- need_change ||
                        terms$out_line[brace_idx] != target_line
                }

                if (!need_change) next

                terms$out_line[all_range] <- target_line
                for (ci_comma in comma_indices) {
                    terms$out_line[ci_comma] <- target_line
                }
                if (has_brace) {
                    terms$out_line[brace_idx] <- target_line
                }

                # Re-sort
                terms <- terms[order(terms$out_line, terms$out_order), ]
                terms$out_order <- seq_len(nrow(terms))
                changed <- TRUE
                break

            } else {
                # Need to wrap: greedy packing at commas
                # First pass: compute which relative line (0, 1, 2, ...)
                # each arg group goes on, without modifying terms yet.
                if (wrap == "fixed") {
                    cont_width <- 8L
                } else {
                    cont_width <- open_col
                }

                current_w <- open_col
                lines_inserted <- 0L
                first_on_line <- TRUE
                # arg_line_offset[ai]: relative line offset (0 = func_line)
                arg_line_offset <- integer(length(arg_groups))

                for (ai in seq_along(arg_groups)) {
                    aw <- arg_widths[ai]
                    extra <- if (ai < length(arg_groups)) 2L else 1L
                    # After "(" or at start of continuation: no space
                    # After ", " from previous arg: space already in extra
                    test_w <- current_w + aw + extra

                    if (test_w > sig_limit && !first_on_line) {
                        lines_inserted <- lines_inserted + 1L
                        arg_line_offset[ai] <- lines_inserted
                        current_w <- cont_width + aw + extra
                        first_on_line <- FALSE
                    } else {
                        arg_line_offset[ai] <- lines_inserted
                        current_w <- test_w
                        first_on_line <- FALSE
                    }
                }

                if (lines_inserted == 0L) next

                # Second pass: first collapse the entire signature region
                # to func_line, then shift everything after it down.
                sig_token_idx <- c(fi, open_idx,
                    unlist(arg_groups), comma_indices, close_idx)
                if (has_brace) {
                    sig_token_idx <- c(sig_token_idx, brace_idx)
                }

                # Find max line the signature currently occupies
                old_sig_end <- max(terms$out_line[sig_token_idx])
                old_sig_lines <- old_sig_end - func_line  # lines used beyond func_line

                # Move all signature tokens to func_line temporarily
                terms$out_line[sig_token_idx] <- func_line

                # Adjust later lines: they were at old_sig_end+1..N,
                # now should be at func_line + lines_inserted + 1..N
                # Net shift = lines_inserted - old_sig_lines
                net_shift <- lines_inserted - old_sig_lines
                later <- terms$out_line > func_line &
                    !(seq_len(nrow(terms)) %in% sig_token_idx)
                if (net_shift != 0L) {
                    terms$out_line[later] <- terms$out_line[later] + net_shift
                }

                # Place function keyword + ( on func_line
                terms$out_line[fi] <- func_line
                terms$out_line[open_idx] <- func_line

                # Place args and commas
                for (ai in seq_along(arg_groups)) {
                    target <- func_line + arg_line_offset[ai]
                    terms$out_line[arg_groups[[ai]]] <- target
                    if (ai < length(arg_groups)) {
                        terms$out_line[comma_indices[ai]] <- target
                    }
                }

                # Place ) on same line as last arg
                last_line <- func_line + arg_line_offset[length(arg_groups)]
                terms$out_line[close_idx] <- last_line

                # Place { based on brace style
                if (has_brace) {
                    # Check for empty body: } immediately after {
                    empty_body <- brace_idx + 1L <= nrow(terms) &&
                        terms$token[brace_idx + 1L] == "'}'"
                    if (brace_style == "kr") {
                        terms$out_line[brace_idx] <- last_line
                        if (empty_body)
                            terms$out_line[brace_idx + 1L] <- last_line
                    } else {
                        # Allman: brace on its own line after )
                        # Need one more line
                        excl <- brace_idx
                        if (empty_body) excl <- c(excl, brace_idx + 1L)
                        later2 <- terms$out_line > last_line &
                            !seq_len(nrow(terms)) %in% excl
                        terms$out_line[later2] <- terms$out_line[later2] + 1L
                        terms$out_line[brace_idx] <- last_line + 1L
                        if (empty_body)
                            terms$out_line[brace_idx + 1L] <- last_line + 1L
                    }
                }

                terms <- terms[order(terms$out_line, terms$out_order), ]
                terms$out_order <- seq_len(nrow(terms))
                changed <- TRUE
                break
            }
        }
    }

    terms
}

#' Check if a Body Token Range is a Complete Statement
#'
#' Returns FALSE if the body has unclosed parens/brackets or ends
#' with an operator that expects a continuation (assignment, binary ops).
#'
#' @param terms Enriched terminal DataFrame.
#' @param body_range Integer vector of row indices for the body.
#' @return TRUE if the body is a complete, self-contained statement.
#' @keywords internal
find_bare_body_end <- function (terms, body_start) {
    n <- nrow(terms)
    pd <- 0L  # paren/bracket depth
    bd <- 0L  # brace depth
    id <- 0L  # if depth (unmatched IF tokens needing ELSE)
    trailing_ops <- c("LEFT_ASSIGN", "EQ_ASSIGN",
        "'+'", "'-'", "'*'", "'/'", "'^'", "'~'",
        "SPECIAL", "OR2", "AND2", "OR", "AND",
        "GT", "GE", "LT", "LE", "EQ", "NE", "PIPE",
        "'$'", "'@'")
    cont_starts <- c("'+'", "'-'", "'*'", "'/'", "'^'",
        "'~'", "SPECIAL", "OR2", "AND2", "OR", "AND",
        "GT", "GE", "LT", "LE", "EQ", "NE", "PIPE",
        "'['", "LBB", "'('", "'$'", "'@'", "'{'")
    last_real_tok <- ""  # last non-COMMENT token for trailing op check
    i <- body_start
    while (i <= n) {
        tok <- terms$token[i]
        if (tok %in% c("'('", "'['")) pd <- pd + 1L
        else if (tok == "LBB") pd <- pd + 2L
        else if (tok %in% c("')'", "']'")) pd <- pd - 1L
        else if (tok == "']]'") pd <- pd - 2L
        else if (tok == "'{'") bd <- bd + 1L
        else if (tok == "'}'") {
            bd <- bd - 1L
            if (bd < 0L) return(i - 1L)
        }
        else if (tok == "IF" && pd == 0L && bd == 0L) id <- id + 1L
        else if (tok == "ELSE" && pd == 0L && bd == 0L) {
            if (id > 0L) id <- id - 1L
            else return(i - 1L)
        }
        # Check statement end at balanced state
        # Skip ELSE tokens — they need to consume their body first
        if (pd == 0L && bd == 0L && id == 0L && i > body_start &&
            tok != "ELSE") {
            if (i + 1L > n) return(i)
            next_tok <- terms$token[i + 1L]
            if (next_tok %in% c("ELSE", "'}'")) {
                # ELSE of enclosing if or closing brace
                return(i)
            }
            if (terms$out_line[i + 1L] != terms$out_line[i]) {
                # For COMMENT tokens, use last non-comment token for
                # trailing op check (comment between <- and value,
                # or comment after else keyword)
                check_tok <- if (tok == "COMMENT") last_real_tok
                    else tok
                if (!(check_tok %in% c(trailing_ops, "ELSE")) &&
                    !(next_tok %in% cont_starts) &&
                    next_tok != "COMMENT") {
                    return(i)
                }
            }
        }
        if (tok != "COMMENT") last_real_tok <- tok
        i <- i + 1L
    }
    n
}

body_is_complete <- function (terms, body_range) {
    # Check paren/bracket balance
    bal <- 0L
    for (bi in body_range) {
        bt <- terms$token[bi]
        if (bt %in% c("'('", "'['")) bal <- bal + 1L
        else if (bt == "LBB") bal <- bal + 2L
        else if (bt %in% c("')'", "']'")) bal <- bal - 1L
        else if (bt == "']]'") bal <- bal - 2L
    }
    if (bal != 0L) return(FALSE)

    # Check for trailing operator (incomplete statement)
    last_tok <- terms$token[body_range[length(body_range)]]
    cont_ops <- c("LEFT_ASSIGN", "EQ_ASSIGN", "RIGHT_ASSIGN",
                   "'+'", "'-'", "'*'", "'/'", "'^'", "'~'",
                   "SPECIAL", "','", "OR2", "AND2", "OR", "AND",
                   "GT", "GE", "LT", "LE", "EQ", "NE")
    if (last_tok %in% cont_ops) return(FALSE)

    TRUE
}

#' Add Control Braces (AST Version)
#'
#' Finds bare control flow bodies (if/for/while/repeat without braces)
#' and transforms them according to the specified mode.
#'
#' Modes:
#' - `TRUE` / `"single"`: Add braces, keep on one line if short enough.
#' - `"multi"`: Add braces, force multi-line.
#' - `"next_line"`: Move same-line body to next line (no braces).
#' - `"same_line"`: Move next-line body to same line; strip single-stmt braces.
#'
#' @param terms Enriched terminal DataFrame.
#' @param mode Control brace mode.
#' @param indent_str Indent string (for line width calculations).
#' @param line_limit Maximum line width.
#' @return Updated DataFrame.
#' @keywords internal
add_control_braces <- function (terms, mode = "single",
                                    indent_str = "    ",
                                    line_limit = 80L) {
    if (isTRUE(mode)) mode <- "single"

    changed <- TRUE
    max_iter <- 200L

    while (changed && max_iter > 0L) {
        max_iter <- max_iter - 1L
        changed <- FALSE

        terms <- terms[order(terms$out_line, terms$out_order), ]
        terms$out_order <- seq_len(nrow(terms))
        n <- nrow(terms)

        ctrl_idx <- which(terms$token %in%
            c("IF", "FOR", "WHILE", "REPEAT", "ELSE"))

        for (ci in ctrl_idx) {
            tok <- terms$token[ci]
            ctrl_line <- terms$out_line[ci]

            # --- Find condition close paren ---
            if (tok %in% c("REPEAT", "ELSE")) {
                # No condition — body starts at next token
                cond_close <- ci
            } else {
                # Next token should be '('
                open_idx <- ci + 1L
                if (open_idx > n) next
                if (terms$token[open_idx] != "'('") next

                # Find matching ')'
                depth <- 1L
                cond_close <- open_idx + 1L
                while (cond_close <= n && depth > 0L) {
                    if (terms$token[cond_close] == "'('") depth <- depth + 1L
                    if (terms$token[cond_close] == "')'") depth <- depth - 1L
                    if (depth > 0L) cond_close <- cond_close + 1L
                }
                if (cond_close > n) next
            }

            # --- Find body start (first non-comment token after condition) ---
            body_start <- cond_close + 1L
            while (body_start <= n &&
                   terms$token[body_start] == "COMMENT") {
                body_start <- body_start + 1L
            }
            if (body_start > n) next

            body_line <- terms$out_line[body_start]

            # --- Skip else-if chains (ELSE followed by IF) ---
            if (tok == "ELSE" && terms$token[body_start] == "IF") next

            # --- Skip expression-context if-else ---
            if (tok %in% c("IF", "ELSE")) {
                # Skip if inside parens (function argument, etc.)
                if (terms$paren_depth[ci] > 0L) next

                # For IF: skip if preceded by assignment on same line
                # For ELSE: find the parent IF and apply same check
                check_idx <- if (tok == "ELSE") {
                    # Walk back to find the matching IF
                    parent_if <- ci - 1L
                    bd <- 0L
                    while (parent_if >= 1L) {
                        pt <- terms$token[parent_if]
                        if (pt == "'}'") bd <- bd + 1L
                        else if (pt == "'{'") bd <- bd - 1L
                        if (bd == 0L && pt == "IF") break
                        parent_if <- parent_if - 1L
                    }
                    if (parent_if >= 1L) parent_if else NULL
                } else {
                    ci
                }

                if (!is.null(check_idx)) {
                    check_line <- terms$out_line[check_idx]
                    skip <- FALSE
                    if (check_idx >= 2L) {
                        for (k in seq(check_idx - 1L, 1L)) {
                            if (terms$out_line[k] != check_line) break
                            if (terms$token[k] %in% c("LEFT_ASSIGN",
                                "EQ_ASSIGN", "RIGHT_ASSIGN")) {
                                skip <- TRUE
                                break
                            }
                        }
                    }
                    if (skip) next
                }
            }

            # --- Mode-specific body detection ---

            if (mode %in% c("single", "multi")) {
                # Body already braced? Skip.
                if (terms$token[body_start] == "'{'") next

                # Body must start on same line as condition close
                # (or next line for bare next-line bodies)
                cond_close_line <- terms$out_line[cond_close]

                # Find body extent: single-statement on one line
                body_end <- body_start
                body_start_line <- terms$out_line[body_start]
                while (body_end + 1L <= n &&
                       terms$out_line[body_end + 1L] == body_start_line &&
                       terms$token[body_end + 1L] != "COMMENT" &&
                       !terms$token[body_end + 1L] %in%
                           c("ELSE", "'}'")) {
                    body_end <- body_end + 1L
                }

                # Skip multi-line bodies (but allow ELSE/}/COMMENT as next)
                if (body_end + 1L <= n &&
                    terms$out_line[body_end + 1L] == body_start_line &&
                    !(terms$token[body_end + 1L] %in%
                        c("ELSE", "'}'", "COMMENT"))) next

                # Skip bodies with complex tokens
                body_range <- seq(body_start, body_end)
                body_toks <- terms$token[body_range]
                if (any(body_toks %in% c("IF", "FOR", "WHILE",
                                         "REPEAT", "FUNCTION"))) next

                # Skip incomplete statements (unclosed parens or
                # trailing operators)
                if (!body_is_complete(terms, body_range)) next

                # Check for ELSE after body
                has_else <- body_end + 1L <= n &&
                    terms$token[body_end + 1L] == "ELSE"

                # --- Insert braces ---
                # Create { and } tokens
                open_brace <- make_token("'{'", "{",
                    out_line = body_start_line,
                    out_order = terms$out_order[body_start] - 0.5)
                close_brace <- make_token("'}'", "}",
                    out_line = body_start_line,
                    out_order = terms$out_order[body_end] + 0.5)

                # Check for trailing comment on body line
                has_comment_on_line <- any(
                    terms$token == "COMMENT" &
                    terms$out_line == body_start_line &
                    terms$out_order > terms$out_order[body_start])

                # Check for comment on condition line
                cond_line_comment <- which(
                    terms$token == "COMMENT" &
                    terms$out_line == cond_close_line)

                if (mode == "multi" ||
                    body_start_line != cond_close_line ||
                    has_comment_on_line) {
                    # Multi-line: { on condition line, body indented,
                    # } on own line
                    later <- terms$out_line > cond_close_line
                    terms$out_line[later] <-
                        terms$out_line[later] + 2L
                    # Move structural tokens after body on
                    # condition line (ELSE, etc.) to close-brace
                    # line — but NOT comments
                    after_body <- terms$out_line == cond_close_line &
                        terms$out_order > terms$out_order[body_end] &
                        !(seq_len(n) %in% body_range) &
                        terms$token != "COMMENT"
                    terms$out_line[after_body] <-
                        cond_close_line + 2L
                    # Body tokens go to cond_close_line + 1
                    terms$out_line[body_range] <-
                        cond_close_line + 1L
                    # Comments trailing body on the SAME line go
                    # with body; comments on condition line (before
                    # body) stay on condition line after {
                    if (body_start_line == cond_close_line) {
                        body_trail_comments <- which(
                            terms$token == "COMMENT" &
                            terms$out_line == cond_close_line &
                            terms$out_order >
                                terms$out_order[body_start])
                        if (length(body_trail_comments) > 0L)
                            terms$out_line[body_trail_comments] <-
                                cond_close_line + 1L
                    }
                    # Move ELSE to close-brace line (} else)
                    if (has_else) {
                        ei <- body_end + 1L
                        terms$out_line[ei] <- cond_close_line + 2L
                    }
                    # Place { after close paren, before comment
                    brace_order <- terms$out_order[cond_close] + 0.5
                    open_brace$out_line <- cond_close_line
                    open_brace$out_order <- brace_order
                    close_brace$out_line <- cond_close_line + 2L
                    close_brace$out_order <-
                        terms$out_order[body_start] - 0.3
                } else {
                    # Single-line: if (cond) { body }
                    # Check if it fits
                    w <- ast_line_width(terms, body_start_line,
                                        indent_str) + 4L  # "{ " + " }"
                    if (w > line_limit) {
                        # Too long — use multi-line
                        later <- terms$out_line > cond_close_line
                        terms$out_line[later] <-
                            terms$out_line[later] + 2L
                        after_body <-
                            terms$out_line == cond_close_line &
                            terms$out_order >
                                terms$out_order[body_end] &
                            !(seq_len(n) %in% body_range) &
                            terms$token != "COMMENT"
                        terms$out_line[after_body] <-
                            cond_close_line + 2L
                        terms$out_line[body_range] <-
                            cond_close_line + 1L
                        open_brace$out_line <- cond_close_line
                        close_brace$out_line <-
                            cond_close_line + 2L
                        close_brace$out_order <-
                            terms$out_order[body_start] - 0.3
                    }
                    # else: keep single line (default out_line values)
                }

                terms <- insert_tokens(terms, open_brace)
                terms <- insert_tokens(terms, close_brace)
                terms <- recompute_nesting(terms)
                terms$out_order <- seq_len(nrow(terms))
                changed <- TRUE
                break

            } else if (mode == "next_line") {
                # Body already braced? Skip.
                if (terms$token[body_start] == "'{'") next

                cond_close_line <- terms$out_line[cond_close]
                body_start_line <- terms$out_line[body_start]
                target_level <- terms$nesting_level[ci] + 1L

                if (body_start_line != cond_close_line) {
                    # Body already on a different line.
                    # Check if it spans multiple lines — if so,
                    # must add braces for valid R.
                    body_end <- find_bare_body_end(terms,
                        body_start)
                    if (terms$out_line[body_end] >
                        body_start_line) {
                        # Multi-line bare body — add braces
                        body_range <- seq(body_start, body_end)
                        has_else <- body_end + 1L <= n &&
                            terms$token[body_end + 1L] == "ELSE"
                        body_end_line <- terms$out_line[body_end]

                        open_brace <- make_token("'{'", "{",
                            out_line = cond_close_line,
                            out_order =
                                terms$out_order[cond_close] + 0.5)
                        close_brace <- make_token("'}'", "}",
                            out_line = body_end_line + 1L,
                            out_order =
                                terms$out_order[body_start] - 0.3)

                        later <- terms$out_line > body_end_line
                        terms$out_line[later] <-
                            terms$out_line[later] + 1L
                        if (has_else)
                            terms$out_line[body_end + 1L] <-
                                body_end_line + 1L

                        terms <- insert_tokens(terms, open_brace)
                        terms <- insert_tokens(terms, close_brace)
                        terms <- recompute_nesting(terms)
                        terms$out_order <- seq_len(nrow(terms))
                        changed <- TRUE
                        break
                    }

                    # Single-line body — just fix nesting
                    first_line_body <- which(
                        terms$out_line == body_start_line &
                        terms$out_order >= terms$out_order[body_start])
                    first_line_body <- first_line_body[
                        !(terms$token[first_line_body] %in%
                            c("ELSE", "'}'"))]
                    if (length(first_line_body) > 0L &&
                        any(terms$nesting_level[first_line_body] !=
                            target_level)) {
                        terms$nesting_level[first_line_body] <-
                            target_level
                        terms <- terms[order(terms$out_line,
                                             terms$out_order), ]
                        terms$out_order <- seq_len(nrow(terms))
                        changed <- TRUE
                        break
                    }
                    next
                }

                # Body on same line as condition — move to next line
                # Find body extent on this line
                body_end <- body_start
                while (body_end + 1L <= n &&
                       terms$out_line[body_end + 1L] ==
                           body_start_line &&
                       terms$token[body_end + 1L] != "COMMENT" &&
                       !terms$token[body_end + 1L] %in%
                           c("ELSE", "'}'")) {
                    body_end <- body_end + 1L
                }

                # Skip complex bodies
                body_range <- seq(body_start, body_end)
                body_toks <- terms$token[body_range]
                if (any(body_toks %in% c("IF", "FOR", "WHILE",
                                         "REPEAT", "FUNCTION"))) next

                # Move body to next line
                later <- terms$out_line > cond_close_line
                terms$out_line[later] <-
                    terms$out_line[later] + 1L
                terms$out_line[body_range] <-
                    cond_close_line + 1L

                # Also move trailing tokens on the cond line
                trailing <- terms$out_line == cond_close_line &
                    terms$out_order > terms$out_order[body_end]
                if (any(trailing))
                    terms$out_line[trailing] <-
                        cond_close_line + 1L

                terms$nesting_level[body_range] <- target_level
                terms <- terms[order(terms$out_line,
                                     terms$out_order), ]
                terms$out_order <- seq_len(nrow(terms))
                changed <- TRUE
                break

            } else if (mode == "same_line") {
                cond_close_line <- terms$out_line[cond_close]

                # Skip if condition line has a trailing comment
                cond_line_toks <- terms$token[
                    terms$out_line == cond_close_line]
                if (any(cond_line_toks == "COMMENT")) next

                if (terms$token[body_start] == "'{'") {
                    # --- Strip braces if single-statement body ---
                    open_brace_idx <- body_start
                    # Find matching }
                    bd <- 1L
                    close_brace_idx <- open_brace_idx + 1L
                    while (close_brace_idx <= n && bd > 0L) {
                        if (terms$token[close_brace_idx] == "'{'") {
                            bd <- bd + 1L
                        }
                        if (terms$token[close_brace_idx] == "'}'") {
                            bd <- bd - 1L
                        }
                        if (bd > 0L) {
                            close_brace_idx <- close_brace_idx + 1L
                        }
                    }
                    if (close_brace_idx > n) next

                    # Inner body tokens
                    inner_start <- open_brace_idx + 1L
                    inner_end <- close_brace_idx - 1L
                    if (inner_start > inner_end) next

                    inner_range <- seq(inner_start, inner_end)
                    inner_toks <- terms$token[inner_range]

                    # Skip if multi-line body
                    inner_lines <- unique(
                        terms$out_line[inner_range])
                    if (length(inner_lines) > 1L) next

                    # Skip complex bodies (control flow, semicolons)
                    if (any(inner_toks %in% c("IF", "FOR", "WHILE",
                        "REPEAT", "FUNCTION", "COMMENT", "';'"))) next

                    # Skip if this if has an else (stripping braces
                    # would put else on a separate line)
                    has_else <- close_brace_idx + 1L <= n &&
                        terms$token[close_brace_idx + 1L] == "ELSE"
                    if (has_else) next

                    # Skip if combined line would exceed line_limit
                    cond_w <- ast_line_width(terms, cond_close_line,
                                              indent_str)
                    body_w <- ast_line_width(terms,
                        terms$out_line[inner_start], indent_str)
                    if (cond_w + 1L + body_w > line_limit) next

                    # Move inner tokens to condition line
                    terms$out_line[inner_range] <- cond_close_line

                    # Remove brace tokens
                    remove_idx <- c(open_brace_idx, close_brace_idx)
                    terms <- terms[-remove_idx, ]
                    terms <- terms[order(terms$out_line,
                                         terms$out_order), ]
                    terms$out_order <- seq_len(nrow(terms))

                    # Compact: shift lines after gap
                    terms <- renumber_lines(terms)
                    terms <- recompute_nesting(terms)
                    changed <- TRUE
                    break
                }

                # Bare body on next line — move to same line
                if (terms$out_line[body_start] != cond_close_line + 1L) {
                    next
                }

                # Find body extent
                body_end <- body_start
                body_start_line <- terms$out_line[body_start]
                while (body_end + 1L <= n &&
                       terms$out_line[body_end + 1L] ==
                           body_start_line &&
                       terms$token[body_end + 1L] != "COMMENT" &&
                       !terms$token[body_end + 1L] %in%
                           c("ELSE", "'}'")) {
                    body_end <- body_end + 1L
                }

                body_range <- seq(body_start, body_end)
                body_toks <- terms$token[body_range]
                if (any(body_toks %in% c("IF", "FOR", "WHILE",
                    "REPEAT", "FUNCTION", "COMMENT"))) next

                # Skip incomplete statements
                if (!body_is_complete(terms, body_range)) next

                # Skip if combined line would exceed line_limit
                cond_w <- ast_line_width(terms, cond_close_line,
                                          indent_str)
                body_w <- ast_line_width(terms, body_start_line,
                                          indent_str)
                if (cond_w + 1L + body_w > line_limit) next

                terms$out_line[body_range] <- cond_close_line
                terms <- renumber_lines(terms)
                terms <- terms[order(terms$out_line,
                                     terms$out_order), ]
                terms$out_order <- seq_len(nrow(terms))
                changed <- TRUE
                break
            }
        }
    }

    terms
}

#' Expand Bare If-Else in Function Call Arguments (AST Version)
#'
#' Finds bare `if (cond) expr else expr` arguments inside function calls
#' on overlong lines and expands them to braced multi-line form.
#'
#' @param terms Enriched terminal DataFrame.
#' @param indent_str Indent string.
#' @param line_limit Maximum line width.
#' @return Updated DataFrame.
#' @keywords internal
expand_call_if_args <- function (terms, indent_str = "    ",
                                     line_limit = 80L) {
    changed <- TRUE
    max_iter <- 200L

    while (changed && max_iter > 0L) {
        max_iter <- max_iter - 1L
        changed <- FALSE

        terms <- terms[order(terms$out_line, terms$out_order), ]
        terms$out_order <- seq_len(nrow(terms))
        n <- nrow(terms)

        if_indices <- which(terms$token == "IF")

        for (ii in if_indices) {
            # Must be inside parens (call argument)
            if (terms$paren_depth[ii] < 1L) next

            # Line must be overlong
            if_line <- terms$out_line[ii]
            if (ast_line_width(terms, if_line, indent_str) <= line_limit) next

            # Skip if inside function formals or inside braces
            # within a call (e.g., function(x) { if (cond) ... })
            skip_if <- FALSE
            enc_paren_idx <- 0L
            scan_depth <- 0L
            for (j in rev(seq_len(ii - 1L))) {
                jt <- terms$token[j]
                if (jt == "')'") scan_depth <- scan_depth + 1L
                else if (jt == "'('") {
                    if (scan_depth == 0L) {
                        enc_paren_idx <- j
                        if (j > 1L && terms$token[j - 1L] == "FUNCTION")
                            skip_if <- TRUE
                        break
                    }
                    scan_depth <- scan_depth - 1L
                }
            }
            # If IF is deeper in braces than the enclosing (,
            # it's inside a block, not a direct call argument
            if (enc_paren_idx > 0L &&
                terms$brace_depth[ii] >
                    terms$brace_depth[enc_paren_idx])
                skip_if <- TRUE
            if (skip_if) next

            # Find condition close paren
            open_idx <- ii + 1L
            if (open_idx > n || terms$token[open_idx] != "'('") next
            depth <- 1L
            close_idx <- open_idx + 1L
            while (close_idx <= n && depth > 0L) {
                if (terms$token[close_idx] == "'('") depth <- depth + 1L
                if (terms$token[close_idx] == "')'") depth <- depth - 1L
                if (depth > 0L) close_idx <- close_idx + 1L
            }
            if (close_idx > n) next

            # Must be bare (no brace after condition)
            body_first <- close_idx + 1L
            if (body_first > n) next
            if (terms$token[body_first] == "'{'") next

            # Find matching ELSE (stop if we exit enclosing parens)
            else_idx <- NULL
            search_idx <- close_idx + 1L
            nest_depth <- 0L
            brace_depth <- 0L
            paren_depth <- 0L
            while (search_idx <= n) {
                st <- terms$token[search_idx]
                if (st %in% c("'('", "'['")) {
                    paren_depth <- paren_depth + 1L
                } else if (st == "LBB") {
                    paren_depth <- paren_depth + 2L
                } else if (st %in% c("')'", "']'")) {
                    paren_depth <- paren_depth - 1L
                    if (paren_depth < 0L) break
                } else if (st == "']]'") {
                    paren_depth <- paren_depth - 2L
                    if (paren_depth < 0L) break
                }
                if (st == "'{'") brace_depth <- brace_depth + 1L
                else if (st == "'}'") {
                    brace_depth <- brace_depth - 1L
                    if (brace_depth < 0L) break
                }
                if (st == "IF") nest_depth <- nest_depth + 1L
                else if (st == "ELSE") {
                    if (nest_depth == 0L && brace_depth == 0L &&
                        paren_depth == 0L) {
                        else_idx <- search_idx
                        break
                    } else if (nest_depth > 0L) {
                        nest_depth <- nest_depth - 1L
                    }
                }
                search_idx <- search_idx + 1L
            }
            if (is.null(else_idx)) next

            # Extract true expression tokens
            true_range <- seq(close_idx + 1L, else_idx - 1L)
            if (length(true_range) == 0L) next

            # Find end of false expression
            false_start <- else_idx + 1L
            if (false_start > n) next
            if (terms$token[false_start] == "'{'") next

            false_end <- false_start
            fp_depth <- 0L
            fb_depth <- 0L
            fif_depth <- 0L
            false_start_line <- terms$out_line[false_start]

            while (false_end <= n) {
                ft <- terms$token[false_end]
                if (ft == "IF" &&
                    terms$out_line[false_end] == false_start_line)
                    fif_depth <- fif_depth + 1L
                if (ft == "ELSE" &&
                    terms$out_line[false_end] == false_start_line)
                    fif_depth <- max(0L, fif_depth - 1L)

                prev_fp <- fp_depth
                if (ft %in% c("'('", "'['")) fp_depth <- fp_depth + 1L
                if (ft == "LBB") fp_depth <- fp_depth + 2L
                if (ft == "','") {
                    if (fp_depth == 0L && fb_depth == 0L &&
                        fif_depth == 0L) {
                        false_end <- false_end - 1L
                        break
                    }
                }
                if (ft %in% c("')'", "']'")) {
                    if (fp_depth == 0L && fb_depth == 0L &&
                        fif_depth == 0L) {
                        false_end <- false_end - 1L
                        break
                    }
                    fp_depth <- fp_depth - 1L
                }
                if (ft == "']]'") fp_depth <- fp_depth - 2L
                if (ft == "'{'") fb_depth <- fb_depth + 1L
                if (ft == "'}'") fb_depth <- fb_depth - 1L

                cont_toks <- c("'['", "LBB", "'('", "'$'",
                    "'@'", "'+'", "'-'",
                    "'*'", "'/'", "'^'", "PIPE", "SPECIAL",
                    "OR2", "AND2", "OR", "AND", "'~'",
                    "EQ", "NE", "GE", "LE", "GT", "LT")
                if (prev_fp > 0L && fp_depth == 0L &&
                    fb_depth == 0L && fif_depth == 0L) {
                    next_ok <- false_end + 1L <= n &&
                        terms$token[false_end + 1L] %in% cont_toks
                    if (!next_ok) break
                    # Continuation confirmed — update start line
                    # so line-boundary check doesn't break on the
                    # continuation operator or its operand
                    false_start_line <- terms$out_line[false_end]
                }

                if (terms$out_line[false_end] > false_start_line &&
                    fp_depth <= 0L && fb_depth <= 0L &&
                    fif_depth == 0L) {
                    false_end <- false_end - 1L
                    break
                }

                false_end <- false_end + 1L
            }
            if (false_end > n) false_end <- n
            if (false_end < false_start) next

            false_range <- seq(false_start, false_end)

            # Skip complex branches
            true_toks <- terms$token[true_range]
            false_toks <- terms$token[false_range]
            if (any(true_toks == "FUNCTION") ||
                any(false_toks == "FUNCTION") ||
                any(true_toks == "COMMENT") ||
                any(false_toks == "COMMENT")) next

            # Skip if already multi-line
            if (any(terms$out_line[true_range] != if_line) ||
                any(terms$out_line[false_range] != if_line)) next

            # --- Restructure to multi-line braced form ---
            # Layout:
            #   ... prefix ..., (or prefix on own line)
            #   if (cond) {
            #       true_expr
            #   } else {
            #       false_expr
            #   }suffix...

            # Indent level for the if keyword
            if_level <- terms$nesting_level[ii]
            body_level <- if_level + 1L

            # All tokens involved in the if-else
            all_if_idx <- c(ii, seq(open_idx, close_idx),
                            true_range, else_idx, false_range)

            # Find tokens on if_line before the IF (prefix)
            # and after the false end (suffix)
            line_toks <- which(terms$out_line == if_line)
            prefix_idx <- line_toks[line_toks < ii]
            suffix_idx <- line_toks[line_toks > false_end]

            # Check if there's a comma before the IF
            has_comma_before <- length(prefix_idx) > 0L &&
                terms$token[prefix_idx[length(prefix_idx)]] == "','"

            # How many new lines we need:
            # Line 0: prefix (possibly up to comma)
            # Line 1: if (cond) {
            # Line 2: true_expr
            # Line 3: } else {
            # Line 4: false_expr
            # Line 5: }suffix
            lines_needed <- 5L  # 5 additional lines beyond the prefix

            # Shift everything after if_line down
            later <- terms$out_line > if_line
            terms$out_line[later] <- terms$out_line[later] + lines_needed

            # Place tokens
            if (has_comma_before) {
                # Prefix stays on if_line (up to and including comma)
                # IF and friends go to if_line + 1
                base_line <- if_line + 1L
            } else {
                # IF stays on if_line with prefix
                base_line <- if_line
                lines_needed <- lines_needed - 1L
                # Re-adjust later (undo +1 since IF stays on same line)
                terms$out_line[later] <- terms$out_line[later] - 1L
            }

            # if (cond) {  - on base_line
            terms$out_line[ii] <- base_line
            terms$out_line[seq(open_idx, close_idx)] <- base_line

            # true_expr - on base_line + 1
            terms$out_line[true_range] <- base_line + 1L
            terms$nesting_level[true_range] <- body_level

            # else - on base_line + 2
            terms$out_line[else_idx] <- base_line + 2L

            # false_expr - on base_line + 3
            terms$out_line[false_range] <- base_line + 3L
            terms$nesting_level[false_range] <- body_level

            # suffix - on base_line + 4
            if (length(suffix_idx) > 0L)
                terms$out_line[suffix_idx] <- base_line + 4L

            # Insert braces
            open_true <- make_token("'{'", "{",
                out_line = base_line,
                out_order = terms$out_order[close_idx] + 0.5)
            close_true <- make_token("'}'", "}",
                out_line = base_line + 2L,
                out_order = terms$out_order[else_idx] - 0.5)
            open_false <- make_token("'{'", "{",
                out_line = base_line + 2L,
                out_order = terms$out_order[else_idx] + 0.5)
            close_false <- make_token("'}'", "}",
                out_line = base_line + 4L,
                out_order = terms$out_order[false_range[1]] - 0.5)

            terms <- insert_tokens(terms, open_true)
            terms <- insert_tokens(terms, close_true)
            terms <- insert_tokens(terms, open_false)
            terms <- insert_tokens(terms, close_false)

            terms <- recompute_nesting(terms)
            terms <- terms[order(terms$out_line, terms$out_order), ]
            terms$out_order <- seq_len(nrow(terms))
            changed <- TRUE
            break
        }
    }

    terms
}

#' Reformat Inline If-Else Assignments (AST Version)
#'
#' Finds `var <- if (cond) true_expr else false_expr` patterns and
#' expands them to braced multi-line form with duplicated assignment:
#'   if (cond) {
#'       var <- true_expr
#'   } else {
#'       var <- false_expr
#'   }
#'
#' @param terms Enriched terminal DataFrame.
#' @param indent_str Indent string.
#' @param line_limit Maximum line width. Use 0 to expand all.
#' @return Updated DataFrame.
#' @keywords internal
reformat_inline_if <- function (terms, indent_str = "    ",
                                    line_limit = 0L) {
    changed <- TRUE
    max_iter <- 200L

    while (changed && max_iter > 0L) {
        max_iter <- max_iter - 1L
        changed <- FALSE

        terms <- terms[order(terms$out_line, terms$out_order), ]
        terms$out_order <- seq_len(nrow(terms))
        n <- nrow(terms)

        assign_idx <- which(terms$token %in%
            c("LEFT_ASSIGN", "EQ_ASSIGN"))

        for (ai in assign_idx) {
            assign_line <- terms$out_line[ai]

            # Find IF token after assignment on same line
            # Stop at semicolons, braces, or other assignments
            if_idx <- NULL
            found_function <- FALSE
            for (j in seq(ai + 1L, n)) {
                if (terms$out_line[j] != assign_line) break
                jt <- terms$token[j]
                if (jt == "FUNCTION") {
                    found_function <- TRUE
                    break
                }
                if (jt %in% c("';'", "'{'", "'}'", "'['", "LBB",
                    "LEFT_ASSIGN", "EQ_ASSIGN")) break
                if (jt == "IF") {
                    if_idx <- j
                    break
                }
            }
            if (found_function || is.null(if_idx)) next

            # Skip if assignment is inside brackets/parens
            if (terms$paren_depth[ai] > 0L) next

            # Skip if preceded by ELSE on same line (part of outer chain)
            line_toks_before <- which(terms$out_line == assign_line &
                terms$out_order < terms$out_order[ai])
            if (any(terms$token[line_toks_before] == "ELSE")) next

            # Skip if IF is inside unclosed parens relative to assignment
            paren_bal <- 0L
            for (j in seq(ai + 1L, if_idx - 1L)) {
                jt <- terms$token[j]
                if (jt == "'('") paren_bal <- paren_bal + 1L
                if (jt == "')'") paren_bal <- paren_bal - 1L
            }
            if (paren_bal > 0L) next

            # Check line_limit (skip short lines when line_limit > 0)
            if (line_limit > 0L &&
                ast_line_width(terms, assign_line, indent_str) <=
                    line_limit) next

            # Find condition close paren
            open_idx <- if_idx + 1L
            if (open_idx > n || terms$token[open_idx] != "'('") next
            depth <- 1L
            close_idx <- open_idx + 1L
            while (close_idx <= n && depth > 0L) {
                if (terms$token[close_idx] == "'('") depth <- depth + 1L
                if (terms$token[close_idx] == "')'") depth <- depth - 1L
                if (depth > 0L) close_idx <- close_idx + 1L
            }
            if (close_idx > n) next

            # Must be bare (no brace)
            if (close_idx + 1L > n) next
            if (terms$token[close_idx + 1L] == "'{'") next

            # Find matching ELSE (stop if we exit enclosing parens)
            else_idx <- NULL
            search_idx <- close_idx + 1L
            nest_depth <- 0L
            brace_depth <- 0L
            paren_depth <- 0L
            while (search_idx <= n) {
                st <- terms$token[search_idx]
                if (st %in% c("'('", "'['")) {
                    paren_depth <- paren_depth + 1L
                } else if (st == "LBB") {
                    paren_depth <- paren_depth + 2L
                } else if (st %in% c("')'", "']'")) {
                    paren_depth <- paren_depth - 1L
                    if (paren_depth < 0L) break
                } else if (st == "']]'") {
                    paren_depth <- paren_depth - 2L
                    if (paren_depth < 0L) break
                }
                if (st == "'{'") brace_depth <- brace_depth + 1L
                else if (st == "'}'") {
                    brace_depth <- brace_depth - 1L
                    if (brace_depth < 0L) break
                }
                if (st == "IF") nest_depth <- nest_depth + 1L
                else if (st == "ELSE") {
                    if (nest_depth == 0L && brace_depth == 0L &&
                        paren_depth == 0L) {
                        else_idx <- search_idx
                        break
                    } else if (nest_depth > 0L) {
                        nest_depth <- nest_depth - 1L
                    }
                }
                search_idx <- search_idx + 1L
            }
            if (is.null(else_idx)) next

            # Extract true and false ranges
            true_range <- seq(close_idx + 1L, else_idx - 1L)
            if (length(true_range) == 0L) next

            false_start <- else_idx + 1L
            if (false_start > n) next
            if (terms$token[false_start] == "'{'") next

            # Find end of false expression
            false_end <- false_start
            fp_depth <- 0L
            fb_depth <- 0L
            fif_depth <- 0L
            false_start_line <- terms$out_line[false_start]

            while (false_end <= n) {
                ft <- terms$token[false_end]
                if (ft == "IF" &&
                    terms$out_line[false_end] == false_start_line)
                    fif_depth <- fif_depth + 1L
                if (ft == "ELSE" &&
                    terms$out_line[false_end] == false_start_line)
                    fif_depth <- max(0L, fif_depth - 1L)

                prev_fp <- fp_depth
                if (ft %in% c("'('", "'['")) fp_depth <- fp_depth + 1L
                if (ft == "LBB") fp_depth <- fp_depth + 2L
                if (ft %in% c("')'", "']'")) {
                    if (fp_depth == 0L && fb_depth == 0L &&
                        fif_depth == 0L) {
                        false_end <- false_end - 1L
                        break
                    }
                    fp_depth <- fp_depth - 1L
                }
                if (ft == "']]'") fp_depth <- fp_depth - 2L
                if (ft == "'{'") fb_depth <- fb_depth + 1L
                if (ft == "'}'") fb_depth <- fb_depth - 1L

                if (prev_fp > 0L && fp_depth == 0L &&
                    fb_depth == 0L && fif_depth == 0L) {
                    cont_toks <- c("'['", "LBB", "'('", "'$'",
                        "'@'", "'+'", "'-'",
                        "'*'", "'/'", "'^'", "PIPE", "SPECIAL",
                        "OR2", "AND2", "OR", "AND", "'~'",
                        "EQ", "NE", "GE", "LE", "GT", "LT")
                    next_ok <- false_end + 1L <= n &&
                        terms$token[false_end + 1L] %in% cont_toks
                    if (!next_ok) break
                    # Continuation confirmed — update start line
                    # so line-boundary check doesn't break on the
                    # continuation operator or its operand
                    false_start_line <- terms$out_line[false_end]
                }

                if (terms$out_line[false_end] > false_start_line &&
                    fp_depth <= 0L && fb_depth <= 0L &&
                    fif_depth == 0L) {
                    false_end <- false_end - 1L
                    break
                }
                false_end <- false_end + 1L
            }
            if (false_end > n) false_end <- n
            if (false_end < false_start) next

            false_range <- seq(false_start, false_end)

            # Skip complex branches
            true_toks <- terms$token[true_range]
            false_toks <- terms$token[false_range]
            if (any(true_toks == "FUNCTION") ||
                any(false_toks == "FUNCTION") ||
                any(true_toks == "COMMENT") ||
                any(false_toks == "COMMENT")) next

            # Skip multi-line if-else when line_limit > 0
            all_lines <- unique(c(terms$out_line[true_range],
                                  terms$out_line[false_range]))
            spans_lines <- length(all_lines) > 1L ||
                any(all_lines != assign_line)
            if (spans_lines && line_limit > 0L) next

            # Skip chained else-if (too complex to restructure)
            if (terms$token[false_start] == "IF") next

            # --- Restructure ---
            # var_tokens: tokens before the assignment operator on assign_line
            line_toks <- which(terms$out_line == assign_line)
            var_idx <- line_toks[line_toks < ai]
            if (length(var_idx) == 0L) next

            # Skip if var tokens contain control flow or braces —
            # the assignment is inside a block and restructuring
            # would break the outer structure
            var_toks <- terms$token[var_idx]
            if (any(var_toks %in% c("IF", "FOR", "WHILE", "REPEAT",
                "ELSE", "'{'", "'}'", "FUNCTION"))) next

            # Condition tokens
            cond_range <- seq(open_idx, close_idx)

            # Compute indent level
            base_level <- terms$nesting_level[ai]
            body_level <- base_level + 1L

            # Find tokens after false_end on the same line (trailing)
            trailing_idx <- line_toks[line_toks > false_end]

            # Layout:
            # Line 0: if (cond) {
            # Line 1:     var <- true_expr
            # Line 2: } else {
            # Line 3:     var <- false_expr
            # Line 4: }
            # Line 5+: trailing tokens (if any)
            lines_needed <- 4L +
                (if (length(trailing_idx) > 0L) 1L else 0L)

            # Shift everything after assign_line down
            later <- terms$out_line > assign_line
            terms$out_line[later] <-
                terms$out_line[later] + lines_needed

            base_line <- assign_line

            # Place if (cond) on base_line
            terms$out_line[if_idx] <- base_line
            terms$out_line[cond_range] <- base_line
            terms$nesting_level[if_idx] <- base_level

            # Place true_expr on base_line + 1
            terms$out_line[true_range] <- base_line + 1L
            terms$nesting_level[true_range] <- body_level

            # Place else on base_line + 2
            terms$out_line[else_idx] <- base_line + 2L

            # Place false_expr on base_line + 3
            terms$out_line[false_range] <- base_line + 3L
            terms$nesting_level[false_range] <- body_level

            # Place trailing tokens on base_line + 4
            if (length(trailing_idx) > 0L)
                terms$out_line[trailing_idx] <- base_line + 4L

            # Move original var + assignment tokens to the true branch
            # (base_line + 1), then duplicate them for the false branch
            # (base_line + 3). Keep original token types for proper spacing.
            var_assign_idx <- c(var_idx, ai)
            terms$out_line[var_assign_idx] <- base_line + 1L
            terms$nesting_level[var_assign_idx] <- body_level
            # Ensure var+assign come before true_expr tokens
            terms$out_order[var_assign_idx] <-
                seq(0.01, by = 0.01, length.out = length(var_assign_idx))

            # Duplicate var + assignment for false branch
            dup_rows <- terms[var_assign_idx, , drop = FALSE]
            dup_rows$out_line <- base_line + 3L
            dup_rows$out_order <-
                seq(0.01, by = 0.01, length.out = nrow(dup_rows))
            # Convert EQ_ASSIGN to LEFT_ASSIGN in duplicates
            eq_in_dup <- which(dup_rows$token == "EQ_ASSIGN")
            if (length(eq_in_dup) > 0L) {
                dup_rows$token[eq_in_dup] <- "LEFT_ASSIGN"
                dup_rows$out_text[eq_in_dup] <- "<-"
            }
            terms <- insert_tokens(terms, dup_rows)

            # Insert braces with proper out_order
            # (must exceed existing token orders on each line)
            max_cond_order <- max(terms$out_order[
                terms$out_line == base_line])
            else_order <- terms$out_order[else_idx]
            terms <- insert_tokens(terms,
                make_token("'{'", "{",
                    out_line = base_line,
                    out_order = max_cond_order + 1))
            terms <- insert_tokens(terms,
                make_token("'}'", "}",
                    out_line = base_line + 2L, out_order = 0.001))
            terms <- insert_tokens(terms,
                make_token("'{'", "{",
                    out_line = base_line + 2L,
                    out_order = else_order + 0.5))
            terms <- insert_tokens(terms,
                make_token("'}'", "}",
                    out_line = base_line + 4L, out_order = 0.001))

            terms <- recompute_nesting(terms)
            terms <- terms[order(terms$out_line, terms$out_order), ]
            terms$out_order <- seq_len(nrow(terms))
            changed <- TRUE
            break
        }
    }

    terms
}

#' AST-Based Format Pipeline
#'
#' Single-pass pipeline: parse once, enrich the terminal DataFrame, run
#' all transforms as DataFrame operations, serialize to text once.
#'
#' @param code Code string for one top-level expression.
#' @param indent Indent string or integer.
#' @param wrap Continuation style: `"paren"` or `"fixed"`.
#' @param expand_if Whether to expand all inline if-else.
#' @param brace_style `"kr"` or `"allman"`.
#' @param line_limit Maximum line length.
#' @param function_space Add space after `function`.
#' @param control_braces Control brace mode.
#' @return Formatted code string.
#' @keywords internal
format_pipeline <- function (code, indent, wrap, expand_if, brace_style,
                                 line_limit, function_space = FALSE,
                                 control_braces = FALSE) {
    indent_str <- if (is.numeric(indent)) strrep(" ", indent) else indent

    # Parse once
    pd <- tryCatch(getParseData(parse(text = code, keep.source = TRUE)),
                   error = function(e) NULL)
    if (is.null(pd) || nrow(pd) == 0L) return(code)

    orig_lines <- strsplit(code, "\n", fixed = TRUE)[[1]]
    terms <- enrich_terminals(pd, orig_lines)

    # --- AST transforms (single DataFrame, no re-parsing) ---

    terms <- collapse_calls(terms, indent_str, line_limit)
    if (!isFALSE(control_braces))
        terms <- add_control_braces(terms, control_braces,
                                        indent_str, line_limit)
    terms <- expand_call_if_args(terms, indent_str, line_limit)
    if (!isFALSE(control_braces))
        terms <- add_control_braces(terms, control_braces,
                                        indent_str, line_limit)
    terms <- wrap_long_operators(terms, indent_str, line_limit)
    terms <- wrap_long_calls(terms, indent_str, wrap, line_limit)
    terms <- reformat_function_defs(terms, indent_str,
        wrap = wrap, brace_style = brace_style,
        line_limit = line_limit, function_space = function_space)
    if (!isFALSE(control_braces))
        terms <- add_control_braces(terms, control_braces,
                                        indent_str, line_limit)
    if (isTRUE(expand_if))
        terms <- reformat_inline_if(terms, indent_str,
                                         line_limit = 0L)
    else
        terms <- reformat_inline_if(terms, indent_str,
                                         line_limit = line_limit)
    if (!isFALSE(control_braces))
        terms <- add_control_braces(terms, control_braces,
                                        indent_str, line_limit)
    terms <- expand_call_if_args(terms, indent_str, line_limit)
    if (!isFALSE(control_braces))
        terms <- add_control_braces(terms, control_braces,
                                        indent_str, line_limit)
    terms <- collapse_calls(terms, indent_str, line_limit)
    terms <- wrap_long_operators(terms, indent_str, line_limit)
    terms <- wrap_long_calls(terms, indent_str, wrap, line_limit)
    terms <- wrap_long_operators(terms, indent_str, line_limit)
    if (!isFALSE(control_braces))
        terms <- add_control_braces(terms, control_braces,
                                        indent_str, line_limit)

    serialize_tokens(terms, indent_str, wrap, line_limit)
}
