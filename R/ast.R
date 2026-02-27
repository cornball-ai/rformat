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
        if (wrap == "paren" && length(call_paren_stack) > 0L) {
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
                is_call <- i > 1L &&
                    line_toks$token[i - 1L] == "SYMBOL_FUNCTION_CALL"
                if (!is_call && i == 1L && li > 1L) {
                    # Check last token from previous line
                    prev_idx <- lines_out[[li - 1L]]
                    if (length(prev_idx) > 0L) {
                        prev_tok <- terms$token[prev_idx[length(prev_idx)]]
                        is_call <- prev_tok == "SYMBOL_FUNCTION_CALL"
                    }
                }
                call_paren_stack <- c(call_paren_stack,
                    if (is_call) pos else 0L)
                brace_at_call <- c(brace_at_call, cur_brace_depth)
            } else if (tt == "')'") {
                if (length(call_paren_stack) > 0L) {
                    call_paren_stack <-
                        call_paren_stack[-length(call_paren_stack)]
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
collapse_calls_ast <- function (terms, indent_str, line_limit = 80L) {
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
            terms$out_line[call_range] <- open_line

            # Also move suffix tokens (after ) on close line) to open_line
            suffix_idx <- which(terms$out_line == close_line &
                terms$out_order > terms$out_order[close_idx])
            if (length(suffix_idx) > 0L) {
                terms$out_line[suffix_idx] <- open_line
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
wrap_long_operators_ast <- function (terms, indent_str, line_limit = 80L) {
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
wrap_long_calls_ast <- function (terms, indent_str, wrap = "paren",
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
                    }
                    comma_indices <- c(comma_indices, k)
                    current_start <- k + 1L
                }
            }
            # Last arg
            if (current_start <= close_idx - 1L) {
                arg_groups[[length(arg_groups) + 1L]] <-
                    seq(current_start, close_idx - 1L)
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

            # Now greedily pack args onto lines
            current_w <- first_line_w
            current_line <- call_line
            lines_inserted <- 0L

            for (ai in seq_along(arg_groups)) {
                # Measure this arg's width
                arg_idx <- arg_groups[[ai]]
                arg_w <- 0L
                aprev <- NULL
                aprev_prev <- NULL
                for (aidx in arg_idx) {
                    atok <- terms[aidx, ]
                    if (!is.null(aprev) &&
                        needs_space(aprev, atok, aprev_prev)) {
                        arg_w <- arg_w + 1L
                    }
                    arg_w <- arg_w + nchar(atok$out_text)
                    aprev_prev <- aprev
                    aprev <- atok
                }
                # Add comma+space (except last arg) or ")" (last arg)
                if (ai < length(arg_groups)) {
                    extra <- 2L  # ", "
                } else {
                    extra <- 1L  # ")"
                }

                # Would it fit on current line?
                # Need space before first token of arg
                space_w <- 0L
                if (current_w > 0L) {
                    space_w <- 1L  # space after comma or after (
                }
                test_w <- current_w + space_w + arg_w + extra

                if (test_w > line_limit && current_w > cont_width) {
                    # Wrap: move this arg to a new line
                    new_ln <- current_line + lines_inserted + 1L
                    lines_inserted <- lines_inserted + 1L

                    # Shift later lines
                    later <- terms$out_line >= new_ln &
                        !(seq_len(nrow(terms)) %in% arg_idx)
                    terms$out_line[later] <- terms$out_line[later] + 1L

                    # Move arg tokens to new line
                    terms$out_line[arg_idx] <- new_ln

                    current_w <- cont_width + arg_w + extra
                    current_line <- new_ln
                } else {
                    current_w <- test_w
                }
            }

            # Move closing ) to same line as last arg
            last_arg_idx <- arg_groups[[length(arg_groups)]]
            terms$out_line[close_idx] <-
                terms$out_line[last_arg_idx[length(last_arg_idx)]]

            if (lines_inserted > 0L) {
                terms <- terms[order(terms$out_line, terms$out_order), ]
                terms$out_order <- seq_len(nrow(terms))
                changed <- TRUE
                break
            }
        }
    }

    terms
}
