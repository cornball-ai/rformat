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
enrich_terminals <- function(pd, orig_lines) {
    terms <- pd[pd$terminal,]
    terms <- terms[order(terms$line1, terms$col1),]
    terms <- restore_truncated_str_const_tokens(terms, orig_lines)

    n <- nrow(terms)
    if (n == 0L) {
        return(terms)
    }

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
    if (length(eq_idx) > 0L) {
        terms$out_text[eq_idx] <- "<-"
    }

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
                        if (terms$token[k] == "')'") {
                            pd2 <- pd2 + 1L
                        }
                        if (terms$token[k] == "'('") {
                            pd2 <- pd2 - 1L
                        }
                        if (pd2 > 0L) {
                            k <- k - 1L
                        }
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
recompute_nesting <- function(terms) {
    n <- nrow(terms)
    if (n == 0L) {
        return(terms)
    }

    # Re-sort by output position
    terms <- terms[order(terms$out_line, terms$out_order),]

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
                        if (terms$token[k] == "')'") {
                            pd2 <- pd2 + 1L
                        }
                        if (terms$token[k] == "'('") {
                            pd2 <- pd2 - 1L
                        }
                        if (pd2 > 0L) {
                            k <- k - 1L
                        }
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
token_indent_level <- function(terms, idx) {
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
ast_line_width <- function(terms, line_num, indent_str) {
    idx <- which(terms$out_line == line_num)
    if (length(idx) == 0L) {
        return(0L)
    }

    line_toks <- terms[idx,]
    # Indent for first token
    first_level <- token_indent_level(terms, idx[1])
    prefix_width <- nchar(indent_str) * first_level

    # Token widths + spaces
    width <- prefix_width
    prev <- NULL
    prev_prev <- NULL
    for (i in seq_len(nrow(line_toks))) {
        tok <- line_toks[i,]
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
serialize_tokens <- function(terms, indent_str, wrap = "paren",
                             line_limit = 80L) {
    if (nrow(terms) == 0L) {
        return("\n")
    }

    # Sort by output position
    terms <- terms[order(terms$out_line, terms$out_order),]

    # Group tokens by out_line
    lines_out <- split(seq_len(nrow(terms)), terms$out_line)
    line_nums <- as.integer(names(lines_out))

    # Track call-paren stack for paren-aligned continuation
    call_paren_stack <- integer(0) # output column of each open call (
    funcdef_paren_stack <- logical(0) # TRUE if paren is from FUNCTION def
    brace_at_call <- integer(0) # brace_depth when each call ( opened
    cur_brace_depth <- 0L

    result_lines <- list()
    prev_line_num <- 0L

    for (li in seq_along(line_nums)) {
        ln <- line_nums[li]
        idx <- lines_out[[li]]
        line_toks <- terms[idx,]

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
        if (has_active_paren && (wrap == "paren" || any(funcdef_paren_stack))) {
            inside_brace <- length(brace_at_call) > 0L &&
            cur_brace_depth > brace_at_call[length(brace_at_call)]
            if (!inside_brace) {
                open_cols <- call_paren_stack[call_paren_stack > 0L]
                if (length(open_cols) > 0L) {
                    first_tok <- line_toks$token[1]
                    if (!first_tok %in% c("')'", "']'", "']]'", "IF",
                            "FOR", "WHILE", "REPEAT", "ELSE", "'}'")) {
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
            tok <- line_toks[i,]
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
        if (extra_newlines > 0L) {
            prev_line_num <- prev_line_num + extra_newlines
        }

        # Update call-paren stack
        prefix_len <- nchar(line_prefix)
        pos <- prefix_len + 1L # 1-based position in output
        prev <- NULL
        prev_prev <- NULL
        for (i in seq_len(nrow(line_toks))) {
            tok <- line_toks[i,]
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
                    if (wrap == "fixed") {
                        8L
                    } else {
                        pos
                    }
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
insert_tokens <- function(terms, new_rows) {
    if (nrow(new_rows) == 0L) {
        return(terms)
    }

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
make_token <- function(token, text, out_line, out_order, parent = 0L) {
    data.frame(token = token, out_text = text, text = text,
               out_line = out_line, out_order = out_order, line1 = 0L,
               col1 = 0L, line2 = 0L, col2 = 0L, terminal = TRUE,
               parent = parent, id = 0L, stringsAsFactors = FALSE)
}

#' Renumber Output Lines Sequentially
#'
#' After transforms that insert or remove lines, renumber `out_line` so
#' values are sequential starting from 1, preserving relative order and
#' gaps for blank lines.
#'
#' @param terms Enriched terminal DataFrame.
#' @return Updated DataFrame with renumbered `out_line`.
#' @importFrom stats setNames
#' @keywords internal
renumber_lines <- function(terms) {
    terms <- terms[order(terms$out_line, terms$out_order),]
    old_lines <- unique(terms$out_line)
    mapping <- setNames(seq_along(old_lines), as.character(old_lines))
    terms$out_line <- as.integer(mapping[as.character(terms$out_line)])
    terms
}

