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
expand_call_if_args <- function(terms, indent_str = "    ", line_limit = 80L) {
    changed <- TRUE
    max_iter <- 200L

    while (changed && max_iter > 0L) {
        max_iter <- max_iter - 1L
        changed <- FALSE

        terms <- terms[order(terms$out_line, terms$out_order),]
        terms$out_order <- seq_len(nrow(terms))
        n <- nrow(terms)

        if_indices <- which(terms$token == "IF")

        for (ii in if_indices) {
            # Must be inside parens (call argument)
            if (terms$paren_depth[ii] < 1L) {
                next
            }

            # Line must be overlong
            if_line <- terms$out_line[ii]
            if (ast_line_width(terms, if_line, indent_str) <= line_limit) {
                next
            }

            # Skip if inside function formals or inside braces
            # within a call (e.g., function(x) { if (cond) ... })
            skip_if <- FALSE
            enc_paren_idx <- 0L
            scan_depth <- 0L
            for (j in rev(seq_len(ii - 1L))) {
                jt <- terms$token[j]
                if (jt == "')'") {
                    scan_depth <- scan_depth + 1L
                } else
                if (jt == "'('") {
                    if (scan_depth == 0L) {
                        enc_paren_idx <- j
                        if (j > 1L && terms$token[j - 1L] == "FUNCTION") {
                            skip_if <- TRUE
                        }

                        break
                    }
                    scan_depth <- scan_depth - 1L
                }
            }
            # If IF is deeper in braces than the enclosing (,
            # it's inside a block, not a direct call argument
            if (enc_paren_idx > 0L &&
                terms$brace_depth[ii] >
                terms$brace_depth[enc_paren_idx]) {
                skip_if <- TRUE
            }

            if (skip_if) {
                next
            }

            # Find condition close paren
            open_idx <- ii + 1L
            if (open_idx > n || terms$token[open_idx] != "'('") {
                next
            }
            depth <- 1L
            close_idx <- open_idx + 1L
            while (close_idx <= n && depth > 0L) {
                if (terms$token[close_idx] == "'('") {
                    depth <- depth + 1L
                }
                if (terms$token[close_idx] == "')'") {
                    depth <- depth - 1L
                }
                if (depth > 0L) {
                    close_idx <- close_idx + 1L
                }
            }
            if (close_idx > n) {
                next
            }

            # Must be bare (no brace after condition)
            body_first <- close_idx + 1L
            if (body_first > n) {
                next
            }
            if (terms$token[body_first] == "'{'") {
                next
            }

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
                    if (paren_depth < 0L) {
                        break
                    }
                } else if (st == "']]'") {
                    paren_depth <- paren_depth - 2L
                    if (paren_depth < 0L) {
                        break
                    }
                }
                if (st == "'{'") {
                    brace_depth <- brace_depth + 1L
                } else
                if (st == "'}'") {
                    brace_depth <- brace_depth - 1L
                    if (brace_depth < 0L) {
                        break
                    }
                }
                if (st == "IF") {
                    nest_depth <- nest_depth + 1L
                } else
                if (st == "ELSE") {
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
            if (is.null(else_idx)) {
                next
            }

            # Extract true expression tokens
            true_range <- seq(close_idx + 1L, else_idx - 1L)
            if (length(true_range) == 0L) {
                next
            }

            # Find end of false expression
            false_start <- else_idx + 1L
            if (false_start > n) {
                next
            }
            if (terms$token[false_start] == "'{'") {
                next
            }

            false_end <- false_start
            fp_depth <- 0L
            fb_depth <- 0L
            fif_depth <- 0L
            false_start_line <- terms$out_line[false_start]

            while (false_end <= n) {
                ft <- terms$token[false_end]
                if (ft == "IF" &&
                    terms$out_line[false_end] == false_start_line) {
                    fif_depth <- fif_depth + 1L
                }

                if (ft == "ELSE" &&
                    terms$out_line[false_end] == false_start_line) {
                    fif_depth <- max(0L, fif_depth - 1L)
                }

                prev_fp <- fp_depth
                if (ft %in% c("'('", "'['")) {
                    fp_depth <- fp_depth + 1L
                }
                if (ft == "LBB") {
                    fp_depth <- fp_depth + 2L
                }
                if (ft == "','") {
                    if (fp_depth == 0L && fb_depth == 0L && fif_depth == 0L) {
                        false_end <- false_end - 1L
                        break
                    }
                }
                if (ft %in% c("')'", "']'")) {
                    if (fp_depth == 0L && fb_depth == 0L && fif_depth == 0L) {
                        false_end <- false_end - 1L
                        break
                    }
                    fp_depth <- fp_depth - 1L
                }
                if (ft == "']]'") {
                    fp_depth <- fp_depth - 2L
                }
                if (ft == "'{'") {
                    fb_depth <- fb_depth + 1L
                }
                if (ft == "'}'") {
                    fb_depth <- fb_depth - 1L
                }

                cont_toks <- c("'['", "LBB", "'('", "'$'", "'@'", "'+'",
                               "'-'", "'*'", "'/'", "'^'", "PIPE",
                               "SPECIAL", "OR2", "AND2", "OR", "AND",
                               "'~'", "EQ", "NE", "GE", "LE", "GT", "LT")
                if (prev_fp > 0L && fp_depth == 0L &&
                    fb_depth == 0L && fif_depth == 0L) {
                    next_ok <- false_end + 1L <= n &&
                    terms$token[false_end + 1L] %in% cont_toks
                    if (!next_ok) {
                        break
                    }
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
            if (false_end > n) {
                false_end <- n
            }
            if (false_end < false_start) {
                next
            }

            false_range <- seq(false_start, false_end)

            # Skip complex branches
            true_toks <- terms$token[true_range]
            false_toks <- terms$token[false_range]
            if (any(true_toks == "FUNCTION") ||
                any(false_toks == "FUNCTION") ||
                any(true_toks == "COMMENT") ||
                any(false_toks == "COMMENT")) {
                next
            }

            # Skip if already multi-line
            if (any(terms$out_line[true_range] != if_line) ||
                any(terms$out_line[false_range] != if_line)) {
                next
            }

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
            all_if_idx <- c(ii, seq(open_idx, close_idx), true_range,
                            else_idx, false_range)

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
            lines_needed <- 5L # 5 additional lines beyond the prefix

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
            if (length(suffix_idx) > 0L) {
                terms$out_line[suffix_idx] <- base_line + 4L
            }

            # Insert braces
            open_true <- make_token("'{'", "{", out_line = base_line,
                                    out_order = terms$out_order[close_idx] + 0.5)
            close_true <- make_token("'}'", "}", out_line = base_line + 2L,
                                     out_order = terms$out_order[else_idx] - 0.5)
            open_false <- make_token("'{'", "{", out_line = base_line + 2L,
                                     out_order = terms$out_order[else_idx] + 0.5)
            close_false <- make_token("'}'", "}", out_line = base_line + 4L,
                                      out_order = terms$out_order[false_range[1]] - 0.5)

            terms <- insert_tokens(terms, open_true)
            terms <- insert_tokens(terms, close_true)
            terms <- insert_tokens(terms, open_false)
            terms <- insert_tokens(terms, close_false)

            terms <- recompute_nesting(terms)
            terms <- terms[order(terms$out_line, terms$out_order),]
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
reformat_inline_if <- function(terms, indent_str = "    ", line_limit = 0L) {
    changed <- TRUE
    max_iter <- 200L

    while (changed && max_iter > 0L) {
        max_iter <- max_iter - 1L
        changed <- FALSE

        terms <- terms[order(terms$out_line, terms$out_order),]
        terms$out_order <- seq_len(nrow(terms))
        n <- nrow(terms)

        assign_idx <- which(terms$token %in% c("LEFT_ASSIGN", "EQ_ASSIGN"))

        for (ai in assign_idx) {
            assign_line <- terms$out_line[ai]

            # Find IF token after assignment on same line
            # Stop at semicolons, braces, or other assignments
            if_idx <- NULL
            found_function <- FALSE
            for (j in seq(ai + 1L, n)) {
                if (terms$out_line[j] != assign_line) {
                    break
                }
                jt <- terms$token[j]
                if (jt == "FUNCTION") {
                    found_function <- TRUE
                    break
                }
                if (jt %in% c("';'", "'{'", "'}'", "'['", "LBB",
                              "LEFT_ASSIGN", "EQ_ASSIGN")) {
                    break
                }
                if (jt == "IF") {
                    if_idx <- j
                    break
                }
            }
            if (found_function || is.null(if_idx)) {
                next
            }

            # Skip if assignment is inside brackets/parens
            if (terms$paren_depth[ai] > 0L) {
                next
            }

            # Skip if preceded by ELSE on same line (part of outer chain)
            line_toks_before <- which(terms$out_line == assign_line &
                                      terms$out_order < terms$out_order[ai])
            if (any(terms$token[line_toks_before] == "ELSE")) {
                next
            }

            # Skip if IF is inside unclosed parens relative to assignment
            paren_bal <- 0L
            for (j in seq(ai + 1L, if_idx - 1L)) {
                jt <- terms$token[j]
                if (jt == "'('") {
                    paren_bal <- paren_bal + 1L
                }
                if (jt == "')'") {
                    paren_bal <- paren_bal - 1L
                }
            }
            if (paren_bal > 0L) {
                next
            }

            # Check line_limit (skip short lines when line_limit > 0)
            if (line_limit > 0L &&
                ast_line_width(terms, assign_line, indent_str) <=
                line_limit) {
                next
            }

            # Find condition close paren
            open_idx <- if_idx + 1L
            if (open_idx > n || terms$token[open_idx] != "'('") {
                next
            }
            depth <- 1L
            close_idx <- open_idx + 1L
            while (close_idx <= n && depth > 0L) {
                if (terms$token[close_idx] == "'('") {
                    depth <- depth + 1L
                }
                if (terms$token[close_idx] == "')'") {
                    depth <- depth - 1L
                }
                if (depth > 0L) {
                    close_idx <- close_idx + 1L
                }
            }
            if (close_idx > n) {
                next
            }

            # Must be bare (no brace)
            if (close_idx + 1L > n) {
                next
            }
            if (terms$token[close_idx + 1L] == "'{'") {
                next
            }

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
                    if (paren_depth < 0L) {
                        break
                    }
                } else if (st == "']]'") {
                    paren_depth <- paren_depth - 2L
                    if (paren_depth < 0L) {
                        break
                    }
                }
                if (st == "'{'") {
                    brace_depth <- brace_depth + 1L
                } else
                if (st == "'}'") {
                    brace_depth <- brace_depth - 1L
                    if (brace_depth < 0L) {
                        break
                    }
                }
                if (st == "IF") {
                    nest_depth <- nest_depth + 1L
                } else
                if (st == "ELSE") {
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
            if (is.null(else_idx)) {
                next
            }

            # Extract true and false ranges
            true_range <- seq(close_idx + 1L, else_idx - 1L)
            if (length(true_range) == 0L) {
                next
            }

            false_start <- else_idx + 1L
            if (false_start > n) {
                next
            }
            if (terms$token[false_start] == "'{'") {
                next
            }

            # Find end of false expression
            false_end <- false_start
            fp_depth <- 0L
            fb_depth <- 0L
            fif_depth <- 0L
            false_start_line <- terms$out_line[false_start]

            while (false_end <= n) {
                ft <- terms$token[false_end]
                if (ft == "IF" &&
                    terms$out_line[false_end] == false_start_line) {
                    fif_depth <- fif_depth + 1L
                }

                if (ft == "ELSE" &&
                    terms$out_line[false_end] == false_start_line) {
                    fif_depth <- max(0L, fif_depth - 1L)
                }

                prev_fp <- fp_depth
                if (ft %in% c("'('", "'['")) {
                    fp_depth <- fp_depth + 1L
                }
                if (ft == "LBB") {
                    fp_depth <- fp_depth + 2L
                }
                if (ft %in% c("')'", "']'")) {
                    if (fp_depth == 0L && fb_depth == 0L && fif_depth == 0L) {
                        false_end <- false_end - 1L
                        break
                    }
                    fp_depth <- fp_depth - 1L
                }
                if (ft == "']]'") {
                    fp_depth <- fp_depth - 2L
                }
                if (ft == "'{'") {
                    fb_depth <- fb_depth + 1L
                }
                if (ft == "'}'") {
                    fb_depth <- fb_depth - 1L
                }

                if (prev_fp > 0L && fp_depth == 0L &&
                    fb_depth == 0L && fif_depth == 0L) {
                    cont_toks <- c("'['", "LBB", "'('", "'$'", "'@'",
                                   "'+'", "'-'", "'*'", "'/'", "'^'",
                                   "PIPE", "SPECIAL", "OR2", "AND2", "OR",
                                   "AND", "'~'", "EQ", "NE", "GE", "LE",
                                   "GT", "LT")
                    next_ok <- false_end + 1L <= n &&
                    terms$token[false_end + 1L] %in% cont_toks
                    if (!next_ok) {
                        break
                    }
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
            if (false_end > n) {
                false_end <- n
            }
            if (false_end < false_start) {
                next
            }

            false_range <- seq(false_start, false_end)

            # Skip complex branches
            true_toks <- terms$token[true_range]
            false_toks <- terms$token[false_range]
            if (any(true_toks == "FUNCTION") ||
                any(false_toks == "FUNCTION") ||
                any(true_toks == "COMMENT") ||
                any(false_toks == "COMMENT")) {
                next
            }

            # Skip multi-line if-else when line_limit > 0
            all_lines <- unique(c(terms$out_line[true_range],
                                  terms$out_line[false_range]))
            spans_lines <- length(all_lines) > 1L ||
            any(all_lines != assign_line)
            if (spans_lines && line_limit > 0L) {
                next
            }

            # Skip chained else-if (too complex to restructure)
            if (terms$token[false_start] == "IF") {
                next
            }

            # --- Restructure ---
            # var_tokens: tokens before the assignment operator on assign_line
            line_toks <- which(terms$out_line == assign_line)
            var_idx <- line_toks[line_toks < ai]
            if (length(var_idx) == 0L) {
                next
            }

            # Skip if var tokens contain control flow or braces —
            # the assignment is inside a block and restructuring
            # would break the outer structure
            var_toks <- terms$token[var_idx]
            if (any(var_toks %in% c("IF", "FOR", "WHILE", "REPEAT", "ELSE",
                                    "'{'", "'}'", "FUNCTION"))) {
                next
            }

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
            if (length(trailing_idx) > 0L) {
                terms$out_line[trailing_idx] <- base_line + 4L
            }

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
            dup_rows <- terms[var_assign_idx,, drop = FALSE]
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
            max_cond_order <- max(terms$out_order[terms$out_line == base_line])
            else_order <- terms$out_order[else_idx]
            terms <- insert_tokens(terms,
                                   make_token("'{'", "{", out_line = base_line,
                    out_order = max_cond_order + 1))
            terms <- insert_tokens(terms,
                                   make_token("'}'", "}", out_line = base_line + 2L,
                    out_order = 0.001))
            terms <- insert_tokens(terms,
                                   make_token("'{'", "{", out_line = base_line + 2L,
                    out_order = else_order + 0.5))
            terms <- insert_tokens(terms,
                                   make_token("'}'", "}", out_line = base_line + 4L,
                    out_order = 0.001))

            terms <- recompute_nesting(terms)
            terms <- terms[order(terms$out_line, terms$out_order),]
            terms$out_order <- seq_len(nrow(terms))
            changed <- TRUE
            break
        }
    }

    terms
}

