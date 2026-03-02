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
collapse_calls <- function(terms, indent_str, line_limit = 80L) {
    changed <- TRUE
    max_iter <- 100L

    while (changed && max_iter > 0L) {
        max_iter <- max_iter - 1L
        changed <- FALSE

        terms <- terms[order(terms$out_line, terms$out_order),]
        target <- c("SYMBOL_FUNCTION_CALL", "IF", "FOR", "WHILE")
        target_idx <- which(terms$token %in% target)

        for (ci in target_idx) {
            # Next token should be (
            open_idx <- ci + 1L
            if (open_idx > nrow(terms)) {
                next
            }
            if (terms$token[open_idx] != "'('") {
                next
            }

            open_line <- terms$out_line[open_idx]

            # Find matching )
            depth <- 1L
            close_idx <- open_idx + 1L
            while (close_idx <= nrow(terms) && depth > 0L) {
                if (terms$token[close_idx] == "'('") {
                    depth <- depth + 1L
                } else
                if (terms$token[close_idx] == "')'") {
                    depth <- depth - 1L
                }
                if (depth > 0L) {
                    close_idx <- close_idx + 1L
                }
            }
            if (close_idx > nrow(terms)) {
                next
            }

            # Only multi-line groups
            close_line <- terms$out_line[close_idx]
            if (close_line == open_line) {
                next
            }

            # Skip if group contains comments or braces
            inner <- terms[seq(open_idx, close_idx),]
            if (any(inner$token == "COMMENT")) {
                next
            }
            if (any(inner$token %in% c("'{'", "'}'"))) {
                next
            }

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
                    if (st %in% c("')'", "']'", "']]'", "','", "';'",
                                  "'{'", "COMMENT", "'+'", "'-'", "'*'",
                                  "'/'", "'?'", "PIPE", "SPECIAL", "OR2",
                                  "AND2", "OR", "AND", "'~'", "EQ", "NE",
                                  "GE", "LE", "GT", "LT", "LEFT_ASSIGN",
                                  "EQ_ASSIGN", "RIGHT_ASSIGN")) {
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
                            if (ist == "'('") {
                                call_depth <- call_depth + 1L
                            }
                            if (ist == "')'") {
                                call_depth <- call_depth - 1L
                            }
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
                            if (ist == "']'") {
                                idx_depth <- idx_depth - 1L
                            }
                            if (ist == "']]'") {
                                idx_depth <- idx_depth - 2L
                            }
                            si <- si + 1L
                        }
                        keep_count <- si - 1L
                    } else {
                        break
                    }
                }
                if (keep_count < n_suffix) {
                    suffix_idx <- suffix_idx[seq_len(keep_count)]
                }

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
                if (too_wide) {
                    next
                }
            }

            all_move <- c(call_range, if (length(suffix_idx) > 0L) suffix_idx,
                if (length(remaining_close) > 0L) remaining_close)
            saved_lines <- terms$out_line[all_move]
            terms$out_line[all_move] <- open_line

            # Check if collapsed line exceeds line limit.
            # Only skip when there are no depth-0 commas inside
            # the call — if commas exist, wrap_long_calls
            # can re-wrap after collapse.
            if (ast_line_width(terms, open_line, indent_str) > line_limit) {
                has_d0_comma <- FALSE
                d0 <- 0L
                for (ck in seq(open_idx + 1L, close_idx - 1L)) {
                    ct <- terms$token[ck]
                    if (ct %in% c("'('", "'['")) {
                        d0 <- d0 + 1L
                    } else
                    if (ct == "LBB") {
                        d0 <- d0 + 2L
                    } else
                    if (ct %in% c("')'", "']'")) {
                        d0 <- d0 - 1L
                    } else
                    if (ct == "']]'") {
                        d0 <- d0 - 2L
                    }
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
            terms <- terms[order(terms$out_line, terms$out_order),]
            terms$out_order <- seq_len(nrow(terms))
            changed <- TRUE
            break # restart scan
        }
    }

    terms
}

