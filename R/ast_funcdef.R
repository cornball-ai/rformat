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
reformat_function_defs <- function(terms, indent_str = "    ",
                                   wrap = "paren", brace_style = "kr",
                                   line_limit = 80L, function_space = FALSE) {
    changed <- TRUE
    max_iter <- 200L

    while (changed && max_iter > 0L) {
        max_iter <- max_iter - 1L
        changed <- FALSE

        terms <- terms[order(terms$out_line, terms$out_order),]
        func_indices <- which(terms$token == "FUNCTION")

        for (fi in func_indices) {
            # Only rewrite named function definitions
            prev_idx <- fi - 1L
            while (prev_idx >= 1L && terms$token[prev_idx] == "COMMENT") {
                prev_idx <- prev_idx - 1L
            }
            if (prev_idx < 1L) {
                next
            }
            if (!(terms$token[prev_idx] %in%
                    c("LEFT_ASSIGN", "EQ_ASSIGN", "RIGHT_ASSIGN"))) {
                next
            }

            # Find ( after function
            open_idx <- fi + 1L
            if (open_idx > nrow(terms)) {
                next
            }
            if (terms$token[open_idx] != "'('") {
                next
            }

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

            # Check for { after )
            has_brace <- close_idx + 1L <= nrow(terms) &&
            terms$token[close_idx + 1L] == "'{'"
            if (has_brace) {
                brace_idx <- close_idx + 1L
            } else {
                brace_idx <- NULL
            }

            func_line <- terms$out_line[fi]

            # Measure prefix width (tokens before 'function' on its line)
            line_idx <- which(terms$out_line == func_line)
            prefix_idx <- line_idx[line_idx < fi]
            prefix_w <- nchar(indent_str) * token_indent_level(terms,
                line_idx[1])
            prev <- NULL
            prev_prev <- NULL
            for (pi in prefix_idx) {
                ptok <- terms[pi,]
                if (!is.null(prev) && needs_space(prev, ptok, prev_prev)) {
                    prefix_w <- prefix_w + 1L
                }
                prefix_w <- prefix_w + nchar(ptok$out_text)
                prev_prev <- prev
                prev <- ptok
            }
            # Add space before function keyword if there's a prefix
            if (length(prefix_idx) > 0L && !is.null(prev) &&
                needs_space(prev, terms[fi,], prev_prev)) {
                prefix_w <- prefix_w + 1L
            }

            if (function_space) {
                func_open_text <- "function ("
            } else {
                func_open_text <- "function("
            }
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
                tok <- terms[i,]
                if (tok$token == "COMMENT") {
                    i <- i + 1L
                    next
                }
                if (tok$token == "'('") {
                    formal_depth <- formal_depth + 1L
                }
                if (tok$token == "')'") {
                    formal_depth <- formal_depth - 1L
                }
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

            if (length(arg_groups) == 0L) {
                next
            }

            # Measure each arg group width
            arg_widths <- integer(length(arg_groups))
            for (ai in seq_along(arg_groups)) {
                aidx <- arg_groups[[ai]]
                aw <- 0L
                aprev <- NULL
                aprev_prev <- NULL
                for (j in aidx) {
                    atok <- terms[j,]
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
            if (has_comment) {
                next
            }
            has_braces <- any(terms$token[formal_range] == "'{'")
            if (has_braces) {
                next
            }

            # Single-line width: prefix + function( + arg1, arg2, ..., argN)
            single_w <- open_col +
            sum(arg_widths) +
            (length(arg_groups) - 1L) * 2L + # ", " between args
            1L # ")"

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

                if (!need_change) {
                    next
                }

                terms$out_line[all_range] <- target_line
                for (ci_comma in comma_indices) {
                    terms$out_line[ci_comma] <- target_line
                }
                if (has_brace) {
                    terms$out_line[brace_idx] <- target_line
                }

                # Re-sort
                terms <- terms[order(terms$out_line, terms$out_order),]
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
                    if (ai < length(arg_groups)) {
                        extra <- 2L
                    } else {
                        extra <- 1L
                    }
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

                if (lines_inserted == 0L) {
                    next
                }

                # Second pass: first collapse the entire signature region
                # to func_line, then shift everything after it down.
                sig_token_idx <- c(fi, open_idx, unlist(arg_groups),
                                   comma_indices, close_idx)
                if (has_brace) {
                    sig_token_idx <- c(sig_token_idx, brace_idx)
                }

                # Find max line the signature currently occupies
                old_sig_end <- max(terms$out_line[sig_token_idx])
                old_sig_lines <- old_sig_end - func_line # lines used beyond func_line

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
                        if (empty_body) {
                            terms$out_line[brace_idx + 1L] <- last_line
                        }

                    } else {
                        # Allman: brace on its own line after )
                        # Need one more line
                        excl <- brace_idx
                        if (empty_body) {
                            excl <- c(excl, brace_idx + 1L)
                        }
                        later2 <- terms$out_line > last_line &
                        !seq_len(nrow(terms)) %in% excl
                        terms$out_line[later2] <- terms$out_line[later2] + 1L
                        terms$out_line[brace_idx] <- last_line + 1L
                        if (empty_body) {
                            terms$out_line[brace_idx + 1L] <- last_line + 1L
                        }

                    }
                }

                terms <- terms[order(terms$out_line, terms$out_order),]
                terms$out_order <- seq_len(nrow(terms))
                changed <- TRUE
                break
            }
        }
    }

    terms
}

