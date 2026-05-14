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
wrap_long_operators <- function(terms, indent_str, line_limit = 80L) {
    break_ops <- c("OR2", "AND2", "OR", "AND")
    changed <- TRUE
    max_iter <- 100L

    while (changed && max_iter > 0L) {
        max_iter <- max_iter - 1L
        changed <- FALSE

        terms <- terms[order(terms$out_line, terms$out_order),]
        terms$out_order <- seq_len(nrow(terms))
        lidx <- build_line_index(terms)
        out_lines <- unique(terms$out_line)

        for (ln in out_lines) {
            width <- line_index_width(terms, lidx, ln, indent_str)
            if (width <= line_limit) {
                next
            }

            idx <- line_index_get(lidx, ln)
            line_toks <- terms[idx,]

            # Skip semicolons
            if (any(line_toks$token == "';'")) {
                next
            }

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
                tok <- line_toks[j,]
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

            if (is.null(best_break)) {
                next
            }

            # Split: tokens after the break go to a new line
            break_at <- best_break
            new_line <- ln + 1L

            # Shift all later lines up by 1
            later <- terms$out_line > ln
            terms$out_line[later] <- terms$out_line[later] + 1L

            # Move tokens after break to new line
            move_idx <- idx[(break_at + 1L):length(idx)]
            terms$out_line[move_idx] <- new_line

            # Re-sort and fix order
            terms <- terms[order(terms$out_line, terms$out_order),]
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
wrap_long_calls <- function(terms, indent_str, wrap = "paren",
                            line_limit = 80L) {
    changed <- TRUE
    max_iter <- 100L

    while (changed && max_iter > 0L) {
        max_iter <- max_iter - 1L
        changed <- FALSE

        terms <- terms[order(terms$out_line, terms$out_order),]
        terms$out_order <- seq_len(nrow(terms))
        lidx <- build_line_index(terms)
        call_idx <- which(terms$token == "SYMBOL_FUNCTION_CALL")

        for (ci in call_idx) {
            open_idx <- ci + 1L
            if (open_idx > nrow(terms)) {
                next
            }
            if (terms$token[open_idx] != "'('") {
                next
            }

            call_line <- terms$out_line[ci]

            # Locate matching close paren first so we can detect the
            # already-wrapped case.
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

            already_wrapped <- terms$out_line[close_idx] != call_line

            # Re-pack already-wrapped calls so the first arg lands on
            # the call line. A call that's already single-line and
            # fits the limit needs no work.
            if (!already_wrapped &&
                line_index_width(terms, lidx, call_line,
                                 indent_str) <= line_limit) {
                next
            }

            # Skip calls containing braces or comments. Comments need
            # their own line, so re-packing would corrupt them.
            inner <- terms[seq(open_idx, close_idx),]
            if (any(inner$token %in% c("'{'", "'}'", "COMMENT"))) {
                next
            }

            # Skip semicolons on this line
            line_idx <- line_index_get(lidx, call_line)
            line_toks <- terms[line_idx,]
            if (any(line_toks$token == "';'")) {
                next
            }

            # Skip inner calls when outer call is wrappable
            func_order <- terms$out_order[ci]
            before <- line_toks[line_toks$out_order < func_order,]
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
                    if (length(oc_row) == 0L) {
                        next
                    }
                    oc_open <- oc_row + 1L
                    if (oc_open > nrow(terms)) {
                        next
                    }
                    if (terms$token[oc_open] != "'('") {
                        next
                    }
                    # Find matching )
                    od <- 1L
                    oc_close <- oc_open + 1L
                    while (oc_close <= nrow(terms) && od > 0L) {
                        if (terms$token[oc_close] == "'('") {
                            od <- od + 1L
                        }
                        if (terms$token[oc_close] == "')'") {
                            od <- od - 1L
                        }
                        if (od > 0L) {
                            oc_close <- oc_close + 1L
                        }
                    }
                    if (oc_close > nrow(terms)) {
                        next
                    }
                    if (terms$out_line[oc_close] != call_line) {
                        next
                    }
                    # Check for comma at depth 1
                    d2 <- 0L
                    for (ki in seq(oc_open + 1L, oc_close - 1L)) {
                        tt <- terms$token[ki]
                        if (tt == "'('") {
                            d2 <- d2 + 1L
                        }
                        if (tt == "')'") {
                            d2 <- d2 - 1L
                        }
                        if (tt == "','" && d2 == 0L) { skip <- TRUE; break }
                    }
                    if (skip) {
                        break
                    }
                }
                if (skip) {
                    next
                }
            }

            # Need at least one depth-0 comma
            has_comma <- FALSE
            d2 <- 0L
            for (k in seq(open_idx + 1L, close_idx - 1L)) {
                tt <- terms$token[k]
                if (tt %in% c("'('", "'['")) {
                    d2 <- d2 + 1L
                }
                if (tt == "LBB") {
                    d2 <- d2 + 2L
                }
                if (tt %in% c("')'", "']'")) {
                    d2 <- d2 - 1L
                }
                if (tt == "']]'") {
                    d2 <- d2 - 2L
                }
                if (tt == "','" && d2 == 0L) { has_comma <- TRUE; break }
            }
            if (!has_comma) {
                next
            }

            # Collect argument groups (ranges of indices between depth-0 commas)
            arg_groups <- list()
            comma_indices <- integer(0)
            current_start <- open_idx + 1L
            d2 <- 0L
            for (k in seq(open_idx + 1L, close_idx - 1L)) {
                tt <- terms$token[k]
                if (tt %in% c("'('", "'['")) {
                    d2 <- d2 + 1L
                }
                if (tt == "LBB") {
                    d2 <- d2 + 2L
                }
                if (tt %in% c("')'", "']'")) {
                    d2 <- d2 - 1L
                }
                if (tt == "']]'") {
                    d2 <- d2 - 2L
                }
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
            if (length(arg_groups) < 2L) {
                next
            }

            # Compute continuation indent
            indent_size <- nchar(indent_str)
            cont_level <- terms$nesting_level[open_idx] + 1L
            cont_width <- cont_level * indent_size

            if (wrap == "paren") {
                # Compute paren column = prefix width + func_name + "("
                prefix_idx <- line_idx[
                    terms$out_order[line_idx] < terms$out_order[ci]]
                prefix_w <- nchar(indent_str) *
                token_indent_level(terms, line_idx[1])
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
                # Add func name + (
                if (!is.null(prev) &&
                    needs_space(prev, terms[ci,], prev_prev)) {
                    prefix_w <- prefix_w + 1L
                }
                paren_col <- prefix_w + nchar(terms$out_text[ci]) + 1L
                if (paren_col <= line_limit %/% 2L) {
                    cont_width <- paren_col
                }
            }

            # Greedy packing: put args on lines until they exceed limit
            # Measure first-line width up to "("
            first_line_w <- 0L
            prev <- NULL
            prev_prev <- NULL
            for (fi in line_idx[line_idx <= open_idx]) {
                tok <- terms[fi,]
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
                    atok <- terms[aidx,]
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
                if (ai < length(arg_groups)) {
                    extra <- 2L
                } else {
                    extra <- 1L
                }
                if (!first_on_line) {
                    space_w <- 1L
                } else {
                    space_w <- 0L
                }
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

            # Capture the original close-paren line BEFORE any
            # mutation so we can compute the line delta for trailing
            # tokens. For an already-wrapped call, old_close_line >
            # call_line.
            old_close_line <- terms$out_line[close_idx]
            new_close_line <-
            call_line + arg_line_offset[length(arg_groups)]

            # Already-wrapped calls reach this point even when
            # lines_inserted == 0 (everything fits on one line),
            # because we want to collapse them. Never-wrapped calls
            # with lines_inserted == 0 need no work.
            if (lines_inserted == 0L && !already_wrapped) {
                next
            }

            # Don't re-pack an already-wrapped call if the first arg
            # alone won't fit on the call line, or if any resulting
            # line would exceed the limit. For deeply-nested calls
            # with huge args the greedy packer would otherwise
            # produce grotesque jam-onto-call-line output.
            if (already_wrapped) {
                repack_ok <- TRUE
                extra1 <- if (length(arg_groups) > 1L) 2L else 1L
                if (first_line_w + arg_widths[1] + extra1 > line_limit) {
                    repack_ok <- FALSE
                }
                if (repack_ok) {
                    line_w <- first_line_w
                    cur_offset <- 0L
                    first <- TRUE
                    for (ai in seq_along(arg_groups)) {
                        extra <- if (ai < length(arg_groups)) 2L else 1L
                        if (arg_line_offset[ai] != cur_offset) {
                            cur_offset <- arg_line_offset[ai]
                            line_w <- cont_width + arg_widths[ai] + extra
                            first <- FALSE
                        } else {
                            space_w <- if (first) 0L else 1L
                            line_w <- line_w + space_w + arg_widths[ai] + extra
                            first <- FALSE
                        }
                        if (line_w > line_limit) {
                            repack_ok <- FALSE
                            break
                        }
                    }
                }
                if (!repack_ok) {
                    next
                }
            }

            # Pass 2: apply line assignments
            # Collect all call-internal token indices
            all_call_idx <- c(unlist(arg_groups), comma_indices, close_idx)

            # Capture tokens trailing the close paren BEFORE the shift
            # pass — otherwise we'd pick up tokens that get shifted
            # INTO old_close_line.
            old_close_trailing <- integer(0)
            if (already_wrapped) {
                old_close_trailing <- which(
                    terms$out_line == old_close_line &
                    terms$out_order > terms$out_order[close_idx] &
                    !(seq_len(nrow(terms)) %in% all_call_idx))
            }

            # Shift later tokens by (new_close_line - old_close_line).
            # For never-wrapped this is +lines_inserted; for
            # already-wrapped it can be negative.
            line_delta <- new_close_line - old_close_line
            if (line_delta != 0L) {
                later <- terms$out_line > old_close_line &
                !(seq_len(nrow(terms)) %in% all_call_idx)
                terms$out_line[later] <- terms$out_line[later] + line_delta
            }

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
            terms$out_line[close_idx] <- new_close_line

            # Move trailing tokens (stuff after `)` on call_line for
            # never-wrapped calls, or on old_close_line for already-
            # wrapped) to the new close-paren line.
            if (last_offset > 0L) {
                trailing <- line_idx[
                    terms$out_order[line_idx] >
                    terms$out_order[close_idx] &
                    !line_idx %in% all_call_idx]
                terms$out_line[trailing] <- new_close_line
            }
            if (length(old_close_trailing) > 0L) {
                terms$out_line[old_close_trailing] <- new_close_line
            }

            terms <- terms[order(terms$out_line, terms$out_order),]
            terms$out_order <- seq_len(nrow(terms))
            changed <- TRUE
            break
        }
    }

    terms
}

