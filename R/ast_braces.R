#' Check if a Body Token Range is a Complete Statement
#'
#' Returns FALSE if the body has unclosed parens/brackets or ends
#' with an operator that expects a continuation (assignment, binary ops).
#'
#' @param terms Enriched terminal DataFrame.
#' @param body_start Integer row index where the bare body begins.
#' @return Integer row index of the last token in the bare body.
#' @keywords internal
find_bare_body_end <- function(terms, body_start) {
    n <- nrow(terms)
    pd <- 0L # paren/bracket depth
    bd <- 0L # brace depth
    id <- 0L # if depth (unmatched IF tokens needing ELSE)
    trailing_ops <- c("LEFT_ASSIGN", "EQ_ASSIGN", "'+'", "'-'", "'*'",
                      "'/'", "'^'", "'~'", "SPECIAL", "OR2", "AND2",
                      "OR", "AND", "GT", "GE", "LT", "LE", "EQ", "NE",
                      "PIPE", "'$'", "'@'")
    cont_starts <- c("'+'", "'-'", "'*'", "'/'", "'^'", "'~'", "SPECIAL",
                     "OR2", "AND2", "OR", "AND", "GT", "GE", "LT", "LE",
                     "EQ", "NE", "PIPE", "'['", "LBB", "'('", "'$'",
                     "'@'", "'{'")
    last_real_tok <- "" # last non-COMMENT token for trailing op check
    i <- body_start
    while (i <= n) {
        tok <- terms$token[i]
        if (tok %in% c("'('", "'['")) {
            pd <- pd + 1L
        } else
        if (tok == "LBB") {
            pd <- pd + 2L
        } else
        if (tok %in% c("')'", "']'")) {
            pd <- pd - 1L
        } else
        if (tok == "']]'") {
            pd <- pd - 2L
        } else
        if (tok == "'{'") {
            bd <- bd + 1L
        } else
        if (tok == "'}'") {
            bd <- bd - 1L
            if (bd < 0L) {
                return(i - 1L)
            }
        }
        else if (tok == "IF" && pd == 0L && bd == 0L) {
            id <- id + 1L
        } else
        if (tok == "ELSE" && pd == 0L && bd == 0L) {
            if (id > 0L) {
                id <- id - 1L
            } else {
                return(i - 1L)
            }

        }
        # Check statement end at balanced state
        # Skip ELSE tokens — they need to consume their body first
        if (pd == 0L && bd == 0L && id == 0L && i > body_start &&
            tok != "ELSE") {
            if (i + 1L > n) {
                return(i)
            }
            next_tok <- terms$token[i + 1L]
            if (next_tok %in% c("ELSE", "'}'")) {
                # ELSE of enclosing if or closing brace
                return(i)
            }
            if (terms$out_line[i + 1L] != terms$out_line[i]) {
                # For COMMENT tokens, use last non-comment token for
                # trailing op check (comment between <- and value,
                # or comment after else keyword)
                if (tok == "COMMENT") {
                    check_tok <- last_real_tok
                } else {
                    check_tok <- tok
                }

                if (!(check_tok %in% c(trailing_ops, "ELSE")) &&
                    !(next_tok %in% cont_starts) &&
                    next_tok != "COMMENT") {
                    return(i)
                }
            }
        }
        if (tok != "COMMENT") {
            last_real_tok <- tok
        }
        i <- i + 1L
    }
    n
}

body_is_complete <- function(terms, body_range) {
    # Check paren/bracket balance
    bal <- 0L
    for (bi in body_range) {
        bt <- terms$token[bi]
        if (bt %in% c("'('", "'['")) {
            bal <- bal + 1L
        } else
        if (bt == "LBB") {
            bal <- bal + 2L
        } else
        if (bt %in% c("')'", "']'")) {
            bal <- bal - 1L
        } else
        if (bt == "']]'") {
            bal <- bal - 2L
        }
    }
    if (bal != 0L) {
        return(FALSE)
    }

    # Check for trailing operator (incomplete statement)
    last_tok <- terms$token[body_range[length(body_range)]]
    cont_ops <- c("LEFT_ASSIGN", "EQ_ASSIGN", "RIGHT_ASSIGN", "'+'", "'-'",
                  "'*'", "'/'", "'^'", "'~'", "SPECIAL", "','", "OR2",
                  "AND2", "OR", "AND", "GT", "GE", "LT", "LE", "EQ",
                  "NE")
    if (last_tok %in% cont_ops) {
        return(FALSE)
    }

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
add_control_braces <- function(terms, mode = "single", indent_str = "    ",
                               line_limit = 80L) {
    if (isTRUE(mode)) {
        mode <- "single"
    }

    changed <- TRUE
    max_iter <- 200L

    while (changed && max_iter > 0L) {
        max_iter <- max_iter - 1L
        changed <- FALSE

        terms <- terms[order(terms$out_line, terms$out_order),]
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
                if (open_idx > n) {
                    next
                }
                if (terms$token[open_idx] != "'('") {
                    next
                }

                # Find matching ')'
                depth <- 1L
                cond_close <- open_idx + 1L
                while (cond_close <= n && depth > 0L) {
                    if (terms$token[cond_close] == "'('") {
                        depth <- depth + 1L
                    }
                    if (terms$token[cond_close] == "')'") {
                        depth <- depth - 1L
                    }
                    if (depth > 0L) {
                        cond_close <- cond_close + 1L
                    }
                }
                if (cond_close > n) {
                    next
                }
            }

            # --- Find body start (first non-comment token after condition) ---
            body_start <- cond_close + 1L
            while (body_start <= n && terms$token[body_start] == "COMMENT") {
                body_start <- body_start + 1L
            }
            if (body_start > n) {
                next
            }

            body_line <- terms$out_line[body_start]

            # --- Skip else-if chains (ELSE followed by IF) ---
            if (tok == "ELSE" && terms$token[body_start] == "IF") {
                next
            }

            # --- Skip expression-context if-else ---
            if (tok %in% c("IF", "ELSE")) {
                # Skip if inside parens (function argument, etc.)
                if (terms$paren_depth[ci] > 0L) {
                    next
                }

                # For IF: skip if preceded by assignment on same line
                # For ELSE: find the parent IF and apply same check
                check_idx <- if (tok == "ELSE") {
                    # Walk back to find the matching IF
                    parent_if <- ci - 1L
                    bd <- 0L
                    while (parent_if >= 1L) {
                        pt <- terms$token[parent_if]
                        if (pt == "'}'") {
                            bd <- bd + 1L
                        } else
                        if (pt == "'{'") {
                            bd <- bd - 1L
                        }
                        if (bd == 0L && pt == "IF") {
                            break
                        }
                        parent_if <- parent_if - 1L
                    }
                    if (parent_if >= 1L) {
                        parent_if
                    } else {
                        NULL
                    }
                } else {
                    ci
                }

                if (!is.null(check_idx)) {
                    check_line <- terms$out_line[check_idx]
                    skip <- FALSE
                    if (check_idx >= 2L) {
                        for (k in seq(check_idx - 1L, 1L)) {
                            if (terms$out_line[k] != check_line) {
                                break
                            }
                            if (terms$token[k] %in% c("LEFT_ASSIGN",
                                    "EQ_ASSIGN", "RIGHT_ASSIGN")) {
                                skip <- TRUE
                                break
                            }
                        }
                    }
                    if (skip) {
                        next
                    }
                }
            }

            # --- Mode-specific body detection ---

            if (mode %in% c("single", "multi")) {
                # Body already braced? Skip.
                if (terms$token[body_start] == "'{'") {
                    next
                }

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
                        c("ELSE", "'}'", "COMMENT"))) {
                    next
                }

                # Skip bodies with complex tokens
                body_range <- seq(body_start, body_end)
                body_toks <- terms$token[body_range]
                if (any(body_toks %in% c("IF", "FOR", "WHILE", "REPEAT",
                            "FUNCTION"))) {
                    next
                }

                # Skip incomplete statements (unclosed parens or
                # trailing operators)
                if (!body_is_complete(terms, body_range)) {
                    next
                }

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
                                        indent_str) + 4L # "{ " + " }"
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
                if (terms$token[body_start] == "'{'") {
                    next
                }

                cond_close_line <- terms$out_line[cond_close]
                body_start_line <- terms$out_line[body_start]
                target_level <- terms$nesting_level[ci] + 1L

                if (body_start_line != cond_close_line) {
                    # Body already on a different line.
                    # Check if it spans multiple lines — if so,
                    # must add braces for valid R.
                    body_end <- find_bare_body_end(terms, body_start)
                    if (terms$out_line[body_end] > body_start_line) {
                        # Multi-line bare body — add braces
                        body_range <- seq(body_start, body_end)
                        has_else <- body_end + 1L <= n &&
                        terms$token[body_end + 1L] == "ELSE"
                        body_end_line <- terms$out_line[body_end]

                        open_brace <- make_token("'{'", "{",
                            out_line = cond_close_line,
                            out_order = terms$out_order[cond_close] + 0.5)
                        close_brace <- make_token("'}'", "}",
                            out_line = body_end_line + 1L,
                            out_order = terms$out_order[body_start] - 0.3)

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
                        terms <- terms[order(terms$out_line, terms$out_order),]
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
                if (any(body_toks %in% c("IF", "FOR", "WHILE", "REPEAT",
                            "FUNCTION"))) {
                    next
                }

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
                terms <- terms[order(terms$out_line, terms$out_order),]
                terms$out_order <- seq_len(nrow(terms))
                changed <- TRUE
                break

            } else if (mode == "same_line") {
                cond_close_line <- terms$out_line[cond_close]

                # Skip if condition line has a trailing comment
                cond_line_toks <- terms$token[
                    terms$out_line == cond_close_line]
                if (any(cond_line_toks == "COMMENT")) {
                    next
                }

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
                    if (close_brace_idx > n) {
                        next
                    }

                    # Inner body tokens
                    inner_start <- open_brace_idx + 1L
                    inner_end <- close_brace_idx - 1L
                    if (inner_start > inner_end) {
                        next
                    }

                    inner_range <- seq(inner_start, inner_end)
                    inner_toks <- terms$token[inner_range]

                    # Skip if multi-line body
                    inner_lines <- unique(terms$out_line[inner_range])
                    if (length(inner_lines) > 1L) {
                        next
                    }

                    # Skip complex bodies (control flow, semicolons)
                    if (any(inner_toks %in% c("IF", "FOR", "WHILE",
                                "REPEAT", "FUNCTION", "COMMENT", "';'"))) {
                        next
                    }

                    # Skip if this if has an else (stripping braces
                    # would put else on a separate line)
                    has_else <- close_brace_idx + 1L <= n &&
                    terms$token[close_brace_idx + 1L] == "ELSE"
                    if (has_else) {
                        next
                    }

                    # Skip if combined line would exceed line_limit
                    cond_w <- ast_line_width(terms, cond_close_line, indent_str)
                    body_w <- ast_line_width(terms,
                        terms$out_line[inner_start], indent_str)
                    if (cond_w + 1L + body_w > line_limit) {
                        next
                    }

                    # Move inner tokens to condition line
                    terms$out_line[inner_range] <- cond_close_line

                    # Remove brace tokens
                    remove_idx <- c(open_brace_idx, close_brace_idx)
                    terms <- terms[-remove_idx,]
                    terms <- terms[order(terms$out_line, terms$out_order),]
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
                if (any(body_toks %in% c("IF", "FOR", "WHILE", "REPEAT",
                            "FUNCTION", "COMMENT"))) {
                    next
                }

                # Skip incomplete statements
                if (!body_is_complete(terms, body_range)) {
                    next
                }

                # Skip if combined line would exceed line_limit
                cond_w <- ast_line_width(terms, cond_close_line, indent_str)
                body_w <- ast_line_width(terms, body_start_line, indent_str)
                if (cond_w + 1L + body_w > line_limit) {
                    next
                }

                terms$out_line[body_range] <- cond_close_line
                terms <- renumber_lines(terms)
                terms <- terms[order(terms$out_line, terms$out_order),]
                terms$out_order <- seq_len(nrow(terms))
                changed <- TRUE
                break
            }
        }
    }

    terms
}

