#' Fix Else Placement
#'
#' Ensures `else` appears on the same line as the closing brace.
#'
#' @param code Code string.
#' @return Code with corrected else placement.
#' @keywords internal
fix_else_placement <- function (code) {
    gsub("\\}\\s*\n\\s*else\\b", "} else", code)
}

#' Add Braces to One-Liner Control Flow
#'
#' Adds braces to single-statement `if`, `for`, `while` bodies:
#' `if (x) y` becomes `if (x) { y }`.
#' Does not modify `if`/`else` used as expressions (value-producing).
#'
#' @param code Code string.
#' @return Code with braces added.
#' @keywords internal
add_control_braces <- function (code) {
    changed <- TRUE
    max_iterations <- 200L

    while (changed && max_iterations > 0) {
        max_iterations <- max_iterations - 1
        changed <- FALSE

        result <- add_one_control_brace(code)
        if (!is.null(result)) {
            code <- result
            changed <- TRUE
        }
    }

    code
}

#' Add Braces to One Control Flow Statement
#'
#' @param code Code string.
#' @return Modified code or NULL if no changes.
#' @keywords internal
add_one_control_brace <- function (code) {
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

    control_keywords <- c("IF", "FOR", "WHILE")

    for (i in seq_len(nrow(terminals))) {
        tok <- terminals[i,]
        if (!(tok$token %in% control_keywords)) { next }

        ctrl_line <- tok$line1

        # Skip if-else used as expression (RHS of assignment or function arg)
        if (tok$token == "IF") {
            before <- terminals[terminals$line1 == ctrl_line &
                terminals$col1 < tok$col1,]
            if (nrow(before) > 0 &&
                any(before$token %in% c("LEFT_ASSIGN", "EQ_ASSIGN",
                                        "RIGHT_ASSIGN"))) {
                next
            }
            # If this IF is preceded by ELSE, trace back the chain
            # to the original IF and check if it's an expression
            if (i > 1 && terminals$token[i - 1] == "ELSE") {
                skip_chain <- FALSE
                chain_idx <- i - 1
                while (chain_idx > 1 && terminals$token[chain_idx] == "ELSE") {
                    # Find the IF that owns this ELSE by scanning backwards
                    # past the body and condition of the preceding if
                    scan <- chain_idx - 1
                    depth <- 0
                    while (scan >= 1) {
                        st <- terminals$token[scan]
                        if (st %in% c("')'", "']'", "'}'", "']]'")) {
                            depth <- depth + 1
                        } else if (st %in% c("'('", "'['", "'{'")) {
                            depth <- depth - 1
                        } else if (st == "LBB") {
                            depth <- depth - 2
                        }
                        if (depth <= 0 && st == "IF") {
                            break
                        }
                        scan <- scan - 1
                    }
                    if (scan < 1 || terminals$token[scan] != "IF") {
                        break
                    }
                    # Check if there's an ELSE before this IF too
                    if (scan > 1 && terminals$token[scan - 1] == "ELSE") {
                        chain_idx <- scan - 1
                    } else {
                        # Found the root IF - check if it's an expression
                        root_line <- terminals$line1[scan]
                        root_before <- terminals[
                            terminals$line1 == root_line &
                            terminals$col1 < terminals$col1[scan],]
                        if (nrow(root_before) > 0 &&
                            any(root_before$token %in% c("LEFT_ASSIGN",
                                    "EQ_ASSIGN", "RIGHT_ASSIGN"))) {
                            skip_chain <- TRUE
                        }
                        break
                    }
                }
                if (skip_chain) { next }
            }
            # Skip if-else used as expression inside unclosed parens/brackets
            all_before <- terminals[seq_len(nrow(terminals)) < i,]
            paren_balance <- 0
            bracket_balance <- 0
            for (bi in seq_len(nrow(all_before))) {
                btk <- all_before$token[bi]
                if (btk == "'('") {
                    paren_balance <- paren_balance + 1
                } else if (btk == "')'") {
                    paren_balance <- paren_balance - 1
                } else if (btk == "'['") {
                    bracket_balance <- bracket_balance + 1
                } else if (btk == "']'") {
                    bracket_balance <- bracket_balance - 1
                } else if (btk == "LBB") {
                    bracket_balance <- bracket_balance + 2
                } else if (btk == "']]'") {
                    bracket_balance <- bracket_balance - 2
                }
            }
            if (paren_balance > 0 || bracket_balance > 0) { next }
        }

        # Find the opening ( after the keyword
        open_idx <- i + 1
        if (open_idx > nrow(terminals)) { next }
        if (terminals$token[open_idx] != "'('") { next }

        # Find matching )
        paren_depth <- 1
        close_paren_idx <- open_idx + 1
        while (close_paren_idx <= nrow(terminals) && paren_depth > 0) {
            if (terminals$token[close_paren_idx] == "'('") {
                paren_depth <- paren_depth + 1
            } else if (terminals$token[close_paren_idx] == "')'") {
                paren_depth <- paren_depth - 1
            }
            if (paren_depth > 0) { close_paren_idx <- close_paren_idx + 1 }
        }

        if (close_paren_idx > nrow(terminals)) { next }

        # Check next token after )
        body_start_idx <- close_paren_idx + 1
        if (body_start_idx > nrow(terminals)) { next }

        # Skip trailing comments on the condition line
        # e.g. if (cond) # comment\n    body
        cond_comment <- NULL
        if (terminals$token[body_start_idx] == "COMMENT" &&
            terminals$line1[body_start_idx] == terminals$line1[close_paren_idx]) {
            cond_comment <- terminals$text[body_start_idx]
            body_start_idx <- body_start_idx + 1
            if (body_start_idx > nrow(terminals)) { next }
        }

        body_start <- terminals[body_start_idx,]

        # Already has braces - skip
        if (body_start$token == "'{'") { next }

        # Body must be on the same line as closing paren (or next line
        # if there was a comment)
        if (is.null(cond_comment)) {
            if (body_start$line1 != terminals$line1[close_paren_idx]) { next }
        } else {
            # With trailing comment, body should be on the next line(s)
            if (body_start$line1 <= terminals$line1[close_paren_idx]) { next }
        }

        # Find end of body expression
        body_end_idx <- body_start_idx
        body_depth <- 0

        while (body_end_idx <= nrow(terminals)) {
            btok <- terminals[body_end_idx,]
            if (btok$token == "LBB") {
                body_depth <- body_depth + 2
            } else if (btok$token %in% c("'('", "'['", "'[['", "'{'")) {
                body_depth <- body_depth + 1
            } else if (btok$token %in% c("')'", "']'", "']]'", "'}'")) {
                body_depth <- body_depth - 1
                if (body_depth < 0) { break }
            }

            if (body_depth == 0) {
                next_idx <- body_end_idx + 1
                if (next_idx > nrow(terminals)) { break }

                next_tok <- terminals[next_idx,]
                if (next_tok$token == "ELSE") { break }

                if (next_tok$line1 > btok$line1) {
                    # If current line ends with assignment, RHS continues
                    if (btok$token %in% c("LEFT_ASSIGN", "EQ_ASSIGN",
                            "RIGHT_ASSIGN")) {
                        body_end_idx <- body_end_idx + 1
                        next
                    }
                    cont_tokens <- c("'+'", "'-'", "'*'", "'/'", "'^'",
                                     "SPECIAL", "AND", "OR", "AND2", "OR2",
                                     "GT", "LT", "GE", "LE", "EQ", "NE",
                                     "LEFT_ASSIGN", "EQ_ASSIGN", "'~'", "','")
                    if (!(next_tok$token %in% cont_tokens)) { break }
                }
            }

            body_end_idx <- body_end_idx + 1
        }

        if (body_end_idx > nrow(terminals)) {
            body_end_idx <- nrow(terminals)
        }

        # If body walk hit an enclosing close bracket (e.g., else body }
        # where } closes outer block), skip — adding braces here would
        # capture the enclosing bracket inside the new block
        if (body_depth < 0) { next }

        body_tokens <- terminals[body_start_idx:body_end_idx,]
        # If body spans multiple lines, skip to avoid collapsing blocks
        body_span_end <- max(body_tokens$line2)
        if (body_span_end > body_start$line1) { next }
        # Conservative guard: avoid rewriting complex one-liners where
        # nested control/function expressions can be mis-associated with
        # the outer `if`/`for`/`while`.
        complex_body_tokens <- c("IF", "ELSE", "FOR", "WHILE", "FUNCTION")
        if (any(body_tokens$token %in% complex_body_tokens)) { next }
        # Skip incomplete tail expressions that continue on the next line.
        continuation_tail_tokens <- c("LEFT_ASSIGN", "EQ_ASSIGN",
                                      "RIGHT_ASSIGN", "'+'", "'-'", "'*'",
                                      "'/'", "'^'", "SPECIAL", "AND", "OR",
                                      "AND2", "OR2", "GT", "LT", "GE", "LE",
                                      "EQ", "NE", "'~'", "','")
        if (body_tokens$token[nrow(body_tokens)] %in% continuation_tail_tokens) {
            next
        }
        body_has_comment <- any(body_tokens$token == "COMMENT")
        body_text <- format_line_tokens(body_tokens)

        # If there was a trailing comment on the condition line,
        # force multi-line and build the opening brace with comment
        if (!is.null(cond_comment)) {
            body_has_comment <- TRUE
        }
        open_brace_suffix <- if (!is.null(cond_comment)) {
            paste0(" { ", cond_comment)
        } else {
            " {"
        }

        # Compute extra depth if control keyword is inside an open block on the same line
        ctrl_indent <- sub("^(\\s*).*", "\\1", lines[ctrl_line])
        before_on_line <- terminals[terminals$line1 == ctrl_line &
            terminals$col1 < tok$col1,]
        extra_depth <- 0
        if (nrow(before_on_line) > 0) {
            extra_depth <- extra_depth +
            sum(before_on_line$token %in% c("'{'", "'('", "'['")) +
            2L * sum(before_on_line$token == "LBB") -
            sum(before_on_line$token %in% c("'}'", "')'", "']'")) -
            2L * sum(before_on_line$token == "']]'")
            if (extra_depth < 0) { extra_depth <- 0 }
        }
        inner_indent <- paste0(ctrl_indent, strrep("    ", extra_depth + 1L))

        has_else <- FALSE
        else_idx <- body_end_idx + 1
        if (else_idx <= nrow(terminals) &&
            terminals$token[else_idx] == "ELSE") {
            has_else <- TRUE
        }

        # Build prefix: everything up to and including )
        # When condition spans multiple lines, use the close paren's line
        close_paren_line <- terminals$line1[close_paren_idx]
        close_paren_col <- terminals$col2[close_paren_idx]
        paren_line_content <- lines[close_paren_line]
        prefix <- substring(paren_line_content, 1,
                            col_to_charpos(paren_line_content, close_paren_col))

        # Lines before the close paren line (for multi-line conditions)
        if (ctrl_line > 1) {
            pre_lines <- lines[seq_len(ctrl_line - 1)]
        } else {
            pre_lines <- character(0)
        }
        if (close_paren_line > ctrl_line) {
            pre_lines <- c(pre_lines, lines[ctrl_line:(close_paren_line - 1)])
        }

        if (has_else) {
            else_tok <- terminals[else_idx,]
            else_body_idx <- else_idx + 1
            if (else_body_idx > nrow(terminals)) { next }

            else_body_start <- terminals[else_body_idx,]

            if (else_body_start$token == "'{'") {
                else_body_line <- else_body_start$line1
                new_line <- paste0(prefix, " { ", body_text, " } else {")
                if (body_has_comment || nchar(new_line) > 80L) {
                    new_lines_vec <- c(paste0(prefix, open_brace_suffix),
                                       paste0(inner_indent, body_text),
                                       paste0(ctrl_indent, "} else {"))
                    new_code_lines <- c(pre_lines, new_lines_vec,
                        if (else_body_line < length(lines)) {
                            lines[seq(else_body_line + 1, length(lines))]
                        } else {
                            character(0)
                        })
                } else {
                    new_code_lines <- c(pre_lines, new_line,
                        if (else_body_line < length(lines)) {
                            lines[seq(else_body_line + 1, length(lines))]
                        } else {
                            character(0)
                        })
                }
            } else if (else_body_start$token == "IF") {
                new_line <- paste0(prefix, " { ", body_text, " } else")
                # The IF may be on the same line as ELSE — reconstruct
                # the "if ..." part from the IF token's column onward
                ctrl_indent <- sub("^(\\s*).*", "\\1", lines[ctrl_line])
                if_rest <- paste0(ctrl_indent,
                                  trimws(substring(
                            lines[else_body_start$line1],
                            else_body_start$col1)))
                if (body_has_comment || nchar(new_line) > 80L) {
                    new_lines_vec <- c(paste0(prefix, open_brace_suffix),
                                       paste0(inner_indent, body_text),
                                       paste0(ctrl_indent, "} else"))
                    new_code_lines <- c(pre_lines, new_lines_vec, if_rest,
                        if (else_body_start$line1 < length(lines)) {
                            lines[seq(else_body_start$line1 + 1, length(lines))]
                        } else {
                            character(0)
                        })
                } else {
                    new_code_lines <- c(pre_lines, new_line, if_rest,
                        if (else_body_start$line1 < length(lines)) {
                            lines[seq(else_body_start$line1 + 1, length(lines))]
                        } else {
                            character(0)
                        })
                }
            } else {
                # Both branches bare
                else_end_idx <- else_body_idx
                else_depth <- 0
                while (else_end_idx <= nrow(terminals)) {
                    etok <- terminals[else_end_idx,]
                    if (etok$token == "LBB") {
                        else_depth <- else_depth + 2
                    } else if (etok$token %in% c("'('", "'['", "'[['", "'{'")) {
                        else_depth <- else_depth + 1
                    } else if (etok$token %in% c("')'", "']'", "']]'", "'}'")) {
                        else_depth <- else_depth - 1
                        if (else_depth < 0) { break }
                    }
                    if (else_depth == 0) {
                        next_idx2 <- else_end_idx + 1
                        if (next_idx2 > nrow(terminals)) { break }
                        if (terminals$line1[next_idx2] > etok$line1) { break }
                    }
                    else_end_idx <- else_end_idx + 1
                }
                if (else_end_idx > nrow(terminals)) {
                    else_end_idx <- nrow(terminals)
                }

                # If else body walk hit an enclosing close bracket,
                # skip — adding braces would capture it inside the block
                if (else_depth < 0) { next }

                else_body_tokens <- terminals[else_body_idx:else_end_idx,]
                if (any(else_body_tokens$token %in% complex_body_tokens)) {
                    next
                }
                if (else_body_tokens$token[nrow(else_body_tokens)] %in% continuation_tail_tokens) {
                    next
                }
                else_has_comment <- any(else_body_tokens$token == "COMMENT")
                else_body_text <- format_line_tokens(else_body_tokens)
                else_end_line <- terminals$line1[else_end_idx]

                new_line <- paste0(prefix, " { ", body_text, " } else { ",
                                   else_body_text, " }")
                if (body_has_comment || else_has_comment ||
                    nchar(new_line) > 80L) {
                    new_lines_vec <- c(paste0(prefix, open_brace_suffix),
                                       paste0(inner_indent, body_text),
                                       paste0(ctrl_indent, "} else {"),
                                       paste0(inner_indent, else_body_text),
                                       paste0(ctrl_indent, "}"))
                    new_code_lines <- c(pre_lines, new_lines_vec,
                        if (else_end_line < length(lines)) {
                            lines[seq(else_end_line + 1, length(lines))]
                        } else {
                            character(0)
                        })
                } else {
                    new_code_lines <- c(pre_lines, new_line,
                        if (else_end_line < length(lines)) {
                            lines[seq(else_end_line + 1, length(lines))]
                        } else {
                            character(0)
                        })
                }
            }
        } else {
            body_end_line <- terminals$line1[body_end_idx]
            new_line <- paste0(prefix, " { ", body_text, " }")
            if (body_has_comment || nchar(new_line) > 80L) {
                new_lines_vec <- c(paste0(prefix, open_brace_suffix),
                                   paste0(inner_indent, body_text),
                                   paste0(ctrl_indent, "}"))
                new_code_lines <- c(pre_lines, new_lines_vec,
                    if (body_end_line < length(lines)) {
                        lines[seq(body_end_line + 1, length(lines))]
                    } else {
                        character(0)
                    })
            } else {
                new_code_lines <- c(pre_lines, new_line,
                    if (body_end_line < length(lines)) {
                        lines[seq(body_end_line + 1, length(lines))]
                    } else {
                        character(0)
                    })
            }
        }

        result <- paste(new_code_lines, collapse = "\n")
        if (!grepl("\n$", result) && nchar(result) > 0) {
            result <- paste0(result, "\n")
        }

        return(result)
    }

    # add_one_control_brace: no changes
    NULL
}

