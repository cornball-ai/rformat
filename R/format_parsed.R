#' Format R Code Using Token-Based Parsing
#'
#' Internal function to format R code using getParseData tokens.
#' Calculates proper indentation based on nesting depth.
#'
#' @param code Character string of R code.
#' @param indent Integer for spaces (default 4), or character string for
#'   literal indent (e.g., `"\\t\\t"` for vintage R Core style).
#' @param wrap Continuation style: `"paren"` (default) aligns to opening
#'   parenthesis, `"fixed"` uses 8-space indent.
#' @param expand_if Expand inline if-else to multi-line (default FALSE).
#' @param brace_style Brace placement: `"kr"` (same line) or `"allman"` (new line).
#' @param line_limit Maximum line length before wrapping (default 80).
#' @return Formatted code as character string.
#' @importFrom utils getParseData
#' @keywords internal
format_tokens <- function (code, indent = 4L, wrap = "paren",
                           expand_if = FALSE, brace_style = "kr",
                           line_limit = 80L, postprocess = TRUE) {
    # Parse with source tracking
    parsed <- tryCatch(
        parse(text = code, keep.source = TRUE),
        error = function (e) NULL
    )

    if (is.null(parsed)) {
        warning("Could not parse code, returning unchanged")
        return(code)
    }

    pd <- getParseData(parsed)

    if (is.null(pd) || nrow(pd) == 0) {
        return(code)
    }

    # Get terminal tokens
    terminals <- pd[pd$terminal,]
    terminals <- terminals[order(terminals$line1, terminals$col1),]

    # Split original into lines for comment preservation
    orig_lines <- strsplit(code, "\n", fixed = TRUE)[[1]]
    is_large_file <- length(orig_lines) > 1500L

    # Fix truncated string constants from getParseData()
    # Long STR_CONST tokens (~1000+ chars) get truncated to
    # '[N chars quoted with "..."]' — recover from source lines
    for (i in seq_len(nrow(terminals))) {
        tok <- terminals[i,]
        if (tok$token == "STR_CONST" &&
            grepl("^\\[\\d+ chars quoted", tok$text)) {
            if (tok$line1 == tok$line2) {
                terminals$text[i] <- substring(orig_lines[tok$line1],
                    col_to_charpos(orig_lines[tok$line1], tok$col1),
                    col_to_charpos(orig_lines[tok$line1], tok$col2))
            } else {
                parts <- c(
                    substring(orig_lines[tok$line1],
                        col_to_charpos(orig_lines[tok$line1], tok$col1)),
                    if (tok$line2 > tok$line1 + 1) {
                    orig_lines[(tok$line1 + 1):(tok$line2 - 1)]
                    },
                    substring(orig_lines[tok$line2], 1,
                        col_to_charpos(orig_lines[tok$line2], tok$col2)))
                terminals$text[i] <- paste(parts, collapse = "\n")
            }
        }
    }

    # Track nesting for indentation
    brace_depth <- 0
    paren_depth <- 0
    paren_at_brace <- integer(0) # stack: paren_depth when each { opened

    # First pass: calculate nesting depth at end of each line
    max_line <- max(c(terminals$line1, length(orig_lines)))
    line_end_brace <- integer(max_line)
    line_end_paren <- integer(max_line)
    line_end_pab <- integer(max_line) # paren_at_brace top-of-stack

    for (i in seq_len(nrow(terminals))) {
        tok <- terminals[i,]
        ln <- tok$line1

        if (tok$token == "'{'") {
            paren_at_brace <- c(paren_at_brace, paren_depth)
            brace_depth <- brace_depth + 1
        } else if (tok$token == "'}'") {
            brace_depth <- max(0, brace_depth - 1)
            if (length(paren_at_brace) > 0) {
                paren_at_brace <- paren_at_brace[-length(paren_at_brace)]
            }
        } else if (tok$token %in% c("'('", "'['", "LBB")) {
            paren_depth <- paren_depth + if (tok$token == "LBB") 2L else 1L
        } else if (tok$token %in% c("')'", "']'")) {
            paren_depth <- max(0, paren_depth - 1)
        }

        line_end_brace[ln] <- brace_depth
        line_end_paren[ln] <- paren_depth
        line_end_pab[ln] <- if (length(paren_at_brace) > 0) {
            paren_at_brace[length(paren_at_brace)]
        } else {
            0L
        }
    }

    # Fill in gaps (comment/blank lines inherit from previous)
    for (ln in seq_len(max_line)) {
        if (ln > 1 && line_end_brace[ln] == 0 && line_end_paren[ln] == 0) {
            if (nrow(terminals[terminals$line1 == ln,]) == 0) {
                line_end_brace[ln] <- line_end_brace[ln - 1]
                line_end_paren[ln] <- line_end_paren[ln - 1]
                line_end_pab[ln] <- line_end_pab[ln - 1]
            }
        }
    }

    # Calculate indent at START of each line
    line_indent <- integer(max_line)
    for (ln in seq_len(max_line)) {
        if (ln == 1) {
            line_indent[ln] <- 0
        } else {
            prev_brace <- line_end_brace[ln - 1]
            prev_paren <- line_end_paren[ln - 1]

            line_tokens <- terminals[terminals$line1 == ln,]
            if (nrow(line_tokens) > 0 && line_tokens$token[1] == "'}'") {
                prev_brace <- max(0, prev_brace - 1)
            }
            if (nrow(line_tokens) > 0 &&
                line_tokens$token[1] %in% c("')'", "']'")) {
                prev_paren <- max(0, prev_paren - 1)
            }

            # Subtract paren depth from before the innermost brace,
            # so only parens opened *inside* the current brace block
            # contribute to indentation (fixes brace-inside-paren)
            prev_pab <- line_end_pab[ln - 1]
            if (nrow(line_tokens) > 0 && line_tokens$token[1] == "'}'") {
                prev_pab <- line_end_pab[ln]
            }
            line_indent[ln] <- prev_brace + max(0L, prev_paren - prev_pab)
        }
    }

    # Build output
    if (is.character(indent)) {
        indent_str <- indent
    } else {
        indent_str <- strrep(" ", indent)
    }
    out_lines <- character(length(orig_lines))

    # Track lines that are internal to multi-line tokens (e.g., multi-line strings)
    # A line should be skipped if:
    # 1. It's within the span of a multi-line token (between line1+1 and line2)
    # 2. AND no tokens START on that line (line1 == line_num)
    # This handles the case where a multi-line string ends on the same line as
    # other tokens (e.g., closing parenthesis)
    multiline_covered <- logical(length(orig_lines))
    for (i in seq_len(nrow(terminals))) {
        tok <- terminals[i,]
        if (tok$line2 > tok$line1) {
            # This token spans multiple lines - mark continuation lines
            for (ln in (tok$line1 + 1):tok$line2) {
                if (ln <= length(multiline_covered)) {
                    multiline_covered[ln] <- TRUE
                }
            }
        }
    }

    for (line_num in seq_along(orig_lines)) {
        line <- orig_lines[line_num]

        # Check if any tokens START on this line
        line_tokens <- terminals[terminals$line1 == line_num,]

        # Skip lines that are purely internal to multi-line tokens
        # (covered by a multi-line token AND no tokens start here)
        if (multiline_covered[line_num] && nrow(line_tokens) == 0) {
            out_lines[line_num] <- NA_character_
            next
        }

        if (grepl("^\\s*$", line)) {
            out_lines[line_num] <- ""
            next
        }

        if (nrow(line_tokens) == 0) {
            out_lines[line_num] <- line
            next
        }

        if (nrow(line_tokens) == 1 && line_tokens$token[1] == "COMMENT") {
            if (grepl("^#'", trimws(line))) {
                out_lines[line_num] <- trimws(line)
            } else {
                if (line_num <= max_line) {
                    line_level <- line_indent[line_num]
                } else {
                    line_level <- 0
                }
                out_lines[line_num] <- paste0(
                    paste0(rep(indent_str, line_level), collapse = ""),
                    trimws(line))
            }
            next
        }

        if (line_num <= max_line) {
            line_level <- line_indent[line_num]
        } else {
            line_level <- 0
        }
        formatted <- format_line_tokens(line_tokens)

        out_lines[line_num] <- paste0(
            paste0(rep(indent_str, line_level), collapse = ""), formatted)
    }

    # Filter out NA lines (multi-line token continuations)
    out_lines <- out_lines[!is.na(out_lines)]
    result <- paste(out_lines, collapse = "\n")
    if (!grepl("\n$", result) && nchar(result) > 0) {
        result <- paste0(result, "\n")
    }

    if (!isTRUE(postprocess) || is_large_file) {
        return(result)
    }

    # Collapse multi-line calls that fit on one line
    result <- apply_if_parseable(result, collapse_calls)

    # Add braces to one-liner control flow (before wrapping, so bare
    # bodies move to their own lines before line-length decisions)
    result <- apply_if_parseable(result, add_control_braces)

    # Wrap long lines at operators (||, &&), then at commas
    result <- apply_if_parseable(result, wrap_long_operators,
        line_limit = line_limit)
    result <- apply_if_parseable(result, wrap_long_calls,
        line_limit = line_limit)

    # Reformat function definitions
    result <- apply_if_parseable(result, reformat_function_defs, wrap = wrap,
        brace_style = brace_style, line_limit = line_limit)
    # Function-def rewrites can expose bare one-line control flow.
    result <- apply_if_parseable(result, add_control_braces)

    # Reformat inline if-else to multi-line
    # Always expand long lines; optionally expand all
    result <- apply_if_parseable(result, reformat_inline_if,
        line_limit = if (expand_if) 0L else line_limit)

    # Final wrap pass: earlier passes may have produced new long lines
    result <- apply_if_parseable(result, wrap_long_operators,
        line_limit = line_limit)
    result <- apply_if_parseable(result, wrap_long_calls,
        line_limit = line_limit)
    # Re-run function/control normalization after final wraps so a single
    # rformat() call does not rely on a second external formatting pass.
    result <- apply_if_parseable(result, reformat_function_defs, wrap = wrap,
        brace_style = brace_style, line_limit = line_limit)
    result <- apply_if_parseable(result, add_control_braces)
    result <- apply_if_parseable(result, wrap_long_operators,
        line_limit = line_limit)
    result <- apply_if_parseable(result, wrap_long_calls,
        line_limit = line_limit)

    # Canonicalize indentation/spacing after structural rewrites.
    canonical <- format_tokens(result, indent = indent, wrap = wrap,
        expand_if = expand_if, brace_style = brace_style,
        line_limit = line_limit, postprocess = FALSE)

    # Canonical spacing can create newly-overlong lines; wrap once more and
    # re-canonicalize so a single call reaches a stable normal form.
    canonical <- apply_if_parseable(canonical, add_control_braces)
    canonical <- apply_if_parseable(canonical, collapse_calls)
    canonical <- apply_if_parseable(canonical, wrap_long_operators,
        line_limit = line_limit)
    canonical <- apply_if_parseable(canonical, wrap_long_calls,
        line_limit = line_limit)
    format_tokens(canonical, indent = indent, wrap = wrap,
        expand_if = expand_if, brace_style = brace_style,
        line_limit = line_limit, postprocess = FALSE)
}

#' Reformat Function Definitions
#'
#' Ensures function definitions follow style guide:
#' - Short signatures on one line
#' - Long signatures wrap with continuation indent
#' - Brace on same line (K&R) or own line (Allman)
#'
#' @param code Formatted code string.
#' @param wrap Continuation style: `"paren"` or `"fixed"`.
#' @param brace_style Brace placement: `"kr"` (same line) or `"allman"` (new line).
#' @param line_limit Maximum line length before wrapping (default 80).
#' @return Code with reformatted function definitions.
#' @keywords internal
reformat_function_defs <- function (code, wrap = "paren", brace_style = "kr",
                                    line_limit = 80L) {
    # Process one function at a time, re-parsing each time
    # to handle line number changes
    changed <- TRUE
    max_iterations <- iteration_budget(code, 200L, mode = "funcdef")

    while (changed && max_iterations > 0) {
        max_iterations <- max_iterations - 1
        changed <- FALSE

        result <- reformat_one_function(code, wrap = wrap,
            brace_style = brace_style, line_limit = line_limit)
        if (!is.null(result)) {
            code <- result
            changed <- TRUE
        }
    }

    code
}

#' Reformat One Function Definition
#'
#' Uses R Core continuation style: args on one line if they fit,
#' otherwise wrap with continuation indent.
#'
#' @param code Code string.
#' @param wrap Continuation style: `"paren"` aligns to opening parenthesis,
#'   `"fixed"` uses 8-space indent.
#' @param brace_style Brace placement: `"kr"` (same line) or `"allman"` (new line).
#' @param line_limit Maximum line length before wrapping (default 80).
#' @return Modified code or NULL if no changes.
#' @keywords internal
reformat_one_function <- function (code, wrap = "paren", brace_style = "kr",
                                   line_limit = 80L) {
    parsed <- tryCatch(
        parse(text = code, keep.source = TRUE),
        error = function (e) NULL
    )

    if (is.null(parsed)) {
        return(NULL)
    }

    pd <- getParseData(parsed)
    if (is.null(pd) || nrow(pd) == 0) {
        return(NULL)
    }

    terminals <- pd[pd$terminal,]
    terminals <- terminals[order(terminals$line1, terminals$col1),]

    func_indices <- which(terminals$token == "FUNCTION")

    if (length(func_indices) == 0) {
        return(NULL)
    }

    lines <- strsplit(code, "\n", fixed = TRUE)[[1]]

    # Find first function that needs reformatting
    for (fi in func_indices) {
        func_tok <- terminals[fi,]
        func_line <- func_tok$line1

        # Only rewrite function definitions assigned to a name. Anonymous
        # function literals in defaults/calls are too unstable here.
        prev_idx <- fi - 1L
        while (prev_idx >= 1L && terminals$token[prev_idx] == "COMMENT") {
            prev_idx <- prev_idx - 1L
        }
        prev_tok <- if (prev_idx >= 1L) terminals$token[prev_idx] else NA_character_
        if (!(prev_tok %in% c("LEFT_ASSIGN", "EQ_ASSIGN", "RIGHT_ASSIGN"))) {
            next
        }

        # Find the opening ( after function
        next_idx <- fi + 1
        if (next_idx > nrow(terminals)) { next }
        if (terminals$token[next_idx] != "'('") { next }

        # Find matching closing )
        paren_depth <- 1
        close_idx <- next_idx + 1
        while (close_idx <= nrow(terminals) && paren_depth > 0) {
            if (terminals$token[close_idx] == "'('") {
                paren_depth <- paren_depth + 1
            } else if (terminals$token[close_idx] == "')'") {
                paren_depth <- paren_depth - 1
            }
            if (paren_depth > 0) { close_idx <- close_idx + 1 }
        }

        if (close_idx > nrow(terminals)) { next }

        close_paren_line <- terminals$line1[close_idx]

        # Check if there's a { after the )
        has_brace <- close_idx + 1 <= nrow(terminals) &&
        terminals$token[close_idx + 1] == "'{'"
        if (has_brace) {
            brace_line <- terminals$line1[close_idx + 1]
        } else {
            brace_line <- NA
        }

        # Find what comes before 'function' on the line using token position
        func_line_content <- lines[func_line]
        base_indent <- sub("^(\\s*).*", "\\1", func_line_content)
        # func_tok$col1 is the 1-based column position of "function"
        if (func_tok$col1 > 1) {
            prefix <- substring(func_line_content, 1,
                col_to_charpos(func_line_content, func_tok$col1 - 1))
        } else {
            prefix <- ""
        }

        # Collect all formals with their defaults
        formal_texts <- character(0)
        i <- next_idx + 1
        while (i < close_idx) {
            tok <- terminals[i,]

            if (tok$token == "SYMBOL_FORMALS") {
                formal_name <- tok$text
                formal_text <- formal_name

                # Check for default value (EQ_FORMALS followed by value)
                if (i + 1 < close_idx &&
                    terminals$token[i + 1] == "EQ_FORMALS") {
                    formal_text <- paste0(formal_name, " = ")
                    i <- i + 2

                    # Collect the default value tokens
                    value_tokens <- list()
                    value_paren_depth <- 0

                    while (i < close_idx) {
                        vtok <- terminals[i,]
                        # Skip comments inside formals (e.g. # type: bool)
                        if (vtok$token == "COMMENT") {
                            i <- i + 1
                            next
                        }
                        if (vtok$token == "'('") {
                            value_paren_depth <- value_paren_depth + 1
                        }
                        if (vtok$token == "')'") {
                            if (value_paren_depth == 0) { break }
                            value_paren_depth <- value_paren_depth - 1
                        }
                        if (vtok$token == "','" && value_paren_depth == 0) {
                            break
                        }

                        value_tokens[[length(value_tokens) + 1]] <- vtok
                        i <- i + 1
                    }

                    # Format value tokens properly
                    if (length(value_tokens) > 0) {
                        value_df <- do.call(rbind,
                            lapply(value_tokens, as.data.frame))
                        formal_text <- paste0(formal_text,
                            format_line_tokens(value_df))
                    }
                }

                formal_texts <- c(formal_texts, formal_text)
            }

            i <- i + 1
        }

        # Build single-line signature: prefix + function (arg1, arg2, ...)
        single_line_sig <- paste0(prefix, "function (",
            paste(formal_texts, collapse = ", "), ")")

        # Account for " {" suffix when using K&R brace style
        if (has_brace && brace_style == "kr") {
            sig_limit <- line_limit - 2L
        } else {
            sig_limit <- line_limit
        }

        # Check if single line fits
        if (nchar(single_line_sig) <= sig_limit) {
            # Single line style
            new_lines <- single_line_sig
        } else {
            # Continuation style - wrap at commas
            if (wrap == "fixed") {
                cont_indent <- strrep(" ", 8L)
            } else {
                # Align to opening paren
                cont_indent <- strrep(" ", nchar(prefix) + nchar("function ("))
            }
            new_lines <- character(0)
            current_line <- paste0(prefix, "function (")

            for (j in seq_along(formal_texts)) {
                arg_with_comma <- if (j < length(formal_texts)) {
                    paste0(formal_texts[j], ", ")
                } else {
                    paste0(formal_texts[j], ")")
                }

                # Would adding this arg exceed the limit?
                test_line <- paste0(current_line, arg_with_comma)
                if (nchar(test_line) > sig_limit &&
                    nchar(current_line) > nchar(cont_indent)) {
                    # Wrap: save current line (remove trailing space if any)
                    new_lines <- c(new_lines, sub(" $", "", current_line))
                    current_line <- paste0(cont_indent, arg_with_comma)
                } else {
                    current_line <- test_line
                }
            }
            # Add final line
            new_lines <- c(new_lines, sub(" $", "", current_line))
        }

        # Check if there's a function body without braces (inline body)
        has_inline_body <- FALSE
        inline_body <- ""
        if (!has_brace && close_idx + 1 <= nrow(terminals)) {
            # Find end of inline body expression by tracking depth
            body_end_idx <- close_idx + 1
            body_depth <- 0
            while (body_end_idx <= nrow(terminals)) {
                btok <- terminals[body_end_idx,]
                if (btok$token %in% c("'('", "'['", "'[['", "'{'")) {
                    body_depth <- body_depth + 1
                } else if (btok$token == "LBB") {
                    body_depth <- body_depth + 2
                } else if (btok$token %in% c("')'", "']'", "'}'")) {
                    body_depth <- body_depth - 1
                    if (body_depth < 0) {
                        break # exiting enclosing context
                    }
                }
                if (btok$token == "','" && body_depth == 0) {
                    break # comma in enclosing call
                }
                if (body_depth == 0 && body_end_idx + 1 <= nrow(terminals)) {
                    next_tok <- terminals[body_end_idx + 1,]
                    if (next_tok$line1 > btok$line1) {
                        # Line break at depth 0 — check for continuation
                        cont_tokens <- c("'+'", "'-'", "'*'", "'/'", "'^'",
                            "SPECIAL", "AND", "OR", "AND2", "OR2", "GT", "LT",
                            "GE", "LE", "EQ", "NE", "LEFT_ASSIGN", "EQ_ASSIGN",
                            "'~'", "PIPE")
                        if (!(next_tok$token %in% cont_tokens)) { break }
                    }
                }
                body_end_idx <- body_end_idx + 1
            }
            if (body_end_idx > nrow(terminals)) {
                body_end_idx <- nrow(terminals)
            }
            # Exclude the breaking token (comma or closing bracket)
            if (body_end_idx > close_idx + 1 ||
                (body_end_idx == close_idx + 1 && body_depth >= 0 &&
                    terminals$token[body_end_idx] != "','")) {
                body_tokens <- terminals[(close_idx + 1):body_end_idx,]
                # Exclude trailing comma/close if we stopped at one
                if (body_depth < 0 || terminals$token[body_end_idx] == "','") {
                    body_end_idx <- body_end_idx - 1
                    body_tokens <- terminals[(close_idx + 1):body_end_idx,]
                }
                # Skip if inline body is complex (multi-line or contains braces/control)
                if (any(body_tokens$line2 > body_tokens$line1) ||
                    any(body_tokens$token %in% c("'{'", "'}'", "IF", "ELSE"))) {
                    next
                }
                has_inline_body <- TRUE
                inline_body <- format_line_tokens(body_tokens)
            }
        }

        # Add brace or inline body
        if (has_brace) {
            brace_tok <- terminals[close_idx + 1,]
            if (brace_style == "kr") {
                # K&R: brace on same line as closing paren
                new_lines[length(new_lines)] <- paste0(
                    new_lines[length(new_lines)], " {")
            } else {
                # Allman: brace on its own line
                new_lines <- c(new_lines, paste0(base_indent, "{"))
            }
            # Preserve body content after { on the same line
            brace_rest <- substring(lines[brace_tok$line1],
                col_to_charpos(lines[brace_tok$line1], brace_tok$col2) + 1)
            if (nzchar(trimws(brace_rest))) {
                new_lines[length(new_lines)] <- paste0(
                    new_lines[length(new_lines)], brace_rest)
            }
        } else if (has_inline_body) {
            # Append inline body and any suffix (e.g., ", other_args)" in outer call)
            body_suffix <- ""
            if (body_end_idx <= nrow(terminals)) {
                last_body_line <- terminals$line1[body_end_idx]
                last_body_col <- terminals$col2[body_end_idx]
                rest_of_line <- substring(lines[last_body_line],
                    last_body_col + 1)
                if (nzchar(rest_of_line)) {
                    body_suffix <- rest_of_line
                }
            }
            new_lines[length(new_lines)] <- paste0(
                new_lines[length(new_lines)], " ", inline_body, body_suffix)
        }

        # Check if reformatting is actually needed
        end_line <- close_paren_line
        if (has_brace) {
            end_line <- brace_line
        } else if (has_inline_body) {
            end_line <- terminals$line1[body_end_idx]
        }

        old_lines <- lines[func_line:end_line]
        if (identical(old_lines, new_lines)) { next }

        # Replace the lines and return
        if (func_line > 1) {
            pre <- lines[seq_len(func_line - 1)]
        } else {
            pre <- character(0)
        }
        if (end_line < length(lines)) {
            post <- lines[seq(end_line + 1, length(lines))]
        } else {
            post <- character(0)
        }
        new_code_lines <- c(pre, new_lines, post)

        result <- paste(new_code_lines, collapse = "\n")
        if (!grepl("\n$", result)) {
            result <- paste0(result, "\n")
        }

        return(result)
    }

    # No function needed reformatting
    NULL
}

#' Reformat Inline If-Else Statements
#'
#' Expands inline if-else to multi-line format per style guide:
#' x <- if (cond) a else b
#' becomes:
#' if (cond) {
#'   x <- a
#' } else {
#'   x <- b
#' }
#'
#' @param code Formatted code string.
#' @param line_limit Only expand if-else on lines exceeding this limit.
#'   Use 0 to expand all inline if-else.
#' @return Code with reformatted inline if-else.
#' @keywords internal
reformat_inline_if <- function (code, line_limit = 0L) {
    changed <- TRUE
    max_iterations <- iteration_budget(code, 100L, mode = "inline_if")

    while (changed && max_iterations > 0) {
        max_iterations <- max_iterations - 1
        changed <- FALSE

        result <- reformat_one_inline_if(code, line_limit = line_limit)
        if (!is.null(result)) {
            code <- result
            changed <- TRUE
        }
    }

    code
}

#' Reformat One Inline If-Else Statement
#'
#' @param code Code string.
#' @param line_limit Only expand if-else on lines exceeding this limit.
#'   Use 0 to expand all inline if-else.
#' @return Modified code or NULL if no changes.
#' @keywords internal
reformat_one_inline_if <- function (code, line_limit = 0L) {
    parsed <- tryCatch(
        parse(text = code, keep.source = TRUE),
        error = function (e) NULL
    )

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

    # Find assignment followed by if on the same line
    assign_tokens <- c("LEFT_ASSIGN", "EQ_ASSIGN")

    for (i in seq_len(nrow(terminals))) {
        tok <- terminals[i,]

        if (!(tok$token %in% assign_tokens)) { next }

        assign_line <- tok$line1

        # Find IF token after assignment on same line
        # Skip if there's a FUNCTION token between assignment and IF
        if_idx <- NULL
        found_function <- FALSE
        for (j in (i + 1):nrow(terminals)) {
            if (j > nrow(terminals)) { break }
            next_tok <- terminals[j,]
            if (next_tok$line1 != assign_line) { break }
            if (next_tok$token == "FUNCTION") {
                found_function <- TRUE
                break
            }
            if (next_tok$token == "IF") {
                if_idx <- j
                break
            }
        }

        if (found_function || is.null(if_idx)) { next }

        # Skip if the IF is inside unclosed parens relative to the assignment
        # e.g., x <- c(if (a) b else d, ...) should not be expanded
        paren_bal <- 0L
        for (j in (i + 1):(if_idx - 1)) {
            if (j > nrow(terminals)) { break }
            jt <- terminals$token[j]
            if (jt == "'('") { paren_bal <- paren_bal + 1L }
            if (jt == "')'") { paren_bal <- paren_bal - 1L }
        }
        if (paren_bal > 0L) { next }

        # Found pattern: assignment followed by if
        # Now find the structure: if ( cond ) true_expr else false_expr

        # Get the variable being assigned (tokens before assignment)
        var_tokens <- terminals[terminals$line1 == assign_line &
            seq_len(nrow(terminals)) < i,]
        if (nrow(var_tokens) == 0) { next }

        var_name <- format_line_tokens(var_tokens)

        # Find opening paren after IF
        open_paren_idx <- if_idx + 1
        if (open_paren_idx > nrow(terminals)) { next }
        if (terminals$token[open_paren_idx] != "'('") { next }

        # Find matching closing paren for condition
        paren_depth <- 1
        close_paren_idx <- open_paren_idx + 1
        while (close_paren_idx <= nrow(terminals) && paren_depth > 0) {
            if (terminals$token[close_paren_idx] == "'('") {
                paren_depth <- paren_depth + 1
            } else if (terminals$token[close_paren_idx] == "')'") {
                paren_depth <- paren_depth - 1
            }
            if (paren_depth > 0) { close_paren_idx <- close_paren_idx + 1 }
        }

        if (close_paren_idx > nrow(terminals)) { next }

        # Extract condition tokens
        cond_tokens <- terminals[(open_paren_idx + 1):(close_paren_idx - 1),]
        cond_text <- format_line_tokens(cond_tokens)

        # Find ELSE token
        else_idx <- NULL
        # Track nesting to find the right ELSE
        search_idx <- close_paren_idx + 1
        nest_depth <- 0
        brace_depth <- 0

        while (search_idx <= nrow(terminals)) {
            stok <- terminals[search_idx,]

            # Track brace boundaries — stop if we exit the enclosing block
            if (stok$token == "'{'") {
                brace_depth <- brace_depth + 1
            } else if (stok$token == "'}'") {
                brace_depth <- brace_depth - 1
                if (brace_depth < 0) { break }
            }

            if (stok$token == "IF") {
                nest_depth <- nest_depth + 1
            } else if (stok$token == "ELSE") {
                if (nest_depth == 0 && brace_depth == 0) {
                    else_idx <- search_idx
                    break
                } else if (nest_depth > 0) {
                    nest_depth <- nest_depth - 1
                }
            }
            search_idx <- search_idx + 1
        }

        if (is.null(else_idx)) { next }

        # Skip if-else that already has braces (already in expanded form)
        body_first <- terminals[close_paren_idx + 1,]
        if (body_first$token == "'{'") { next }

        # Extract true expression (between close_paren and else)
        true_tokens <- terminals[(close_paren_idx + 1):(else_idx - 1),]
        true_text <- format_line_tokens(true_tokens)

        # Extract false expression (after else to end of statement)
        # Need to find where the statement ends
        false_start <- else_idx + 1
        if (false_start > nrow(terminals)) { next }

        # Skip braced false body — multi-statement blocks can't be inlined
        if (terminals$token[false_start] == "'{'") { next }

        # Find end of false expression - could be end of line or next statement
        false_end <- false_start
        false_paren_depth <- 0
        false_brace_depth <- 0
        false_if_depth <- 0 # track nested if-else
        false_start_line <- terminals$line1[false_start]

        while (false_end <= nrow(terminals)) {
            ftok <- terminals[false_end,]

            # Track if-else nesting for chained else-if on same line only
            # (IF tokens on subsequent lines are new statements, not part of
            # the false expression)
            if (ftok$token == "IF" && ftok$line1 == false_start_line) {
                false_if_depth <- false_if_depth + 1
            }
            if (ftok$token == "ELSE" && ftok$line1 == false_start_line) {
                false_if_depth <- max(0, false_if_depth - 1)
            }

            # Track nesting
            prev_paren_depth <- false_paren_depth
            if (ftok$token == "'('") {
                false_paren_depth <- false_paren_depth + 1
            }
            if (ftok$token == "')'") {
                false_paren_depth <- false_paren_depth - 1
            }
            if (ftok$token == "'{'") {
                false_brace_depth <- false_brace_depth + 1
            }
            if (ftok$token == "'}'") {
                false_brace_depth <- false_brace_depth - 1
            }

            # If we just closed the outermost paren/brace, include this
            # token and stop — but not inside an if-else (condition parens)
            if (prev_paren_depth > 0 && false_paren_depth == 0 &&
                false_brace_depth == 0 && false_if_depth == 0) {
                break
            }

            # End at line break when not nested (for simple expressions)
            if (ftok$line1 > false_start_line && false_paren_depth <= 0 &&
                false_brace_depth <= 0 && false_if_depth == 0) {
                false_end <- false_end - 1
                break
            }

            false_end <- false_end + 1
        }

        if (false_end > nrow(terminals)) {
            false_end <- nrow(terminals)
        }

        # Determine line ranges for true and false expressions
        if (nrow(true_tokens) > 0) {
            true_end_line <- max(true_tokens$line1)
        } else {
            true_end_line <- assign_line
        }
        false_tokens <- terminals[false_start:false_end,]
        # Skip if branches are complex (function expressions, comments, or multi-line)
        if (any(true_tokens$token == "FUNCTION") ||
            any(false_tokens$token == "FUNCTION") ||
            any(true_tokens$token == "COMMENT") ||
            any(false_tokens$token == "COMMENT") ||
            any(true_tokens$line2 > true_tokens$line1) ||
            any(false_tokens$line2 > false_tokens$line1)) {
            next
        }
        if (nrow(false_tokens) > 0) {
            false_end_line <- max(false_tokens$line2)
        } else {
            false_end_line <- assign_line
        }

        # Skip multi-line inline if-else when line_limit > 0 (avoid breaking)
        spans_lines <- false_end_line > assign_line ||
        true_end_line > assign_line
        if (spans_lines && line_limit > 0L) {
            next
        }

        # Skip short single-line expressions when line_limit > 0
        if (line_limit > 0L && !spans_lines &&
            nchar(lines[assign_line]) <= line_limit) {
            next
        }

        # Skip chained if-else expressions (else clause starts with IF)
        # e.g., status <- if (a) "x" else if (b) "y" else "z"
        # These are a valid R idiom; don't expand unless user requested it
        if (line_limit > 0L && false_start <= nrow(terminals) &&
            terminals$token[false_start] == "IF") {
            next
        }

        # Get base indent from current line
        current_line <- lines[assign_line]
        base_indent <- sub("^(\\s*).*", "\\1", current_line)
        inner_indent <- paste0(base_indent, "    ")

        # Extract true and false expressions from tokens
        # Always use format_line_tokens (not extract_expr_text) since the
        # expression may share source lines with other parts of the if-else
        true_text <- format_line_tokens(true_tokens)
        false_text <- format_line_tokens(false_tokens)

        # Build replacement lines
        new_lines <- c(paste0(base_indent, "if (", cond_text, ") {"),
            paste0(inner_indent, var_name, " <- ", true_text),
            paste0(base_indent, "} else {"),
            paste0(inner_indent, var_name, " <- ", false_text),
            paste0(base_indent, "}"))

        # Find actual end line (could span multiple lines)
        end_line <- false_end_line

        # Replace lines
        pre <- if (assign_line > 1) lines[1:(assign_line - 1)] else character(0)
        if (end_line < length(lines)) {
            post <- lines[(end_line + 1):length(lines)]
        } else {
            post <- character(0)
        }
        new_code_lines <- c(pre, new_lines, post)

        result <- paste(new_code_lines, collapse = "\n")
        if (!grepl("\n$", result)) {
            result <- paste0(result, "\n")
        }

        return(result)
    }

    NULL
}

#' Format Tokens on a Single Line
#'
#' @param tokens Data frame of tokens for one line.
#' @return Formatted line content (no leading whitespace).
#' @keywords internal
#' @param prev_token Optional token to treat as the previous token when
#'   formatting a token subset (e.g., suffix after a collapsed call).
#' @param prev_prev_token Optional token before prev_token for unary detection.
format_line_tokens <- function (tokens, prev_token = NULL,
    prev_prev_token = NULL) {
    if (nrow(tokens) == 0) {
        return("")
    }

    tokens <- tokens[order(tokens$line1, tokens$col1),]
    parts <- character(nrow(tokens))
    prev <- prev_token
    prev_prev <- prev_prev_token

    for (i in seq_len(nrow(tokens))) {
        tok <- tokens[i,]

        # Convert = assignment to <-
        tok_text <- tok$text
        if (tok$token == "EQ_ASSIGN") {
            tok_text <- "<-"
        }

        if (!is.null(prev)) {
            if (needs_space(prev, tok, prev_prev)) {
                parts[i] <- paste0(" ", tok_text)
            } else {
                parts[i] <- tok_text
            }
        } else {
            parts[i] <- tok_text
        }

        prev_prev <- prev
        prev <- tok
    }

    paste(parts, collapse = "")
}

#' Determine If Space Needed Between Tokens
#'
#' @param prev Previous token (data frame row).
#' @param tok Current token (data frame row).
#' @param prev_prev Token before prev (data frame row or NULL), for unary detection.
#' @return Logical.
#' @keywords internal
needs_space <- function (prev, tok, prev_prev = NULL) {
    p <- prev$token
    t <- tok$token

    # Always space before comments
    if (t == "COMMENT") {
        return(TRUE)
    }

    binary_ops <- c("LEFT_ASSIGN", "EQ_ASSIGN", "RIGHT_ASSIGN", "EQ_SUB",
        "EQ_FORMALS", "AND", "OR", "AND2", "OR2", "GT", "LT", "GE", "LE", "EQ",
        "NE", "'+'", "'-'", "'*'", "'/'", "'^'", "SPECIAL", "'~'")

    if (p %in% c("'('", "'['", "LBB")) {
        return(FALSE)
    }

    if (t %in% c("')'", "']'", "']]'", "','")) {
        return(FALSE)
    }

    if (p == "')'") {
        if (t %in% c("'['", "LBB", "'('", "'$'", "'@'", "':'")) {
            return(FALSE)
        }
        return(TRUE)
    }

    if (t == "'('" && p == "SYMBOL_FUNCTION_CALL") {
        return(FALSE)
    }

    if (p == "'!'") {
        return(FALSE)
    }

    if (p %in% c("'$'", "'@'", "':'") || t %in% c("'$'", "'@'", "':'")) {
        return(FALSE)
    }

    if (p == "NS_GET" || t == "NS_GET" || p == "NS_GET_INT" ||
        t == "NS_GET_INT") {
        return(FALSE)
    }

    if (p %in% c("IF", "ELSE", "FOR", "WHILE", "REPEAT", "IN")) {
        return(TRUE)
    }

    # Space before IN and ELSE keywords
    if (t %in% c("IN", "ELSE")) {
        return(TRUE)
    }

    if (t == "'('" && p == "FUNCTION") {
        return(TRUE)
    }

    if (t == "'{'") {
        return(TRUE)
    }

    if (p == "'{'") {
        return(TRUE)
    }

    if (t == "'}'") {
        return(TRUE)
    }

    if (p == "'}'") {
        return(TRUE)
    }

    # Detect unary minus/plus: no space after - or + when it's unary
    if (p %in% c("'-'", "'+'")) {
        if (is.null(prev_prev)) {
            # First real token is -/+, it's unary
            return(FALSE)
        }
        pp <- prev_prev$token
        unary_context <- c("'('", "'['", "LBB", "','", "'{'", "LEFT_ASSIGN",
            "EQ_ASSIGN", "RIGHT_ASSIGN", "EQ_SUB", "EQ_FORMALS", "AND", "OR",
            "AND2", "OR2", "GT", "LT", "GE", "LE", "EQ", "NE", "'+'", "'-'",
            "'*'", "'/'", "'^'", "SPECIAL", "'~'", "'!'", "IF", "ELSE", "FOR",
            "WHILE", "REPEAT", "IN", "RETURN", "NEXT", "BREAK")
        if (pp %in% unary_context) {
            return(FALSE)
        }
    }

    if (t %in% binary_ops || p %in% binary_ops) {
        return(TRUE)
    }

    if (p %in% c("','", "';'")) {
        return(TRUE)
    }

    if (p %in% c("NEXT", "BREAK", "RETURN")) {
        return(TRUE)
    }

    symbols <- c("SYMBOL", "SYMBOL_FUNCTION_CALL", "SYMBOL_FORMALS",
        "SYMBOL_SUB", "SYMBOL_PACKAGE", "NUM_CONST", "STR_CONST", "NULL_CONST",
        "SPECIAL")

    if (p %in% symbols && t %in% symbols) {
        return(TRUE)
    }

    FALSE
}

#' Convert Tab-Expanded Column to Character Position
#'
#' R's getParseData() reports columns with tabs expanded to 8-column tab stops.
#' This function converts such a column back to a character position for use
#' with substring().
#'
#' @param line A single line of text.
#' @param col Tab-expanded column position (1-based).
#' @return Character position (1-based) in the string.
#' @keywords internal
col_to_charpos <- function (line, col) {
    if (!grepl("\t", line, fixed = TRUE)) {
        return(col)
    }
    chars <- strsplit(line, "")[[1]]
    display_col <- 0L
    for (i in seq_along(chars)) {
        if (chars[i] == "\t") {
            display_col <- display_col + (8L - (display_col %% 8L))
        } else {
            display_col <- display_col + 1L
        }
        if (display_col >= col) {
            return(i)
        }
    }
    length(chars)
}

#' Get Tab-Expanded Line Length
#'
#' Returns the display width of a line, with tabs expanded to 8-column stops.
#'
#' @param line A single line of text.
#' @return Display width of the line.
#' @keywords internal
display_width <- function (line) {
    if (!grepl("\t", line, fixed = TRUE)) {
        return(nchar(line))
    }
    chars <- strsplit(line, "")[[1]]
    col <- 0L
    for (ch in chars) {
        if (ch == "\t") {
            col <- col + (8L - (col %% 8L))
        } else {
            col <- col + 1L
        }
    }
    col
}

#' Extract Expression Text from Source Lines
#'
#' Extract original text for a multi-line expression and re-indent it.
#'
#' @param lines Source code lines.
#' @param tokens Token data frame for the expression.
#' @param target_indent Target indentation string for continuation lines.
#' @return Expression text with first line unindented, continuation lines re-indented.
#' @keywords internal
extract_expr_text <- function (lines, tokens, target_indent) {
    if (nrow(tokens) == 0) { return("") }

    # Get line range
    start_line <- min(tokens$line1)
    end_line <- max(tokens$line2) # Use line2 for end position

    # Single line - just use format_line_tokens
    if (start_line == end_line) {
        return(format_line_tokens(tokens))
    }

    # Multi-line - extract from source
    # First line: from first token to end of line
    first_tok <- tokens[tokens$line1 == start_line,][1,]
    first_line_text <- substring(lines[start_line],
        col_to_charpos(lines[start_line], first_tok$col1))

    # Middle lines: full line content, re-indented
    result_lines <- first_line_text

    if (end_line > start_line) {
        for (ln in (start_line + 1):end_line) {
            line_text <- lines[ln]
            # Remove existing indentation and add target indent
            trimmed <- sub("^\\s*", "", line_text)
            # Add extra indent for continuation (2 more spaces)
            result_lines <- c(result_lines,
                paste0("\n", target_indent, "  ", trimmed))
        }
    }

    paste(result_lines, collapse = "")
}

#' Format Blank Lines
#'
#' Normalize blank lines between code blocks.
#'
#' @param code Code string.
#' @return Code with normalized blank lines.
#' @keywords internal
format_blank_lines <- function (code) {
    # Remove trailing whitespace from each line
    code <- gsub(" +\n", "\n", code)
    # Remove blank lines after opening brace
    code <- gsub("\\{\n\\s*\n", "{\n", code)
    # Collapse multiple blank lines to one
    code <- gsub("\n{3,}", "\n\n", code)
    # Remove trailing newlines (keep one)
    code <- gsub("\n+$", "\n", code)
    code
}

#' Parse Gate for Transform Passes
#'
#' @param code Code string.
#' @return TRUE if code parses, FALSE otherwise.
#' @keywords internal
is_parseable_code <- function (code) {
    !is.null(tryCatch(parse(text = code, keep.source = TRUE),
        error = function (e) NULL))
}

#' Apply Transform Only If Output Parses
#'
#' @param code Code string.
#' @param fn Transform function taking `code` as first argument.
#' @param ... Additional arguments passed to `fn`.
#' @return Transformed code if parseable, otherwise original code.
#' @keywords internal
apply_if_parseable <- function (code, fn, ...) {
    updated <- tryCatch(
        fn(code, ...),
        error = function (e) code
    )

    if (!is.character(updated) || length(updated) != 1L) {
        return(code)
    }

    if (!identical(updated, code) && !is_parseable_code(updated)) {
        return(code)
    }

    updated
}

#' Size-Aware Iteration Budget
#'
#' Limits expensive parse/rewrite loops on very large files.
#'
#' @param code Code string.
#' @param default_max Default maximum iterations.
#' @param mode Rewrite family. One of `"collapse"`, `"funcdef"`,
#'   `"control"`, `"wrap"`, `"inline_if"`, or `"generic"`.
#' @return Integer iteration budget.
#' @keywords internal
iteration_budget <- function (code, default_max, mode = "generic") {
    n_lines <- length(strsplit(code, "\n", fixed = TRUE)[[1]])
    # On very large files, one-at-a-time parse/rewrite loops are too costly
    # and can remain non-idempotent under tight timeout budgets. In that
    # regime we keep only the base token formatting pass.
    if (n_lines > 1500L) {
        return(0L)
    }
    # For medium/large files we disable one-change-at-a-time structural
    # rewrites that are the main source of non-idempotent drift.
    if (n_lines > 250L && mode %in% c("collapse", "funcdef", "control")) {
        return(0L)
    }
    if (n_lines > 600L) {
        return(min(default_max, 6L))
    }
    if (n_lines > 250L) {
        return(min(default_max, 12L))
    }
    default_max
}

#' Collapse Multi-Line Function Calls
#'
#' Collapses function calls spanning multiple lines into a single line.
#' Long lines are re-wrapped by `wrap_long_calls()` afterward. For example:
#' \preformatted{c(x,
#'   y,
#'   z
#' )}
#' becomes `c(x, y, z)`.
#'
#' @param code Formatted code string.
#' @return Code with collapsed function calls.
#' @keywords internal
collapse_calls <- function (code) {
    changed <- TRUE
    max_iterations <- iteration_budget(code, 100L, mode = "collapse")

    while (changed && max_iterations > 0) {
        max_iterations <- max_iterations - 1
        changed <- FALSE

        result <- collapse_one_call(code)
        if (!is.null(result)) {
            code <- result
            changed <- TRUE
        }
    }

    code
}

#' Collapse One Multi-Line Function Call
#'
#' Finds the first multi-line function call that can fit on one line
#' and collapses it. Skips calls containing comments.
#'
#' @param code Code string.
#' @return Modified code or NULL if no changes.
#' @keywords internal
collapse_one_call <- function (code) {
    parsed <- tryCatch(
        parse(text = code, keep.source = TRUE),
        error = function (e) NULL
    )

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

    # Find multi-line parenthesized groups:
    # 1. Function calls: SYMBOL_FUNCTION_CALL followed by '('
    # 2. Control flow: IF/FOR/WHILE followed by '('
    target_tokens <- c("SYMBOL_FUNCTION_CALL", "IF", "FOR", "WHILE")
    target_indices <- which(terminals$token %in% target_tokens)

    for (ci in target_indices) {
        # Next token should be '('
        open_idx <- ci + 1
        if (open_idx > nrow(terminals)) { next }
        if (terminals$token[open_idx] != "'('") { next }

        open_line <- terminals$line1[open_idx]

        # Find matching ')'
        paren_depth <- 1
        close_idx <- open_idx + 1
        while (close_idx <= nrow(terminals) && paren_depth > 0) {
            if (terminals$token[close_idx] == "'('") {
                paren_depth <- paren_depth + 1
            } else if (terminals$token[close_idx] == "')'") {
                paren_depth <- paren_depth - 1
            }
            if (paren_depth > 0) { close_idx <- close_idx + 1 }
        }

        if (close_idx > nrow(terminals)) { next }

        close_line <- terminals$line1[close_idx]

        # Only process multi-line groups
        if (close_line == open_line) { next }

        # Skip if any token in the group is a comment
        inner_tokens <- terminals[seq(open_idx, close_idx),]
        if (any(inner_tokens$token == "COMMENT")) { next }

        # Skip if the group contains a FUNCTION definition or braces
        if (any(inner_tokens$token == "FUNCTION")) { next }
        if (any(inner_tokens$token == "'{'")) { next }

        # Build collapsed text from tokens
        call_tokens <- terminals[seq(ci, close_idx),]
        collapsed <- format_line_tokens(call_tokens)

        # Get prefix: everything before the function name on its line
        func_line <- terminals$line1[ci]
        func_col <- terminals$col1[ci]
        if (func_col > 1) {
            prefix <- substring(lines[func_line], 1,
                col_to_charpos(lines[func_line], func_col - 1))
        } else {
            prefix <- ""
        }

        full_line <- paste0(prefix, collapsed)

        # Check if there are tokens after the closing paren on its line
        # that need to be appended (e.g., trailing comma, closing paren of outer call)
        after_close <- terminals[terminals$line1 == close_line &
            terminals$col1 > terminals$col1[close_idx],]
        suffix <- ""
        if (nrow(after_close) > 0) {
            last_call_tok <- call_tokens[nrow(call_tokens),]
            prev_prev_tok <- NULL
            if (nrow(call_tokens) >= 2) {
                prev_prev_tok <- call_tokens[nrow(call_tokens) - 1,]
            }
            suffix <- format_line_tokens(after_close,
                prev_token = last_call_tok,
                prev_prev_token = prev_prev_tok)
        }

        full_line <- paste0(full_line, suffix)

        # Also check if there are tokens before the function call on func_line
        # that aren't part of the prefix (i.e., code tokens before the call)
        before_call <- terminals[terminals$line1 == func_line &
            terminals$col1 < func_col,]

        # Replace the lines
        if (func_line > 1) {
            pre <- lines[seq_len(func_line - 1)]
        } else {
            pre <- character(0)
        }
        if (close_line < length(lines)) {
            post <- lines[seq(close_line + 1, length(lines))]
        } else {
            post <- character(0)
        }
        new_code_lines <- c(pre, full_line, post)

        result <- paste(new_code_lines, collapse = "\n")
        if (!grepl("\n$", result) && nchar(result) > 0) {
            result <- paste0(result, "\n")
        }

        return(result)
    }

    NULL
}

#' Wrap Long Function Calls
#'
#' Wraps function call lines that exceed the line limit by breaking at commas.
#' Continuation lines are aligned to the opening parenthesis.
#'
#' @param code Formatted code string.
#' @param line_limit Maximum line length (default 80).
#' @return Code with wrapped long calls.
#' @keywords internal
wrap_long_calls <- function (code, line_limit = 80L) {
    changed <- TRUE
    max_iterations <- iteration_budget(code, 100L, mode = "wrap")

    while (changed && max_iterations > 0) {
        max_iterations <- max_iterations - 1
        changed <- FALSE

        result <- wrap_one_long_call(code, line_limit = line_limit)
        if (!is.null(result)) {
            code <- result
            changed <- TRUE
        }
    }

    code
}

#' Wrap One Long Function Call
#'
#' Finds the first single-line function call that exceeds the line limit
#' and wraps it at commas with paren-aligned continuation.
#'
#' @param code Code string.
#' @param line_limit Maximum line length (default 80).
#' @return Modified code or NULL if no changes.
#' @keywords internal
wrap_one_long_call <- function (code, line_limit = 80L) {
    parsed <- tryCatch(
        parse(text = code, keep.source = TRUE),
        error = function (e) NULL
    )

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

    # Find function calls on lines that exceed the limit
    call_indices <- which(terminals$token == "SYMBOL_FUNCTION_CALL")

    for (ci in call_indices) {
        open_idx <- ci + 1
        if (open_idx > nrow(terminals)) { next }
        if (terminals$token[open_idx] != "'('") { next }

        call_line <- terminals$line1[ci]

        # Only consider single-line calls on lines that are too long
        if (display_width(lines[call_line]) <= line_limit) { next }

        # Skip lines with semicolons (multiple statements on one line)
        line_toks <- terminals[terminals$line1 == call_line,]
        if (any(line_toks$token == "';'")) { next }

        # Skip calls nested inside another call's parens on the same line
        # (wrap the outermost call, not inner ones)
        func_col <- terminals$col1[ci]
        before <- line_toks[line_toks$col1 < func_col,]
        paren_depth_before <- sum(before$token == "'('") -
        sum(before$token == "')'")
        if (paren_depth_before > 0) { next }

        # Find matching ')'
        paren_depth <- 1
        close_idx <- open_idx + 1
        while (close_idx <= nrow(terminals) && paren_depth > 0) {
            if (terminals$token[close_idx] == "'('") {
                paren_depth <- paren_depth + 1
            } else if (terminals$token[close_idx] == "')'") {
                paren_depth <- paren_depth - 1
            }
            if (paren_depth > 0) { close_idx <- close_idx + 1 }
        }

        if (close_idx > nrow(terminals)) { next }

        # Only wrap single-line calls
        if (terminals$line1[close_idx] != call_line) { next }

        # Skip calls with embedded control blocks or function literals
        call_tokens <- terminals[seq(open_idx, close_idx),]
        if (any(call_tokens$token %in% c("'{'", "'}'", "FUNCTION", "IF", "ELSE"))) {
            next
        }

        # Need at least one comma at depth 1 to have something to wrap
        has_comma <- FALSE
        for (k in seq(open_idx + 1, close_idx - 1)) {
            if (terminals$token[k] == "','" &&
                terminals$line1[k] == call_line) {
                # Check this comma is at depth 1 (not inside nested parens/brackets)
                depth <- 0
                for (m in seq(open_idx + 1, k)) {
                    if (terminals$token[m] == "'('") { depth <- depth + 1 }
                    if (terminals$token[m] == "')'") { depth <- depth - 1 }
                    if (terminals$token[m] == "'['") { depth <- depth + 1 }
                    if (terminals$token[m] == "']'") { depth <- depth - 1 }
                    if (terminals$token[m] == "LBB") { depth <- depth + 2 }
                    if (terminals$token[m] == "']]'") { depth <- depth - 2 }
                }
                if (depth == 0) {
                    has_comma <- TRUE
                    break
                }
            }
        }
        if (!has_comma) { next }

        # Continuation indent: depth-based to match format_tokens
        # (base_indent + 1 level for the call's (, plus levels for any
        # unclosed [ or [[ brackets before the call on the same line)
        base_indent <- sub("^(\\s*).*", "\\1", lines[call_line])
        bracket_depth_before <- sum(before$token == "'['") +
        2L * sum(before$token == "LBB") -
        sum(before$token == "']'")
        cont_indent <- paste0(base_indent,
            strrep("    ", max(0, bracket_depth_before) + 1))

        # Collect arguments as groups of tokens between commas at depth 1
        args <- list()
        current_arg_tokens <- list()
        depth <- 0

        for (k in seq(open_idx + 1, close_idx - 1)) {
            tok <- terminals[k,]
            if (tok$token == "'('") { depth <- depth + 1 }
            if (tok$token == "')'") { depth <- depth - 1 }
            if (tok$token == "'['") { depth <- depth + 1 }
            if (tok$token == "']'") { depth <- depth - 1 }
            if (tok$token == "LBB") { depth <- depth + 2 }
            if (tok$token == "']]'") { depth <- depth - 2 }

            if (tok$token == "','" && depth == 0) {
                if (length(current_arg_tokens) > 0) {
                    arg_df <- do.call(rbind,
                        lapply(current_arg_tokens, as.data.frame))
                    args[[length(args) + 1]] <- format_line_tokens(arg_df)
                }
                current_arg_tokens <- list()
            } else {
                current_arg_tokens[[length(current_arg_tokens) + 1]] <- tok
            }
        }
        # Last argument
        if (length(current_arg_tokens) > 0) {
            arg_df <- do.call(rbind, lapply(current_arg_tokens, as.data.frame))
            args[[length(args) + 1]] <- format_line_tokens(arg_df)
        }

        if (length(args) < 2) { next }

        # Get prefix (everything before the function name)
        # Use col_to_charpos to handle tab-expanded columns from getParseData
        func_col <- terminals$col1[ci]
        line_content <- lines[call_line]
        if (func_col > 1) {
            char_pos <- col_to_charpos(line_content, func_col - 1)
            prefix <- substring(line_content, 1, char_pos)
        } else {
            prefix <- ""
        }

        func_name <- terminals$text[ci]

        # Get suffix (everything after closing paren on the same line)
        close_col <- terminals$col2[close_idx]
        char_pos_close <- col_to_charpos(line_content, close_col)
        if (char_pos_close < nchar(line_content)) {
            suffix <- substring(line_content, char_pos_close + 1)
        } else {
            suffix <- ""
        }

        # Build wrapped lines
        new_lines <- character(0)
        current_line <- paste0(prefix, func_name, "(")

        for (j in seq_along(args)) {
            if (j < length(args)) {
                arg_text <- paste0(args[[j]], ", ")
            } else {
                arg_text <- paste0(args[[j]], ")", suffix)
            }

            test_line <- paste0(current_line, arg_text)
            if (nchar(test_line) > line_limit &&
                nchar(current_line) > nchar(cont_indent)) {
                new_lines <- c(new_lines, sub(" $", "", current_line))
                current_line <- paste0(cont_indent, arg_text)
            } else {
                current_line <- test_line
            }
        }
        new_lines <- c(new_lines, sub(" $", "", current_line))

        # Only wrap if we actually split into multiple lines
        if (length(new_lines) < 2) { next }

        # Replace the line
        if (call_line > 1) {
            pre <- lines[seq_len(call_line - 1)]
        } else {
            pre <- character(0)
        }
        if (call_line < length(lines)) {
            post <- lines[seq(call_line + 1, length(lines))]
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

    # wrap_one_long_call: no changes
    NULL
}

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
    max_iterations <- iteration_budget(code, 200L, mode = "control")

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
    parsed <- tryCatch(
        parse(text = code, keep.source = TRUE),
        error = function (e) NULL
    )

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
                any(before$token %in% c("LEFT_ASSIGN", "EQ_ASSIGN", "RIGHT_ASSIGN"))) {
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
                            any(root_before$token %in% c("LEFT_ASSIGN", "EQ_ASSIGN", "RIGHT_ASSIGN"))) {
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
                    if (btok$token %in% c("LEFT_ASSIGN", "EQ_ASSIGN", "RIGHT_ASSIGN")) {
                        body_end_idx <- body_end_idx + 1
                        next
                    }
                    cont_tokens <- c("'+'", "'-'", "'*'", "'/'", "'^'",
                        "SPECIAL", "AND", "OR", "AND2", "OR2", "GT", "LT",
                        "GE", "LE", "EQ", "NE", "LEFT_ASSIGN", "EQ_ASSIGN",
                        "'~'", "','")
                    if (!(next_tok$token %in% cont_tokens)) { break }
                }
            }

            body_end_idx <- body_end_idx + 1
        }

        if (body_end_idx > nrow(terminals)) {
            body_end_idx <- nrow(terminals)
        }

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
            "RIGHT_ASSIGN", "'+'", "'-'", "'*'", "'/'", "'^'", "SPECIAL",
            "AND", "OR", "AND2", "OR2", "GT", "LT", "GE", "LE", "EQ",
            "NE", "'~'", "','")
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
                        if (else_body_line < length(lines)) lines[seq(
                                else_body_line + 1,
                                length(lines))] else character(0))
                } else {
                    new_code_lines <- c(pre_lines, new_line,
                        if (else_body_line < length(lines)) lines[seq(
                                else_body_line + 1,
                                length(lines))] else character(0))
                }
            } else if (else_body_start$token == "IF") {
                new_line <- paste0(prefix, " { ", body_text, " } else")
                # The IF may be on the same line as ELSE — reconstruct
                # the "if ..." part from the IF token's column onward
                ctrl_indent <- sub("^(\\s*).*", "\\1", lines[ctrl_line])
                if_rest <- paste0(ctrl_indent,
                    trimws(substring(lines[else_body_start$line1], else_body_start$col1)))
                if (body_has_comment || nchar(new_line) > 80L) {
                    new_lines_vec <- c(paste0(prefix, open_brace_suffix),
                        paste0(inner_indent, body_text),
                        paste0(ctrl_indent, "} else"))
                    new_code_lines <- c(pre_lines, new_lines_vec, if_rest,
                        if (else_body_start$line1 < length(lines)) lines[seq(
                                else_body_start$line1 + 1,
                                length(lines))] else character(0))
                } else {
                    new_code_lines <- c(pre_lines, new_line, if_rest,
                        if (else_body_start$line1 < length(lines)) lines[seq(
                                else_body_start$line1 + 1,
                                length(lines))] else character(0))
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

                else_body_tokens <- terminals[else_body_idx:else_end_idx,]
                if (any(else_body_tokens$token %in% complex_body_tokens)) {
                    next
                }
                if (else_body_tokens$token[nrow(else_body_tokens)] %in%
                    continuation_tail_tokens) {
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
                        if (else_end_line < length(lines)) lines[seq(
                                else_end_line + 1,
                                length(lines))] else character(0))
                } else {
                    new_code_lines <- c(pre_lines, new_line,
                        if (else_end_line < length(lines)) lines[seq(
                                else_end_line + 1,
                                length(lines))] else character(0))
                }
            }
        } else {
            body_end_line <- terminals$line1[body_end_idx]
            new_line <- paste0(prefix, " { ", body_text, " }")
            if (body_has_comment || nchar(new_line) > 80L) {
                new_lines_vec <- c(paste0(prefix, open_brace_suffix),
                    paste0(inner_indent, body_text), paste0(ctrl_indent, "}"))
                new_code_lines <- c(pre_lines, new_lines_vec,
                    if (body_end_line < length(lines)) lines[seq(
                            body_end_line + 1,
                            length(lines))] else character(0))
            } else {
                new_code_lines <- c(pre_lines, new_line,
                    if (body_end_line < length(lines)) lines[seq(
                            body_end_line + 1,
                            length(lines))] else character(0))
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

#' Wrap Long Lines at Operators
#'
#' Wraps lines exceeding the line limit at logical operators (`||`, `&&`)
#' with continuation aligned to the first operand.
#'
#' @param code Formatted code string.
#' @param line_limit Maximum line length (default 80).
#' @return Code with wrapped long lines.
#' @keywords internal
wrap_long_operators <- function (code, line_limit = 80L) {
    changed <- TRUE
    max_iterations <- iteration_budget(code, 100L, mode = "wrap")

    while (changed && max_iterations > 0) {
        max_iterations <- max_iterations - 1
        changed <- FALSE

        result <- wrap_one_long_operator(code, line_limit = line_limit)
        if (!is.null(result)) {
            code <- result
            changed <- TRUE
        }
    }

    code
}

#' Wrap One Long Line at an Operator
#'
#' Finds the first line exceeding the limit that contains a top-level
#' logical operator and wraps there. The operator stays at the end of the
#' first line, continuation is aligned to the first operand.
#'
#' @param code Code string.
#' @param line_limit Maximum line length (default 80).
#' @return Modified code or NULL if no changes.
#' @keywords internal
wrap_one_long_operator <- function (code, line_limit = 80L) {
    parsed <- tryCatch(
        parse(text = code, keep.source = TRUE),
        error = function (e) NULL
    )

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

    # Operators to break at, in priority order
    break_ops <- c("OR2", "AND2", "OR", "AND")

    for (line_num in seq_along(lines)) {
        if (display_width(lines[line_num]) <= line_limit) { next }

        line_toks <- terminals[terminals$line1 == line_num,]
        if (nrow(line_toks) == 0) { next }

        # Skip lines with semicolons (multiple statements on one line)
        if (any(line_toks$token == "';'")) { next }

        # Calculate paren depth at the start of this line
        start_depth <- 0
        before_toks <- terminals[terminals$line1 < line_num,]
        for (j in seq_len(nrow(before_toks))) {
            if (before_toks$token[j] == "'('") {
                start_depth <- start_depth + 1
            }
            if (before_toks$token[j] == "')'") {
                start_depth <- start_depth - 1
            }
        }

        # Find the best operator to break at
        best_break <- NULL
        depth <- start_depth

        for (j in seq_len(nrow(line_toks))) {
            tok <- line_toks[j,]
            if (tok$token == "'('") { depth <- depth + 1 }
            if (tok$token == "')'") { depth <- depth - 1 }

            # Break at operators inside the immediate context (not deeply nested)
            if (tok$token %in% break_ops && depth <= start_depth + 1) {
                if (tok$col2 <= line_limit) {
                    best_break <- j
                }
            }
        }

        if (is.null(best_break)) { next }

        break_tok <- line_toks[best_break,]

        # First part: up to and including the operator
        first_part <- substring(lines[line_num], 1,
            col_to_charpos(lines[line_num], break_tok$col2))

        # Continuation indent: depth-based to match format_tokens
        # (base_indent already reflects brace/paren depth from format_tokens;
        # add one indent level per unclosed bracket on this line before the break)
        base_indent <- sub("^(\\s*).*", "\\1", lines[line_num])

        bracket_depth <- 0
        for (j in seq_len(nrow(line_toks))) {
            tok_j <- line_toks[j,]
            if (tok_j$col1 > break_tok$col1) { break }
            if (tok_j$token %in% c("'('", "'['")) {
                bracket_depth <- bracket_depth + 1
            } else if (tok_j$token == "LBB") {
                bracket_depth <- bracket_depth + 2
            } else if (tok_j$token %in% c("')'", "']'")) {
                bracket_depth <- bracket_depth - 1
            } else if (tok_j$token == "']]'") {
                bracket_depth <- bracket_depth - 2
            }
        }

        cont_indent <- paste0(base_indent,
            strrep("    ", max(0, bracket_depth)))

        # Second part: rest of line, trimmed
        rest <- substring(lines[line_num],
            col_to_charpos(lines[line_num], break_tok$col2) + 1)
        rest <- sub("^\\s+", "", rest)

        new_lines <- c(first_part, paste0(cont_indent, rest))

        pre <- if (line_num > 1) lines[seq_len(line_num - 1)] else character(0)
        if (line_num < length(lines)) {
            post <- lines[seq(line_num + 1, length(lines))]
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
