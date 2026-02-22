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
                           line_limit = 80L) {
    # Parse with source tracking
    parsed <- tryCatch(parse(text = code, keep.source = TRUE),
                       error = function (e) NULL)

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
    # pab right after the first '}' on a line (before any subsequent '{').
    # Used for '} else {' lines where end-of-line pab includes the new '{'.
    line_pab_after_close <- integer(max_line)

    for (i in seq_len(nrow(terminals))) {
        tok <- terminals[i,]
        ln <- tok$line1

        if (tok$token == "'{'") {
            # For control-flow braces (if/for/while/else bodies),
            # preserve the enclosing paren context so body lines inside
            # a call get an extra indent level. For bare brace args
            # (tryCatch({...})) and function bodies, zero out paren
            # contribution as before.
            is_ctrl_brace <- FALSE
            if (i >= 2L) {
                pt <- terminals$token[i - 1L]
                if (pt == "ELSE" || pt == "REPEAT") {
                    is_ctrl_brace <- TRUE
                } else if (pt == "')'") {
                    # Check if this ) closes an if/for/while condition
                    # by scanning back to find the matching ( and checking
                    # the token before it
                    pd2 <- 1L
                    k <- i - 2L
                    while (k >= 1L && pd2 > 0L) {
                        if (terminals$token[k] == "')'") { pd2 <- pd2 + 1L }
                        if (terminals$token[k] == "'('") { pd2 <- pd2 - 1L }
                        if (pd2 > 0L) { k <- k - 1L }
                    }
                    if (k >= 2L &&
                        terminals$token[k - 1L] %in% c("IF", "FOR", "WHILE")) {
                        is_ctrl_brace <- TRUE
                    }
                }
            }
            if (is_ctrl_brace) {
                # Preserve enclosing pab so paren contribution carries
                # through into the control-flow body
                enclosing_pab <- if (length(paren_at_brace) > 0) {
                    paren_at_brace[length(paren_at_brace)]
                } else {
                    0L
                }
                paren_at_brace <- c(paren_at_brace, enclosing_pab)
            } else {
                paren_at_brace <- c(paren_at_brace, paren_depth)
            }
            brace_depth <- brace_depth + 1
        } else if (tok$token == "'}'") {
            brace_depth <- max(0, brace_depth - 1)
            if (length(paren_at_brace) > 0) {
                paren_at_brace <- paren_at_brace[-length(paren_at_brace)]
            }
            # Capture pab right after this close-brace
            line_pab_after_close[ln] <- if (length(paren_at_brace) > 0) {
                paren_at_brace[length(paren_at_brace)]
            } else {
                0L
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
                # Use pab right after the '}' closes, not end-of-line pab.
                # This matters for '} else {' where end-of-line pab includes
                # the new '{' push and would zero out paren contribution.
                prev_pab <- line_pab_after_close[ln]
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

    if (is_large_file) {
        return(result)
    }

    # Collapse multi-line calls that fit on one line
    result <- apply_if_parseable(result, collapse_calls)

    # Add braces to one-liner control flow (before wrapping, so bare
    # bodies move to their own lines before line-length decisions)
    result <- apply_if_parseable(result, add_control_braces)

    # Expand bare if-else arguments in overlong calls before wrapping,
    # so the braced form is stable and wrap passes see clean lines
    result <- apply_if_parseable(result, expand_call_if_args,
                                 line_limit = line_limit)

    # Wrap long lines at operators (||, &&), then at commas
    result <- apply_if_parseable(result, wrap_long_operators,
                                 line_limit = line_limit)
    result <- apply_if_parseable(result, wrap_long_calls,
                                 line_limit = line_limit)

    # Reformat function definitions
    result <- apply_if_parseable(result, reformat_function_defs, wrap = wrap,
                                 brace_style = brace_style,
                                 line_limit = line_limit)
    # Function-def rewrites can expose bare one-line control flow.
    result <- apply_if_parseable(result, add_control_braces)

    # Reformat inline if-else to multi-line
    # Always expand long lines; optionally expand all
    result <- apply_if_parseable(result, reformat_inline_if, line_limit = if (expand_if) {
            0L
        } else {
            line_limit
        })

    # Final expansion of bare if-else call args
    result <- apply_if_parseable(result, expand_call_if_args,
                                 line_limit = line_limit)

    # Final wrap pass: earlier passes may have produced new long lines
    result <- apply_if_parseable(result, wrap_long_operators,
                                 line_limit = line_limit)
    result <- apply_if_parseable(result, wrap_long_calls,
                                 line_limit = line_limit)
    result
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
                    "EQ_FORMALS", "AND", "OR", "AND2", "OR2", "GT", "LT", "GE",
                    "LE", "EQ", "NE", "'+'", "'-'", "'*'", "'/'", "'^'",
                    "SPECIAL", "'~'")

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
                           "EQ_ASSIGN", "RIGHT_ASSIGN", "EQ_SUB", "EQ_FORMALS",
                           "AND", "OR", "AND2", "OR2", "GT", "LT", "GE", "LE",
                           "EQ", "NE", "'+'", "'-'", "'*'", "'/'", "'^'",
                           "SPECIAL", "'~'", "'!'", "IF", "ELSE", "FOR",
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
                 "SYMBOL_SUB", "SYMBOL_PACKAGE", "NUM_CONST", "STR_CONST",
                 "NULL_CONST", "SPECIAL")

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
    updated <- tryCatch(fn(code, ...), error = function (e) code)

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
    # On very large files, one-at-a-time parse/rewrite loops are too costly.
    # Keep only the base token formatting pass.
    if (n_lines > 1500L) {
        return(0L)
    }
    if (n_lines > 800L) {
        return(min(default_max, 6L))
    }
    if (n_lines > 400L) {
        return(min(default_max, 25L))
    }
    default_max
}

