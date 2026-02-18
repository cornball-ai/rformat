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
#' @return Formatted code as character string.
#' @importFrom utils getParseData
#' @keywords internal
format_tokens <- function (code, indent = 4L, wrap = "paren",
                           expand_if = FALSE, brace_style = "kr") {
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
    orig_lines <- strsplit(code, "\n", fixed = TRUE) [[1]]

    # Track nesting for indentation
    brace_depth <- 0
    paren_depth <- 0

    # First pass: calculate nesting depth at end of each line
    max_line <- max(c(terminals$line1, length(orig_lines)))
    line_end_brace <- integer(max_line)
    line_end_paren <- integer(max_line)

    for (i in seq_len(nrow(terminals))) {
        tok <- terminals[i,]
        ln <- tok$line1

        if (tok$token == "'{'") {
            brace_depth <- brace_depth + 1
        } else if (tok$token == "'}'") {
            brace_depth <- max(0, brace_depth - 1)
        } else if (tok$token == "'('") {
            paren_depth <- paren_depth + 1
        } else if (tok$token == "')'") {
            paren_depth <- max(0, paren_depth - 1)
        }

        line_end_brace[ln] <- brace_depth
        line_end_paren[ln] <- paren_depth
    }

    # Fill in gaps (comment/blank lines inherit from previous)
    for (ln in seq_len(max_line)) {
        if (ln > 1 && line_end_brace[ln] == 0 && line_end_paren[ln] == 0) {
            if (nrow(terminals[terminals$line1 == ln,]) == 0) {
                line_end_brace[ln] <- line_end_brace[ln - 1]
                line_end_paren[ln] <- line_end_paren[ln - 1]
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
            if (nrow(line_tokens) > 0 && line_tokens$token[1] == "')'") {
                prev_paren <- max(0, prev_paren - 1)
            }

            # Both braces and parens add indent
            line_indent[ln] <- prev_brace + prev_paren
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
                    indent <- line_indent[line_num]
                } else {
                    indent <- 0
                }
                out_lines[line_num] <- paste0(
                    paste0(rep(indent_str, indent), collapse = ""),
                    trimws(line)
                )
            }
            next
        }

        if (line_num <= max_line) {
            indent <- line_indent[line_num]
        } else {
            indent <- 0
        }
        formatted <- format_line_tokens(line_tokens)

        out_lines[line_num] <- paste0(
            paste0(rep(indent_str, indent), collapse = ""),
            formatted
        )
    }

    # Filter out NA lines (multi-line token continuations)
    out_lines <- out_lines[!is.na(out_lines)]
    result <- paste(out_lines, collapse = "\n")
    if (!grepl("\n$", result) && nchar(result) > 0) {
        result <- paste0(result, "\n")
    }

    # Collapse multi-line calls that fit on one line
    result <- collapse_calls(result)

    # Wrap long function calls
    result <- wrap_long_calls(result)

    # Add braces to one-liner control flow
    result <- add_control_braces(result)

    # Reformat function definitions
    result <- reformat_function_defs(result, wrap = wrap, brace_style = brace_style)

    # Reformat inline if-else to multi-line (optional)
    if (expand_if) {
        result <- reformat_inline_if(result)
    }

    result
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
#' @return Code with reformatted function definitions.
#' @keywords internal
reformat_function_defs <- function(code, wrap = "paren", brace_style = "kr")
{
    # Process one function at a time, re-parsing each time
    # to handle line number changes
    changed <- TRUE
    max_iterations <- 50

    while (changed && max_iterations > 0) {
        max_iterations <- max_iterations - 1
        changed <- FALSE

        result <- reformat_one_function(code, wrap = wrap, brace_style = brace_style)
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
reformat_one_function <- function(code, wrap = "paren", brace_style = "kr",
    line_limit = 80L)
{
    parsed <- tryCatch(
        parse(text = code, keep.source = TRUE),
        error = function(e) NULL
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

    lines <- strsplit(code, "\n", fixed = TRUE) [[1]]

    # Find first function that needs reformatting
    for (fi in func_indices) {
        func_tok <- terminals[fi,]
        func_line <- func_tok$line1

        # Find the opening ( after function
        next_idx <- fi + 1
        if (next_idx > nrow(terminals)) next
        if (terminals$token[next_idx] != "'('") next

        # Find matching closing )
        paren_depth <- 1
        close_idx <- next_idx + 1
        while (close_idx <= nrow(terminals) && paren_depth > 0) {
            if (terminals$token[close_idx] == "'('") {
                paren_depth <- paren_depth + 1
            } else if (terminals$token[close_idx] == "')'") {
                paren_depth <- paren_depth - 1
            }
            if (paren_depth > 0) close_idx <- close_idx + 1
        }

        if (close_idx > nrow(terminals)) next

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
            prefix <- substring(func_line_content, 1, func_tok$col1 - 1)
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
                if (i + 1 < close_idx && terminals$token[i + 1] == "EQ_FORMALS") {
                    formal_text <- paste0(formal_name, " = ")
                    i <- i + 2

                    # Collect the default value tokens
                    value_tokens <- list()
                    value_paren_depth <- 0

                    while (i < close_idx) {
                        vtok <- terminals[i,]
                        if (vtok$token == "'('") value_paren_depth <- value_paren_depth + 1
                        if (vtok$token == "')'") {
                            if (value_paren_depth == 0) break
                            value_paren_depth <- value_paren_depth - 1
                        }
                        if (vtok$token == "','" && value_paren_depth == 0) break

                        value_tokens[[length(value_tokens) + 1]] <- vtok
                        i <- i + 1
                    }

                    # Format value tokens properly
                    if (length(value_tokens) > 0) {
                        value_df <- do.call(rbind, lapply(value_tokens, as.data.frame))
                        formal_text <- paste0(formal_text, format_line_tokens(value_df))
                    }
                }

                formal_texts <- c(formal_texts, formal_text)
            }

            i <- i + 1
        }

        # Build single-line signature: prefix + function (arg1, arg2, ...)
        single_line_sig <- paste0(prefix, "function (", paste(formal_texts, collapse = ", "), ")")

        # Check if single line fits
        if (nchar(single_line_sig) <= line_limit) {
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
                if (nchar(test_line) > line_limit && nchar(current_line) > nchar(cont_indent)) {
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
            body_tokens <- terminals[terminals$line1 >= close_paren_line &
            seq_len(nrow(terminals)) > close_idx,]
            if (nrow(body_tokens) > 0) {
                has_inline_body <- TRUE
                body_end_line <- max(body_tokens$line1)
                body_lines <- lines[close_paren_line:body_end_line]
                first_body_line <- body_lines[1]
                close_paren_col <- terminals$col2[close_idx]
                inline_body <- substring(first_body_line, close_paren_col + 1)
                if (length(body_lines) > 1) {
                    inline_body <- paste(c(inline_body, body_lines[- 1]), collapse = "\n")
                }
                inline_body <- trimws(inline_body)
            }
        }

        # Add brace or inline body
        if (has_brace) {
            if (brace_style == "kr") {
                # K&R: brace on same line as closing paren
                new_lines[length(new_lines)] <- paste0(new_lines[length(new_lines)], " {")
            } else {
                # Allman: brace on its own line
                new_lines <- c(new_lines, paste0(base_indent, "{"))
            }
        } else if (has_inline_body) {
            # Append inline body to last line
            new_lines[length(new_lines)] <- paste0(new_lines[length(new_lines)], " ", inline_body)
        }

        # Check if reformatting is actually needed
        end_line <- close_paren_line
        if (has_brace) {
            end_line <- brace_line
        } else if (has_inline_body) {
            body_tokens <- terminals[terminals$line1 >= close_paren_line &
            seq_len(nrow(terminals)) > close_idx,]
            if (nrow(body_tokens) > 0) {
                end_line <- max(body_tokens$line1)
            }
        }

        old_lines <- lines[func_line:end_line]
        if (identical(old_lines, new_lines)) next

        # Replace the lines and return
        new_code_lines <- c(
            if (func_line > 1) lines[seq_len(func_line - 1)] else character(0),
            new_lines,
            if (end_line < length(lines)) lines[seq(end_line + 1, length(lines))] else character(0)
        )

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
#' @return Code with reformatted inline if-else.
#' @keywords internal
reformat_inline_if <- function(code) {
    changed <- TRUE
    max_iterations <- 100

    while (changed && max_iterations > 0) {
        max_iterations <- max_iterations - 1
        changed <- FALSE

        result <- reformat_one_inline_if(code)
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
#' @return Modified code or NULL if no changes.
#' @keywords internal
reformat_one_inline_if <- function(code) {
    parsed <- tryCatch(
        parse(text = code, keep.source = TRUE),
        error = function(e) NULL
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

    lines <- strsplit(code, "\n", fixed = TRUE) [[1]]

    # Find assignment followed by if on the same line
    assign_tokens <- c("LEFT_ASSIGN", "EQ_ASSIGN")

    for (i in seq_len(nrow(terminals))) {
        tok <- terminals[i,]

        if (!(tok$token %in% assign_tokens)) next

        assign_line <- tok$line1

        # Find IF token after assignment on same line
        # Skip if there's a FUNCTION token between assignment and IF
        if_idx <- NULL
        found_function <- FALSE
        for (j in (i + 1) :nrow(terminals)) {
            if (j > nrow(terminals)) break
            next_tok <- terminals[j,]
            if (next_tok$line1 != assign_line) break
            if (next_tok$token == "FUNCTION") {
                found_function <- TRUE
                break
            }
            if (next_tok$token == "IF") {
                if_idx <- j
                break
            }
        }

        if (found_function || is.null(if_idx)) next

        # Found pattern: assignment followed by if
        # Now find the structure: if ( cond ) true_expr else false_expr

        # Get the variable being assigned (tokens before assignment)
        var_tokens <- terminals[terminals$line1 == assign_line &
        seq_len(nrow(terminals)) < i,]
        if (nrow(var_tokens) == 0) next

        var_name <- paste(var_tokens$text, collapse = "")

        # Find opening paren after IF
        open_paren_idx <- if_idx + 1
        if (open_paren_idx > nrow(terminals)) next
        if (terminals$token[open_paren_idx] != "'('") next

        # Find matching closing paren for condition
        paren_depth <- 1
        close_paren_idx <- open_paren_idx + 1
        while (close_paren_idx <= nrow(terminals) && paren_depth > 0) {
            if (terminals$token[close_paren_idx] == "'('") {
                paren_depth <- paren_depth + 1
            } else if (terminals$token[close_paren_idx] == "')'") {
                paren_depth <- paren_depth - 1
            }
            if (paren_depth > 0) close_paren_idx <- close_paren_idx + 1
        }

        if (close_paren_idx > nrow(terminals)) next

        # Extract condition tokens
        cond_tokens <- terminals[(open_paren_idx + 1) :(close_paren_idx - 1),]
        cond_text <- format_line_tokens(cond_tokens)

        # Find ELSE token
        else_idx <- NULL
        # Track nesting to find the right ELSE
        search_idx <- close_paren_idx + 1
        nest_depth <- 0

        while (search_idx <= nrow(terminals)) {
            stok <- terminals[search_idx,]

            if (stok$token == "IF") {
                nest_depth <- nest_depth + 1
            } else if (stok$token == "ELSE") {
                if (nest_depth == 0) {
                    else_idx <- search_idx
                    break
                } else {
                    nest_depth <- nest_depth - 1
                }
            }
            search_idx <- search_idx + 1
        }

        if (is.null(else_idx)) next

        # Check if this is all on one line (inline if-else)
        else_line <- terminals$line1[else_idx]
        if (else_line != assign_line) next

        # Extract true expression (between close_paren and else)
        true_tokens <- terminals[(close_paren_idx + 1) :(else_idx - 1),]
        true_text <- format_line_tokens(true_tokens)

        # Extract false expression (after else to end of statement)
        # Need to find where the statement ends
        false_start <- else_idx + 1
        if (false_start > nrow(terminals)) next

        # Find end of false expression - could be end of line or next statement
        false_end <- false_start
        false_paren_depth <- 0
        false_brace_depth <- 0

        while (false_end <= nrow(terminals)) {
            ftok <- terminals[false_end,]

            # Track nesting
            prev_paren_depth <- false_paren_depth
            if (ftok$token == "'('") false_paren_depth <- false_paren_depth + 1
            if (ftok$token == "')'") false_paren_depth <- false_paren_depth - 1
            if (ftok$token == "'{'") false_brace_depth <- false_brace_depth + 1
            if (ftok$token == "'}'") false_brace_depth <- false_brace_depth - 1

            # If we just closed the outermost paren/brace, include this token and stop
            if (prev_paren_depth > 0 && false_paren_depth == 0 && false_brace_depth == 0) {
                break
            }

            # End at line break when not nested (for simple expressions)
            if (ftok$line1 > assign_line && false_paren_depth <= 0 && false_brace_depth <= 0) {
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
        if (nrow(false_tokens) > 0) {
            false_end_line <- max(false_tokens$line1)
        } else {
            false_end_line <- assign_line
        }

        # Get base indent from current line
        current_line <- lines[assign_line]
        base_indent <- sub("^(\\s*).*", "\\1", current_line)
        inner_indent <- paste0(base_indent, "  ")

        # Extract true expression - use original text if multi-line
        if (true_end_line > assign_line) {
            # Multi-line true expression - extract from source
            true_text <- extract_expr_text(lines, true_tokens, inner_indent)
        } else {
            true_text <- format_line_tokens(true_tokens)
        }

        # Extract false expression - use original text if multi-line
        if (false_end_line > assign_line) {
            # Multi-line false expression - extract from source
            false_text <- extract_expr_text(lines, false_tokens, inner_indent)
        } else {
            false_text <- format_line_tokens(false_tokens)
        }

        # Build replacement lines
        new_lines <- c(
            paste0(base_indent, "if (", cond_text, ") {"),
            paste0(inner_indent, var_name, " <- ", true_text),
            paste0(base_indent, "} else {"),
            paste0(inner_indent, var_name, " <- ", false_text),
            paste0(base_indent, "}")
        )

        # Find actual end line (could span multiple lines)
        end_line <- false_end_line

        # Replace lines
        new_code_lines <- c(
            if (assign_line > 1) lines[1:(assign_line - 1)] else character(0),
            new_lines,
            if (end_line < length(lines)) lines[(end_line + 1) :length(lines)] else character(0)
        )

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
format_line_tokens <- function(tokens) {
    if (nrow(tokens) == 0) {
        return("")
    }

    tokens <- tokens[order(tokens$line1, tokens$col1),]
    parts <- character(nrow(tokens))
    prev <- NULL

    for (i in seq_len(nrow(tokens))) {
        tok <- tokens[i,]

        # Convert = assignment to <-
        tok_text <- tok$text
        if (tok$token == "EQ_ASSIGN") {
            tok_text <- "<-"
        }

        if (!is.null(prev)) {
            if (needs_space(prev, tok)) {
                parts[i] <- paste0(" ", tok_text)
            } else {
                parts[i] <- tok_text
            }
        } else {
            parts[i] <- tok_text
        }

        prev <- tok
    }

    paste(parts, collapse = "")
}

#' Determine If Space Needed Between Tokens
#'
#' @param prev Previous token (data frame row).
#' @param tok Current token (data frame row).
#' @return Logical.
#' @keywords internal
needs_space <- function(
    prev,
    tok)
{
    p <- prev$token
    t <- tok$token

    binary_ops <- c(
        "LEFT_ASSIGN", "EQ_ASSIGN", "RIGHT_ASSIGN", "EQ_SUB", "EQ_FORMALS",
        "AND", "OR", "AND2", "OR2",
        "GT", "LT", "GE", "LE", "EQ", "NE",
        "'+'", "'-'", "'*'", "'/'", "'^'", "SPECIAL",
        "'~'"
    )

    if (p %in% c("'('", "'['", "'[['")) {
        return(FALSE)
    }

    if (t %in% c("')'", "']'", "']]'", "','")) {
        return(FALSE)
    }

    if (p == "')'") {
        if (t %in% c("'['", "'[['", "'('", "'$'", "'@'")) {
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

    if (p %in% c("'$'", "'@'") || t %in% c("'$'", "'@'")) {
        return(FALSE)
    }

    if (p == "NS_GET" || t == "NS_GET" || p == "NS_GET_INT" || t == "NS_GET_INT") {
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
        return(FALSE)
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

    if (t %in% binary_ops || p %in% binary_ops) {
        return(TRUE)
    }

    if (p == "','") {
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

#' Extract Expression Text from Source Lines
#'
#' Extract original text for a multi-line expression and re-indent it.
#'
#' @param lines Source code lines.
#' @param tokens Token data frame for the expression.
#' @param target_indent Target indentation string for continuation lines.
#' @return Expression text with first line unindented, continuation lines re-indented.
#' @keywords internal
extract_expr_text <- function(
    lines,
    tokens,
    target_indent)
{
    if (nrow(tokens) == 0) return("")

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
    first_line_text <- substring(lines[start_line], first_tok$col1)

    # Middle lines: full line content, re-indented
    result_lines <- first_line_text

    if (end_line > start_line) {
        for (ln in (start_line + 1) :end_line) {
            line_text <- lines[ln]
            # Remove existing indentation and add target indent
            trimmed <- sub("^\\s*", "", line_text)
            # Add extra indent for continuation (2 more spaces)
            result_lines <- c(result_lines, paste0("\n", target_indent, "  ", trimmed))
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
format_blank_lines <- function(code) {
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
collapse_calls <- function(code) {
    changed <- TRUE
    max_iterations <- 100

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
collapse_one_call <- function(code) {
    parsed <- tryCatch(
        parse(text = code, keep.source = TRUE),
        error = function(e) NULL
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

    # Find function calls: SYMBOL_FUNCTION_CALL followed by '('
    call_indices <- which(terminals$token == "SYMBOL_FUNCTION_CALL")

    for (ci in call_indices) {
        # Next token should be '('
        open_idx <- ci + 1
        if (open_idx > nrow(terminals)) next
        if (terminals$token[open_idx] != "'('") next

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
            if (paren_depth > 0) close_idx <- close_idx + 1
        }

        if (close_idx > nrow(terminals)) next

        close_line <- terminals$line1[close_idx]

        # Only process multi-line calls
        if (close_line == open_line) next

        # Skip if any token in the call is a comment
        inner_tokens <- terminals[seq(open_idx, close_idx),]
        if (any(inner_tokens$token == "COMMENT")) next

        # Skip if the call contains a FUNCTION definition
        if (any(inner_tokens$token == "FUNCTION")) next

        # Build collapsed text from tokens: func(args)
        call_tokens <- terminals[seq(ci, close_idx),]
        collapsed <- format_line_tokens(call_tokens)

        # Get prefix: everything before the function name on its line
        func_line <- terminals$line1[ci]
        func_col <- terminals$col1[ci]
        if (func_col > 1) {
            prefix <- substring(lines[func_line], 1, func_col - 1)
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
            suffix <- format_line_tokens(after_close)
            # Add space before suffix if needed
            last_call_tok <- call_tokens[nrow(call_tokens),]
            first_after <- after_close[1,]
            if (needs_space(last_call_tok, first_after)) {
                suffix <- paste0(" ", suffix)
            }
        }

        full_line <- paste0(full_line, suffix)

        # Also check if there are tokens before the function call on func_line
        # that aren't part of the prefix (i.e., code tokens before the call)
        before_call <- terminals[terminals$line1 == func_line &
            terminals$col1 < func_col,]

        # Replace the lines
        new_code_lines <- c(
            if (func_line > 1) lines[seq_len(func_line - 1)] else character(0),
            full_line,
            if (close_line < length(lines)) lines[seq(close_line + 1, length(lines))] else character(0)
        )

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
wrap_long_calls <- function(code, line_limit = 80L) {
    changed <- TRUE
    max_iterations <- 100

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
wrap_one_long_call <- function(code, line_limit = 80L) {
    parsed <- tryCatch(
        parse(text = code, keep.source = TRUE),
        error = function(e) NULL
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
        if (open_idx > nrow(terminals)) next
        if (terminals$token[open_idx] != "'('") next

        call_line <- terminals$line1[ci]

        # Only consider single-line calls on lines that are too long
        if (nchar(lines[call_line]) <= line_limit) next

        # Find matching ')'
        paren_depth <- 1
        close_idx <- open_idx + 1
        while (close_idx <= nrow(terminals) && paren_depth > 0) {
            if (terminals$token[close_idx] == "'('") {
                paren_depth <- paren_depth + 1
            } else if (terminals$token[close_idx] == "')'") {
                paren_depth <- paren_depth - 1
            }
            if (paren_depth > 0) close_idx <- close_idx + 1
        }

        if (close_idx > nrow(terminals)) next

        # Only wrap single-line calls
        if (terminals$line1[close_idx] != call_line) next

        # Need at least one comma at depth 1 to have something to wrap
        has_comma <- FALSE
        for (k in seq(open_idx + 1, close_idx - 1)) {
            if (terminals$token[k] == "','" && terminals$line1[k] == call_line) {
                # Check this comma is at depth 1 (not inside nested parens)
                depth <- 0
                for (m in seq(open_idx + 1, k)) {
                    if (terminals$token[m] == "'('") depth <- depth + 1
                    if (terminals$token[m] == "')'") depth <- depth - 1
                }
                if (depth == 0) {
                    has_comma <- TRUE
                    break
                }
            }
        }
        if (!has_comma) next

        # Continuation indent: align to column after opening paren
        open_col <- terminals$col2[open_idx]  # col2 of '(' is its position
        cont_indent <- strrep(" ", open_col)

        # Collect arguments as groups of tokens between commas at depth 1
        args <- list()
        current_arg_tokens <- list()
        depth <- 0

        for (k in seq(open_idx + 1, close_idx - 1)) {
            tok <- terminals[k,]
            if (tok$token == "'('") depth <- depth + 1
            if (tok$token == "')'") depth <- depth - 1

            if (tok$token == "','" && depth == 0) {
                if (length(current_arg_tokens) > 0) {
                    arg_df <- do.call(rbind, lapply(current_arg_tokens, as.data.frame))
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

        if (length(args) < 2) next

        # Get prefix (everything before the function name)
        func_col <- terminals$col1[ci]
        if (func_col > 1) {
            prefix <- substring(lines[call_line], 1, func_col - 1)
        } else {
            prefix <- ""
        }

        func_name <- terminals$text[ci]

        # Get suffix (everything after closing paren on the same line)
        close_col <- terminals$col2[close_idx]
        line_content <- lines[call_line]
        if (close_col < nchar(line_content)) {
            suffix <- substring(line_content, close_col + 1)
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
            if (nchar(test_line) > line_limit && nchar(current_line) > nchar(cont_indent)) {
                new_lines <- c(new_lines, sub(" $", "", current_line))
                current_line <- paste0(cont_indent, arg_text)
            } else {
                current_line <- test_line
            }
        }
        new_lines <- c(new_lines, sub(" $", "", current_line))

        # Only wrap if we actually split into multiple lines
        if (length(new_lines) < 2) next

        # Replace the line
        new_code_lines <- c(
            if (call_line > 1) lines[seq_len(call_line - 1)] else character(0),
            new_lines,
            if (call_line < length(lines)) lines[seq(call_line + 1, length(lines))] else character(0)
        )

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
#' Ensures `else` appears on the same line as the closing brace:
#' \preformatted{
#' }
#' else {
#' }
#' becomes `} else {`.
#'
#' @param code Code string.
#' @return Code with corrected else placement.
#' @keywords internal
fix_else_placement <- function(code) {
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
add_control_braces <- function(code) {
    changed <- TRUE
    max_iterations <- 200

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
add_one_control_brace <- function(code) {
    parsed <- tryCatch(
        parse(text = code, keep.source = TRUE),
        error = function(e) NULL
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
        if (!(tok$token %in% control_keywords)) next

        ctrl_line <- tok$line1

        # Skip if-else used as expression (RHS of assignment)
        if (tok$token == "IF") {
            before <- terminals[terminals$line1 == ctrl_line &
                terminals$col1 < tok$col1,]
            if (nrow(before) > 0 &&
                any(before$token %in% c("LEFT_ASSIGN", "EQ_ASSIGN", "RIGHT_ASSIGN"))) {
                next
            }
        }

        # Find the opening ( after the keyword
        open_idx <- i + 1
        if (open_idx > nrow(terminals)) next
        if (terminals$token[open_idx] != "'('") next

        # Find matching )
        paren_depth <- 1
        close_paren_idx <- open_idx + 1
        while (close_paren_idx <= nrow(terminals) && paren_depth > 0) {
            if (terminals$token[close_paren_idx] == "'('") {
                paren_depth <- paren_depth + 1
            } else if (terminals$token[close_paren_idx] == "')'") {
                paren_depth <- paren_depth - 1
            }
            if (paren_depth > 0) close_paren_idx <- close_paren_idx + 1
        }

        if (close_paren_idx > nrow(terminals)) next

        # Check next token after )
        body_start_idx <- close_paren_idx + 1
        if (body_start_idx > nrow(terminals)) next

        body_start <- terminals[body_start_idx,]

        # Already has braces - skip
        if (body_start$token == "'{'") next

        # Body must be on the same line as the closing paren
        if (body_start$line1 != terminals$line1[close_paren_idx]) next

        # Find end of body expression
        body_end_idx <- body_start_idx
        body_depth <- 0

        while (body_end_idx <= nrow(terminals)) {
            btok <- terminals[body_end_idx,]
            if (btok$token %in% c("'('", "'['", "'[['", "'{'")) {
                body_depth <- body_depth + 1
            } else if (btok$token %in% c("')'", "']'", "']]'", "'}'")) {
                body_depth <- body_depth - 1
                if (body_depth < 0) break
            }

            if (body_depth == 0) {
                next_idx <- body_end_idx + 1
                if (next_idx > nrow(terminals)) break

                next_tok <- terminals[next_idx,]
                if (next_tok$token == "ELSE") break

                if (next_tok$line1 > btok$line1) {
                    cont_tokens <- c("'+'", "'-'", "'*'", "'/'", "'^'",
                        "SPECIAL", "AND", "OR", "AND2", "OR2",
                        "GT", "LT", "GE", "LE", "EQ", "NE",
                        "LEFT_ASSIGN", "EQ_ASSIGN", "'~'", "','")
                    if (!(next_tok$token %in% cont_tokens)) break
                }
            }

            body_end_idx <- body_end_idx + 1
        }

        if (body_end_idx > nrow(terminals)) {
            body_end_idx <- nrow(terminals)
        }

        body_tokens <- terminals[body_start_idx:body_end_idx,]
        body_text <- format_line_tokens(body_tokens)

        has_else <- FALSE
        else_idx <- body_end_idx + 1
        if (else_idx <= nrow(terminals) && terminals$token[else_idx] == "ELSE") {
            has_else <- TRUE
        }

        # Build prefix: everything up to and including )
        close_paren_col <- terminals$col2[close_paren_idx]
        ctrl_line_content <- lines[ctrl_line]
        prefix <- substring(ctrl_line_content, 1, close_paren_col)

        if (has_else) {
            else_tok <- terminals[else_idx,]
            else_body_idx <- else_idx + 1
            if (else_body_idx > nrow(terminals)) next

            else_body_start <- terminals[else_body_idx,]

            if (else_body_start$token == "'{'") {
                new_line <- paste0(prefix, " { ", body_text, " } else {")
                else_body_line <- else_body_start$line1
                new_code_lines <- c(
                    if (ctrl_line > 1) lines[seq_len(ctrl_line - 1)] else character(0),
                    new_line,
                    if (else_body_line < length(lines)) lines[seq(else_body_line + 1, length(lines))] else character(0)
                )
            } else if (else_body_start$token == "IF") {
                new_line <- paste0(prefix, " { ", body_text, " } else")
                new_code_lines <- c(
                    if (ctrl_line > 1) lines[seq_len(ctrl_line - 1)] else character(0),
                    new_line,
                    if (else_tok$line1 < length(lines)) lines[seq(else_tok$line1 + 1, length(lines))] else character(0)
                )
            } else {
                # Both branches bare
                else_end_idx <- else_body_idx
                else_depth <- 0
                while (else_end_idx <= nrow(terminals)) {
                    etok <- terminals[else_end_idx,]
                    if (etok$token %in% c("'('", "'['", "'[['", "'{'")) {
                        else_depth <- else_depth + 1
                    } else if (etok$token %in% c("')'", "']'", "']]'", "'}'")) {
                        else_depth <- else_depth - 1
                        if (else_depth < 0) break
                    }
                    if (else_depth == 0) {
                        next_idx2 <- else_end_idx + 1
                        if (next_idx2 > nrow(terminals)) break
                        if (terminals$line1[next_idx2] > etok$line1) break
                    }
                    else_end_idx <- else_end_idx + 1
                }
                if (else_end_idx > nrow(terminals)) {
                    else_end_idx <- nrow(terminals)
                }

                else_body_tokens <- terminals[else_body_idx:else_end_idx,]
                else_body_text <- format_line_tokens(else_body_tokens)
                else_end_line <- terminals$line1[else_end_idx]

                new_line <- paste0(prefix, " { ", body_text, " } else { ", else_body_text, " }")
                new_code_lines <- c(
                    if (ctrl_line > 1) lines[seq_len(ctrl_line - 1)] else character(0),
                    new_line,
                    if (else_end_line < length(lines)) lines[seq(else_end_line + 1, length(lines))] else character(0)
                )
            }
        } else {
            body_end_line <- terminals$line1[body_end_idx]
            new_line <- paste0(prefix, " { ", body_text, " }")
            new_code_lines <- c(
                if (ctrl_line > 1) lines[seq_len(ctrl_line - 1)] else character(0),
                new_line,
                if (body_end_line < length(lines)) lines[seq(body_end_line + 1, length(lines))] else character(0)
            )
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
