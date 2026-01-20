#' Format R Code Using Token-Based Parsing
#'
#' Internal function to format R code using getParseData tokens.
#' Calculates proper indentation based on nesting depth.
#'
#' @param code Character string of R code.
#' @return Formatted code as character string.
#' @keywords internal
format_tokens <- function(code) {

  # Parse with source tracking
  parsed <- tryCatch(
    parse(text = code, keep.source = TRUE),
    error = function(e) NULL
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
  indent_str <- "  "
  out_lines <- character(length(orig_lines))

  for (line_num in seq_along(orig_lines)) {
    line <- orig_lines[line_num]

    if (grepl("^\\s*$", line)) {
      out_lines[line_num] <- ""
      next
    }

    line_tokens <- terminals[terminals$line1 == line_num,]

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

  result <- paste(out_lines, collapse = "\n")
  if (!grepl("\n$", result) && nchar(result) > 0) {
    result <- paste0(result, "\n")
  }

  # Reformat function definitions
  result <- reformat_function_defs(result)

  # Reformat inline if-else to multi-line
  result <- reformat_inline_if(result)

  result
}

#' Reformat Function Definitions
#'
#' Ensures function definitions follow style guide:
#' - First argument on new line after function(
#' - Each argument on its own line
#' - Closing ) { on its own line
#'
#' @param code Formatted code string.
#' @return Code with reformatted function definitions.
#' @keywords internal
reformat_function_defs <- function(code) {

  # Process one function at a time, re-parsing each time
  # to handle line number changes
  changed <- TRUE
  max_iterations <- 50

  while (changed && max_iterations > 0) {
    max_iterations <- max_iterations - 1
    changed <- FALSE

    result <- reformat_one_function(code)
    if (!is.null(result)) {
      code <- result
      changed <- TRUE
    }
  }

  code
}

#' Reformat One Function Definition
#'
#' @param code Code string.
#' @return Modified code or NULL if no changes.
#' @keywords internal
reformat_one_function <- function(code) {

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

    open_paren_line <- terminals$line1[next_idx]

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

    # Get all formals between ( and )
    formal_indices <- which(
      terminals$token == "SYMBOL_FORMALS" &
      seq_len(nrow(terminals)) > next_idx &
      seq_len(nrow(terminals)) < close_idx
    )

    # Skip if fewer than 2 formals (don't split function(x) or function(e))
    if (length(formal_indices) < 2) next

    # Check if first formal is on same line as function(
    first_formal_line <- terminals$line1[formal_indices[1]]
    if (first_formal_line != func_line) next

    # Need to reformat - extract the function signature
    # Find base indent (everything before 'function')
    func_line_content <- lines[func_line]
    base_indent <- sub("^(\\s*).*", "\\1", func_line_content)

    # Find what comes before 'function' on the line
    prefix_match <- regmatches(
      func_line_content,
      regexpr("^.*?(?=function)", func_line_content, perl = TRUE)
    )
    if (length(prefix_match) > 0) {
      prefix <- prefix_match
    } else {
      prefix <- ""
    }

    # Build new function signature
    new_lines <- character(0)

    # First line: prefix + function(
    new_lines <- c(new_lines, paste0(prefix, "function("))

    # Collect all formals with their defaults
    arg_indent <- paste0(base_indent, "  ")
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

      if (terminals$token[i] == "','") {
        # Skip comma, we'll add our own
      }

      i <- i + 1
    }

    # Add each formal on its own line
    for (j in seq_along(formal_texts)) {
      if (j < length(formal_texts)) {
        comma <- ","
      } else {
        comma <- ""
      }
      new_lines <- c(new_lines, paste0(arg_indent, formal_texts[j], comma))
    }

    # Check if there's a { after the )
    has_brace <- FALSE
    if (close_idx + 1 <= nrow(terminals) &&
      terminals$token[close_idx + 1] == "'{'") {
      has_brace <- TRUE
    }

    # Check if there's a function body without braces (single expression body)
    # This handles cases like: function(x, y) if (is.null(x)) y else x
    has_inline_body <- FALSE
    inline_body <- ""
    if (!has_brace && close_idx + 1 <= nrow(terminals)) {
      # There are tokens after ) that aren't { - this is an inline body
      body_tokens <- terminals[terminals$line1 >= close_paren_line &
      seq_len(nrow(terminals)) > close_idx,]
      if (nrow(body_tokens) > 0) {
        has_inline_body <- TRUE
        # Get the original text from close_paren position to end of expression
        # Find all lines that are part of this expression
        body_end_line <- max(body_tokens$line1)
        body_lines <- lines[close_paren_line:body_end_line]
        # Get everything after the ) on the first line
        first_body_line <- body_lines[1]
        close_paren_col <- terminals$col2[close_idx]
        inline_body <- substring(first_body_line, close_paren_col + 1)
        if (length(body_lines) > 1) {
          inline_body <- paste(c(inline_body, body_lines[- 1]), collapse = "\n")
        }
        inline_body <- trimws(inline_body)
      }
    }

    # Add closing line
    if (has_brace) {
      new_lines <- c(new_lines, paste0(base_indent, ") {"))
    } else if (has_inline_body) {
      new_lines <- c(new_lines, paste0(base_indent, ") ", inline_body))
    } else {
      new_lines <- c(new_lines, paste0(base_indent, ")"))
    }

    # Replace lines from func_line to close_paren_line (and { if present)
    end_line <- close_paren_line
    if (has_brace && terminals$line1[close_idx + 1] == close_paren_line) {
      # { is on same line as )
    } else if (has_brace) {
      end_line <- terminals$line1[close_idx + 1]
    } else if (has_inline_body) {
      # Include all lines of the inline body
      body_tokens <- terminals[terminals$line1 >= close_paren_line &
      seq_len(nrow(terminals)) > close_idx,]
      if (nrow(body_tokens) > 0) {
        end_line <- max(body_tokens$line1)
      }
    }

    # Replace the lines and return
    new_code_lines <- c(
      lines[seq_len(func_line - 1)],
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
    if_idx <- NULL
    for (j in (i + 1) :nrow(terminals)) {
      if (j > nrow(terminals)) break
      next_tok <- terminals[j,]
      if (next_tok$line1 != assign_line) break
      if (next_tok$token == "IF") {
        if_idx <- j
        break
      }
    }

    if (is.null(if_idx)) next

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

    if (!is.null(prev)) {
      if (needs_space(prev, tok)) {
        parts[i] <- paste0(" ", tok$text)
      } else {
        parts[i] <- tok$text
      }
    } else {
      parts[i] <- tok$text
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
  tok
) {

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
  target_indent
) {
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
  code <- gsub("\n{3,}", "\n\n", code)
  code <- gsub("\n+$", "\n", code)
  code
}

