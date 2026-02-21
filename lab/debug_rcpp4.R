library(rformat)

# Reproduce exactly what wrap_one_long_call does for paste() on line 42
# of the Rcpp file after 2 successful wraps

code <- paste(readLines("/tmp/rcpp_debug/Rcpp/R/03_prompt.R", warn = FALSE),
              collapse = "\n")

# Apply first 2 wraps
orig_wrap <- rformat:::wrap_one_long_call
call_count <- 0

get_state <- function(code, line_limit = 80L) {
    call_count <<- call_count + 1
    if (call_count == 3) {
        # This is the state before the 3rd wrap
        # Now manually simulate wrap_one_long_call for paste on line 42
        parsed <- parse(text = code, keep.source = TRUE)
        pd <- getParseData(parsed)
        terminals <- pd[pd$terminal,]
        terminals <- terminals[order(terminals$line1, terminals$col1),]
        lines <- strsplit(code, "\n", fixed = TRUE)[[1]]

        # Find paste on line 42
        ci <- which(terminals$token == "SYMBOL_FUNCTION_CALL" &
                        terminals$text == "paste" & terminals$line1 == 42)[1]
        open_idx <- ci + 1

        # Find matching )
        paren_depth <- 1
        close_idx <- open_idx + 1
        while (close_idx <= nrow(terminals) && paren_depth > 0) {
            if (terminals$token[close_idx] == "'('") paren_depth <- paren_depth + 1
            if (terminals$token[close_idx] == "')'") paren_depth <- paren_depth - 1
            if (paren_depth > 0) close_idx <- close_idx + 1
        }
        cat("close_idx token:", terminals$token[close_idx],
            "line:", terminals$line1[close_idx],
            "col:", terminals$col2[close_idx], "\n")

        # Collect args
        args_text <- list()
        current_arg_tokens <- list()
        depth <- 0
        for (k in seq(open_idx + 1, close_idx - 1)) {
            tok <- terminals[k,]
            if (tok$token == "'('") depth <- depth + 1
            if (tok$token == "')'") depth <- depth - 1
            if (tok$token == "','" && depth == 0) {
                if (length(current_arg_tokens) > 0) {
                    arg_df <- do.call(rbind, lapply(current_arg_tokens, as.data.frame))
                    arg_text <- rformat:::format_line_tokens(arg_df)
                    args_text[[length(args_text) + 1]] <- arg_text
                    cat(sprintf("Arg %d tokens: %s\n", length(args_text),
                                paste(sapply(current_arg_tokens, function(t) t$text), collapse=" ")))
                    cat(sprintf("Arg %d formatted: [%s]\n", length(args_text), arg_text))
                }
                current_arg_tokens <- list()
            } else {
                current_arg_tokens[[length(current_arg_tokens) + 1]] <- tok
            }
        }
        if (length(current_arg_tokens) > 0) {
            arg_df <- do.call(rbind, lapply(current_arg_tokens, as.data.frame))
            arg_text <- rformat:::format_line_tokens(arg_df)
            args_text[[length(args_text) + 1]] <- arg_text
            cat(sprintf("Arg %d tokens: %s\n", length(args_text),
                        paste(sapply(current_arg_tokens, function(t) t$text), collapse=" ")))
            cat(sprintf("Arg %d formatted: [%s]\n", length(args_text), arg_text))
        }

        # Prefix and suffix
        func_col <- terminals$col1[ci]
        prefix <- substring(lines[42], 1, func_col - 1)
        cat(sprintf("\nprefix: [%s]\n", prefix))
        cat(sprintf("func_name: %s\n", terminals$text[ci]))

        close_col <- terminals$col2[close_idx]
        if (close_col < nchar(lines[42])) {
            suffix <- substring(lines[42], close_col + 1)
        } else {
            suffix <- ""
        }
        cat(sprintf("suffix: [%s]\n", suffix))

        # Build wrapped
        func_name <- terminals$text[ci]
        open_col <- terminals$col2[open_idx]
        cont_indent <- strrep(" ", open_col)
        new_lines_out <- character(0)
        current_line <- paste0(prefix, func_name, "(")

        cat(sprintf("\nStarting line: [%s]\n", current_line))

        for (j in seq_along(args_text)) {
            if (j < length(args_text)) {
                at <- paste0(args_text[[j]], ", ")
            } else {
                at <- paste0(args_text[[j]], ")", suffix)
            }
            test_line <- paste0(current_line, at)
            cat(sprintf("  test_line [%d chars]: [%s]\n", nchar(test_line), test_line))
            if (nchar(test_line) > 80 && nchar(current_line) > nchar(cont_indent)) {
                new_lines_out <- c(new_lines_out, sub(" $", "", current_line))
                current_line <- paste0(cont_indent, at)
            } else {
                current_line <- test_line
            }
        }
        new_lines_out <- c(new_lines_out, sub(" $", "", current_line))

        cat("\nRebuilt lines:\n")
        for (l in new_lines_out) cat(sprintf("  [%s]\n", l))
    }
    orig_wrap(code, line_limit)
}

environment(get_state) <- asNamespace("rformat")
assignInNamespace("wrap_one_long_call", get_state, ns = "rformat")

result <- rformat(code)

assignInNamespace("wrap_one_long_call", orig_wrap, ns = "rformat")
