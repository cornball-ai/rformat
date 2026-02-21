library(rformat)

code <- paste0(
    'sprintf("functions: \\\\describe{\n',
    '%s\n',
    '\t\t}", paste(sprintf("        \\\\item{%s}{ ~~ description of function %s ~~ }", names(info), names(info)), collapse = "\n"))'
)

cat("=== Input ===\n")
cat(code, "\n\n")

parsed <- parse(text = code, keep.source = TRUE)
pd <- getParseData(parsed)
terminals <- pd[pd$terminal,]
terminals <- terminals[order(terminals$line1, terminals$col1),]
lines <- strsplit(code, "\n", fixed = TRUE)[[1]]

cat("=== Lines ===\n")
for (i in seq_along(lines)) {
    cat(sprintf("  L%d [%d chars]: %s\n", i, nchar(lines[i]), lines[i]))
}

cat("\n=== All tokens ===\n")
for (i in seq_len(nrow(terminals))) {
    cat(sprintf("  [%d] L%d C%d-%d %s %s\n",
                i, terminals$line1[i], terminals$col1[i], terminals$col2[i],
                terminals$token[i], substr(terminals$text[i], 1, 40)))
}

# Which function calls would be considered?
call_indices <- which(terminals$token == "SYMBOL_FUNCTION_CALL")
cat("\n=== Function calls ===\n")
for (ci in call_indices) {
    call_line <- terminals$line1[ci]
    line_len <- nchar(lines[call_line])
    cat(sprintf("  %s at L%d C%d (line len=%d, limit=80, too_long=%s)\n",
                terminals$text[ci], call_line, terminals$col1[ci],
                line_len, line_len > 80))
}

# Manually simulate what wrap_one_long_call does for sprintf at L3 C27
ci <- which(terminals$token == "SYMBOL_FUNCTION_CALL" &
                terminals$text == "sprintf" & terminals$line1 == 3)[1]
cat("\n=== Simulating wrap for token", ci, ":", terminals$text[ci], "===\n")
open_idx <- ci + 1
call_line <- terminals$line1[ci]

# Find matching )
paren_depth <- 1
close_idx <- open_idx + 1
while (close_idx <= nrow(terminals) && paren_depth > 0) {
    if (terminals$token[close_idx] == "'('") paren_depth <- paren_depth + 1
    if (terminals$token[close_idx] == "')'") paren_depth <- paren_depth - 1
    if (paren_depth > 0) close_idx <- close_idx + 1
}
cat("close_idx =", close_idx, "line =", terminals$line1[close_idx], "\n")

# Prefix
func_col <- terminals$col1[ci]
prefix <- substring(lines[call_line], 1, func_col - 1)
cat("prefix: [", prefix, "]\n")

# Suffix
close_col <- terminals$col2[close_idx]
line_content <- lines[call_line]
cat("close_col:", close_col, "nchar(line):", nchar(line_content), "\n")
if (close_col < nchar(line_content)) {
    suffix <- substring(line_content, close_col + 1)
} else {
    suffix <- ""
}
cat("suffix: [", suffix, "]\n")

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
            args_text[[length(args_text) + 1]] <- rformat:::format_line_tokens(arg_df)
        }
        current_arg_tokens <- list()
    } else {
        current_arg_tokens[[length(current_arg_tokens) + 1]] <- tok
    }
}
if (length(current_arg_tokens) > 0) {
    arg_df <- do.call(rbind, lapply(current_arg_tokens, as.data.frame))
    args_text[[length(args_text) + 1]] <- rformat:::format_line_tokens(arg_df)
}

cat("\nArgs collected:", length(args_text), "\n")
for (i in seq_along(args_text)) {
    cat(sprintf("  arg %d: [%s]\n", i, args_text[[i]]))
}

# Build wrapped output
func_name <- terminals$text[ci]
open_col <- terminals$col2[open_idx]
cont_indent <- strrep(" ", open_col)
new_lines <- character(0)
current_line <- paste0(prefix, func_name, "(")
for (j in seq_along(args_text)) {
    if (j < length(args_text)) {
        arg_text <- paste0(args_text[[j]], ", ")
    } else {
        arg_text <- paste0(args_text[[j]], ")", suffix)
    }
    test_line <- paste0(current_line, arg_text)
    if (nchar(test_line) > 80 && nchar(current_line) > nchar(cont_indent)) {
        new_lines <- c(new_lines, sub(" $", "", current_line))
        current_line <- paste0(cont_indent, arg_text)
    } else {
        current_line <- test_line
    }
}
new_lines <- c(new_lines, sub(" $", "", current_line))
cat("\nRebuilt lines:\n")
for (l in new_lines) cat(sprintf("  [%s]\n", l))

# Now actually run wrap_one_long_call
cat("\n=== wrap_one_long_call output ===\n")
result <- rformat:::wrap_one_long_call(code, 80L)
if (is.null(result)) {
    cat("NULL (no changes)\n")
} else {
    cat(result, "\n")
}

# Check if output parses
if (!is.null(result)) {
    p <- tryCatch(parse(text = result), error = function(e) e)
    if (inherits(p, "error")) {
        cat("\nPARSE ERROR:", p$message, "\n")
    } else {
        cat("\nParses OK\n")
    }
}
