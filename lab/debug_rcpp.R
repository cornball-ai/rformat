library(rformat)

code <- paste(readLines("/tmp/rcpp_debug/Rcpp/R/03_prompt.R", warn = FALSE),
              collapse = "\n")

# Step through the passes manually (mimic format_tokens)
parsed <- parse(text = code, keep.source = TRUE)
pd <- getParseData(parsed)
terminals <- pd[pd$terminal,]
terminals <- terminals[order(terminals$line1, terminals$col1),]

# Pass 1: format_line_tokens + indentation (the full format_tokens first pass)
# Just run the full thing up to before wrapping
r1 <- rformat:::format_line_tokens(terminals)
cat("=== After format_line_tokens (just tokens, no indent) ===\n")
# Find the sprintf with multiline string
lines1 <- strsplit(r1, "\n")[[1]]
idx <- grep("paste.*sprintf.*item", lines1)
if (length(idx) > 0) {
    for (i in idx) cat(sprintf("  L%d: %s\n", i, lines1[i]))
}

# Now run the actual formatter to see where it goes wrong
# We need to trace each pass inside format_tokens
# Let's use the actual function but check intermediate results

# Run full format and check each step
# Since we can't hook into format_tokens, let's call the individual passes

# The full format_tokens does:
# 1. format_line_tokens + indentation => code1
# 2. collapse_calls(code1)
# 3. wrap_long_operators
# 4. wrap_long_calls (1st)
# 5. fix_else_placement
# 6. add_control_braces
# 7. reformat_function_defs
# 8. reformat_inline_if
# 9. wrap_long_calls (final)
# 10. format_blank_lines

# Get result after step 1 (full format_tokens with all passes)
# Instead, let's just check: does the problem exist after collapse?
# We need the full indented output from format_tokens step 1

# Actually, let's just diff what format_tokens produces vs what parses
result <- rformat(code)
lines_r <- strsplit(result, "\n")[[1]]

# Find the problematic sprintf line
idx <- grep("paste.*sprintf.*item", lines_r)
cat("\n=== Lines with paste+sprintf+item in output ===\n")
for (i in idx) {
    start <- max(1, i - 2)
    end <- min(length(lines_r), i + 2)
    for (j in start:end) {
        cat(sprintf("  %s L%d: %s\n", if (j == i) ">>>" else "   ", j, lines_r[j]))
    }
}

# Also check: does the line with paste(sprintf...) have the nesting issue?
# Parse the formatted code and look at token structure
cat("\n=== Token structure of formatted output ===\n")
p2 <- tryCatch(parse(text = result, keep.source = TRUE), error = function(e) NULL)
if (is.null(p2)) {
    cat("Cannot parse formatted output\n")

    # Try parsing line by line to find first error
    for (i in seq_along(lines_r)) {
        partial <- paste(lines_r[1:i], collapse = "\n")
        p <- tryCatch(parse(text = partial, keep.source = TRUE),
                       error = function(e) e)
        if (inherits(p, "error")) {
            cat(sprintf("First parse error at line %d: %s\n", i, p$message))
            cat("Lines around error:\n")
            start <- max(1, i - 5)
            for (j in start:i) {
                cat(sprintf("  L%d: %s\n", j, lines_r[j]))
            }
            break
        }
    }
}
