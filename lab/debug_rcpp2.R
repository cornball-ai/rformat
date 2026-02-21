library(rformat)

code <- paste(readLines("/tmp/rcpp_debug/Rcpp/R/03_prompt.R", warn = FALSE),
              collapse = "\n")

# Run format_tokens but stop before wrap_long_calls
# We need to replicate the pipeline. Read the format_tokens source to get
# the exact sequence.

# Parse and get indented output (full format_tokens minus wrapping)
# Actually, let's just use a targeted approach:
# Add tracing to wrap_one_long_call by monkey-patching

orig_wrap <- rformat:::wrap_one_long_call

trace_wrap <- function(code, line_limit = 80L) {
    result <- orig_wrap(code, line_limit)
    if (!is.null(result)) {
        # Find what changed
        old_lines <- strsplit(code, "\n")[[1]]
        new_lines <- strsplit(result, "\n")[[1]]

        # Find first differing line
        min_len <- min(length(old_lines), length(new_lines))
        for (i in seq_len(min_len)) {
            if (old_lines[i] != new_lines[i]) {
                cat(sprintf("--- wrap_one_long_call changed line %d ---\n", i))
                cat(sprintf("  BEFORE: %s\n", old_lines[i]))
                cat(sprintf("  AFTER:  %s\n", new_lines[i]))
                if (i + 1 <= length(new_lines)) {
                    cat(sprintf("  AFTER+1: %s\n", new_lines[i + 1]))
                }
                break
            }
        }
    }
    result
}

# Temporarily replace
environment(trace_wrap) <- asNamespace("rformat")
assignInNamespace("wrap_one_long_call", trace_wrap, ns = "rformat")

# Now run rformat
result <- rformat(code)
cat("\n=== Parse check ===\n")
p <- tryCatch(parse(text = result), error = function(e) e)
if (inherits(p, "error")) {
    cat("PARSE ERROR:", p$message, "\n")
} else {
    cat("OK\n")
}

# Restore
assignInNamespace("wrap_one_long_call", orig_wrap, ns = "rformat")
