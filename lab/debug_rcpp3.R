library(rformat)

# The code JUST BEFORE the corrupting wrap pass
# (after 2 successful wraps at lines 18 and 23)
code <- paste(readLines("/tmp/rcpp_debug/Rcpp/R/03_prompt.R", warn = FALSE),
              collapse = "\n")

# Apply first 2 wraps manually by running format then extracting intermediate
# Actually, let's construct the problematic state directly.
# Line 42 (0-indexed from format output) is inside the sprintf() multiline string.

# Let me just check what the code looks like at the point of wrapping line 42.
orig_wrap <- rformat:::wrap_one_long_call
call_count <- 0

capture_wrap <- function(code, line_limit = 80L) {
    call_count <<- call_count + 1
    if (call_count == 3) {
        # This is the wrap that corrupts line 42
        lines <- strsplit(code, "\n")[[1]]
        cat("=== Code at time of 3rd wrap (line 42 area) ===\n")
        for (i in 38:46) {
            cat(sprintf("  L%d [%d]: %s\n", i, nchar(lines[i]), lines[i]))
        }

        # Parse and show tokens on line 42
        parsed <- parse(text = code, keep.source = TRUE)
        pd <- getParseData(parsed)
        terminals <- pd[pd$terminal,]
        terminals <- terminals[order(terminals$line1, terminals$col1),]

        cat("\n=== Tokens on line 42 ===\n")
        t42 <- terminals[terminals$line1 == 42,]
        for (i in seq_len(nrow(t42))) {
            cat(sprintf("  [%d] C%d-%d %s %s\n",
                        which(rownames(terminals) == rownames(t42)[i]),
                        t42$col1[i], t42$col2[i], t42$token[i],
                        substr(t42$text[i], 1, 50)))
        }

        # Check which function calls on line 42
        fc42 <- t42[t42$token == "SYMBOL_FUNCTION_CALL",]
        cat("\n=== Function calls on line 42 ===\n")
        for (i in seq_len(nrow(fc42))) {
            cat(sprintf("  %s at C%d\n", fc42$text[i], fc42$col1[i]))

            # Check paren_depth_before
            before <- t42[t42$col1 < fc42$col1[i],]
            pd_before <- sum(before$token == "'('") - sum(before$token == "')'")
            cat(sprintf("    paren_depth_before = %d\n", pd_before))
        }
    }
    orig_wrap(code, line_limit)
}

environment(capture_wrap) <- asNamespace("rformat")
assignInNamespace("wrap_one_long_call", capture_wrap, ns = "rformat")

result <- rformat(code)

assignInNamespace("wrap_one_long_call", orig_wrap, ns = "rformat")
