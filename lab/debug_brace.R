library(rformat)

code <- "status <- if (traceable &&\n              isTRUE(correct)) \"PASS\" else if (traceable) sprintf(\"TRACE-OK (%d breaks)\",\n                                                                  n_breaks) else sprintf(\"%d breaks\",\n                                                                                         n_breaks)"

cat("Input code:\n")
cat(code, "\n\n")

cat("Calling add_one_control_brace...\n")
result <- rformat:::add_one_control_brace(code)
if (is.null(result)) {
    cat("Result: NULL (no changes)\n")
} else {
    cat("Result:\n", result, "\n")
}
