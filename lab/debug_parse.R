library(rformat)

# Format the file
code <- paste(readLines("lab/type_spineplot.R"), collapse = "\n")
r1 <- rformat:::format_tokens(code)
fmt_lines <- strsplit(r1, "\n")[[1]]

# Find the problematic section by progressively parsing
# Start from the end and work backwards to find what's unclosed
# Parse the whole thing and let R tell us
err <- tryCatch(
    parse(text = r1),
    error = function(e) conditionMessage(e)
)
cat("Full file parse error:", err, "\n\n")

# Now use token-based approach on original to find what might get lost
parsed <- parse(text = code, keep.source = TRUE)
pd <- getParseData(parsed)
terminals <- pd[pd$terminal,]
terminals <- terminals[order(terminals$line1, terminals$col1),]

# Check for multiline tokens that might cause issues
multi <- terminals[terminals$line2 > terminals$line1,]
cat("Multiline tokens:\n")
for (i in seq_len(nrow(multi))) {
    cat(sprintf("  %s at lines %d-%d: %s\n",
        multi$token[i], multi$line1[i], multi$line2[i],
        substr(multi$text[i], 1, 40)))
}
