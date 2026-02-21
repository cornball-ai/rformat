library(rformat)
code <- "foo <- function(a = 1, b = 2, c = 3, d = 4, e = 5, f = 6, g = 7, h = 8, i = 9) {\n    body\n}\n"
parsed <- parse(text = code, keep.source = TRUE)
pd <- getParseData(parsed)
terminals <- pd[pd$terminal,]
terminals <- terminals[order(terminals$line1, terminals$col1),]

fi <- which(terminals$token == "FUNCTION")[1]
func_tok <- terminals[fi,]
next_idx <- fi + 1

# Find close paren
paren_depth <- 1
close_idx <- next_idx + 1
while (close_idx <= nrow(terminals) && paren_depth > 0) {
    if (terminals$token[close_idx] == "'('") paren_depth <- paren_depth + 1
    if (terminals$token[close_idx] == "')'") paren_depth <- paren_depth - 1
    if (paren_depth > 0) close_idx <- close_idx + 1
}

cat("close_idx token:", terminals$token[close_idx], "line:", terminals$line1[close_idx], "\n")
cat("close_idx+1 token:", terminals$token[close_idx + 1], "\n")

has_brace <- close_idx + 1 <= nrow(terminals) &&
    terminals$token[close_idx + 1] == "'{'"
cat("has_brace:", has_brace, "\n")
brace_line <- terminals$line1[close_idx + 1]
cat("brace_line:", brace_line, "\n")

lines <- strsplit(code, "\n", fixed = TRUE)[[1]]
func_line <- func_tok$line1
func_line_content <- lines[func_line]
base_indent <- sub("^(\\s*).*", "\\1", func_line_content)
prefix <- substring(func_line_content, 1, func_tok$col1 - 1)
cat("prefix:", repr::repr(prefix), "\n")

# single_line_sig
formals_text <- "a = 1, b = 2, c = 3, d = 4, e = 5, f = 6, g = 7, h = 8, i = 9"
single_line_sig <- paste0(prefix, "function (", formals_text, ")")
cat("single_line_sig:", nchar(single_line_sig), "chars\n")
cat(single_line_sig, "\n")

# Check what old_lines vs new_lines would be
end_line <- brace_line
old_lines <- lines[func_line:end_line]
cat("old_lines:\n")
cat(paste(old_lines, collapse = "\n"), "\n")

new_lines <- single_line_sig
# K&R brace
new_lines <- paste0(new_lines, " {")
cat("new_lines:\n")
cat(new_lines, "\n")
cat("identical:", identical(old_lines, new_lines), "\n")
