library(rformat)

# Format the file
code <- paste(readLines("lab/type_spineplot.R"), collapse = "\n")
r1 <- rformat:::format_tokens(code)
fmt_lines <- strsplit(r1, "\n")[[1]]

# Try to find the specific issue by parsing subsections
# We know the full inner function (84-303) doesn't parse
# Let me find which statement is broken by testing each top-level statement

# Parse the inner function body line by line, tracking what's complete
brace_depth <- 1  # we're inside {
paren_depth <- 0
stmt_start <- 85  # first line of body

for (i in 85:303) {
    line <- fmt_lines[i]
    for (ch in strsplit(line, "")[[1]]) {
        if (ch == "{") brace_depth <- brace_depth + 1
        if (ch == "}") brace_depth <- brace_depth - 1
        if (ch == "(") paren_depth <- paren_depth + 1
        if (ch == ")") paren_depth <- paren_depth - 1
    }

    # When we're back to depth 1 and paren_depth 0, try parsing
    if (brace_depth == 1 && paren_depth == 0) {
        chunk <- paste(fmt_lines[stmt_start:i], collapse = "\n")
        ok <- tryCatch({parse(text = chunk); TRUE}, error = function(e) FALSE)
        if (!ok) {
            cat(sprintf("Broken statement: lines %d-%d\n", stmt_start, i))
            cat(paste(fmt_lines[stmt_start:min(i, stmt_start+10)], collapse = "\n"), "\n...\n")
            err <- tryCatch(parse(text = chunk), error = function(e) conditionMessage(e))
            cat("Error:", err, "\n\n")
        }
        stmt_start <- i + 1
    }
}
