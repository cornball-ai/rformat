# rformat dogfooding bugs - patterns from R/format_parsed.R that break

# Bug 1: Space before [[
# strsplit(...)[[1]] gets space inserted before [[
lines <- strsplit(code, "\n", fixed = TRUE)[[1]]

# Bug 2: Space before : operator
# (tok$line1 + 1):tok$line2 gets space before :
for (ln in (tok$line1 + 1):tok$line2) {
    x <- ln
}

# Bug 3: c() with inline if-else gets expanded (BREAKS CODE)
# add_control_braces shouldn't touch if-else inside c()
new_code_lines <- c(
    if (func_line > 1) lines[seq_len(func_line - 1)] else character(0),
    new_lines,
    if (end_line < length(lines)) lines[seq(end_line + 1, length(lines))] else character(0)
)

# Bug 4: Trailing comments lose spacing
# "code  # comment" becomes "code# comment"
false_if_depth <- 0  # track nested if-else
open_col <- terminals$col2[open_idx]  # col2 of '(' is its position

# Bug 5: Unary minus inside [ spaced wrong
# x[-length(x)] becomes x[- length(x)]
paren_at_brace <- paren_at_brace[-length(paren_at_brace)]

# Bug 6: || continuation indent wrong
# Should align to expression start, not use fixed 4-space indent
spans_lines <- false_end_line > assign_line ||
    true_end_line > assign_line

# Bug 7: Condition continuation in [ misaligned
# terminals[cond &\n  cond2,] loses alignment
after_close <- terminals[terminals$line1 == close_line &
    terminals$col1 > terminals$col1[close_idx],]
before_call <- terminals[terminals$line1 == func_line &
    terminals$col1 < func_col,]
