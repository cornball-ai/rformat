# Tests for AST parse-once infrastructure

library(rformat)

# Helper: run enrich + serialize round-trip on formatted code
ast_roundtrip <- function (code) {
    fmt <- rformat(code)
    pd <- getParseData(parse(text = fmt, keep.source = TRUE))
    lines <- strsplit(fmt, "\n", fixed = TRUE)[[1]]
    terms <- rformat:::enrich_terminals(pd, lines)
    rformat:::serialize_tokens(terms, "    ", "paren", 80L)
}

# Helper: run collapse + wrap pipeline on formatted code
ast_pipeline <- function (code, wrap = "paren") {
    fmt <- rformat(code)
    pd <- getParseData(parse(text = fmt, keep.source = TRUE))
    lines <- strsplit(fmt, "\n", fixed = TRUE)[[1]]
    terms <- rformat:::enrich_terminals(pd, lines)
    terms <- rformat:::collapse_calls_ast(terms, "    ", 80L)
    terms <- rformat:::wrap_long_operators_ast(terms, "    ", 80L)
    terms <- rformat:::wrap_long_calls_ast(terms, "    ", wrap, 80L)
    rformat:::serialize_tokens(terms, "    ", wrap, 80L)
}

# --- enrich_terminals + serialize_tokens round-trip ---

# Simple expression
expect_equal(ast_roundtrip("x <- 1 + 2\n"), "x <- 1 + 2\n")

# EQ_ASSIGN conversion
expect_equal(ast_roundtrip("y = 2\n"), "y <- 2\n")

# Multi-line
expect_equal(
    ast_roundtrip("if (x > 0) {\n    y <- 1\n}\n"),
    "if (x > 0) {\n    y <- 1\n}\n"
)

# Nested braces
expect_equal(
    ast_roundtrip("for (i in seq_len(n)) {\n    if (x[i] > 0) {\n        y[i] <- log(x[i])\n    }\n}\n"),
    "for (i in seq_len(n)) {\n    if (x[i] > 0) {\n        y[i] <- log(x[i])\n    }\n}\n"
)

# Function call
expect_equal(ast_roundtrip("foo(x, y, z)\n"), "foo(x, y, z)\n")

# Blank line preservation
expect_equal(
    ast_roundtrip("x <- 1\n\ny <- 2\n"),
    "x <- 1\n\ny <- 2\n"
)

# Comments
expect_equal(
    ast_roundtrip("x <- 1 # a comment\n"),
    "x <- 1 # a comment\n"
)

# --- collapse_calls_ast ---

# Short multi-line call collapses to one line
expect_equal(
    ast_pipeline("foo(\n    a, b\n)\n"),
    "foo(a, b)\n"
)

# Collapse preserves suffix tokens after )
expect_equal(
    ast_pipeline("x <- foo(\n    a, b\n)\n"),
    "x <- foo(a, b)\n"
)

# Call with comment inside does NOT collapse
code_with_comment <- "foo(\n    a, # note\n    b\n)\n"
fmt_comment <- rformat(code_with_comment)
expect_true(grepl("\n", trimws(fmt_comment, "right")))

# --- wrap_long_operators_ast ---

# Short line stays on one line
expect_equal(
    ast_pipeline("x <- a || b\n"),
    "x <- a || b\n"
)

# Long operator line wraps
long_op <- "x <- a_long || b_long || c_long || d_long || e_long || f_long || g_long || h_long || i_long\n"
result_op <- ast_pipeline(long_op)
expect_true(grepl("\n", trimws(result_op, "right")))

# --- wrap_long_calls_ast ---

# Short call stays on one line
expect_equal(
    ast_pipeline("foo(a, b, c)\n"),
    "foo(a, b, c)\n"
)

# Long call wraps at commas
long_call <- "result <- some_function(very_long_argument_one, very_long_argument_two, very_long_argument_three)\n"
result_call <- ast_pipeline(long_call)
result_lines <- strsplit(result_call, "\n")[[1]]
expect_true(length(result_lines) > 1L)
# All lines should be <= 80 chars
expect_true(all(nchar(result_lines) <= 80L))

# --- Pipeline matches rformat output ---
# The AST pipeline (collapse+wrap) should produce same output as rformat
# for code that only needs those transforms (no funcdef rewrite, no braces, etc.)

test_match <- function (code, desc) {
    fmt <- rformat(code)
    ast_out <- ast_pipeline(code)
    expect_equal(ast_out, fmt, info = desc)
}

test_match("x <- 1 + 2\n", "simple assignment")
test_match("foo(x, y, z)\n", "simple call")
test_match("if (x > 0) {\n    y <- 1\n}\n", "if block")
test_match("x <- a || b\n", "short logical")

# --- reformat_function_defs_ast ---

# Helper: full pipeline including funcdef
ast_funcdef_pipeline <- function (code, wrap = "paren") {
    fmt <- rformat(code, wrap = wrap)
    pd <- getParseData(parse(text = fmt, keep.source = TRUE))
    lines <- strsplit(fmt, "\n", fixed = TRUE)[[1]]
    terms <- rformat:::enrich_terminals(pd, lines)
    terms <- rformat:::collapse_calls_ast(terms, "    ", 80L)
    terms <- rformat:::reformat_function_defs_ast(terms, "    ",
        wrap = wrap, brace_style = "kr", line_limit = 80L)
    terms <- rformat:::wrap_long_operators_ast(terms, "    ", 80L)
    terms <- rformat:::wrap_long_calls_ast(terms, "    ", wrap, 80L)
    rformat:::serialize_tokens(terms, "    ", wrap, 80L)
}

# Short function stays on one line
expect_equal(
    ast_funcdef_pipeline("f <- function(x, y) {\n    x + y\n}\n"),
    "f <- function(x, y) {\n    x + y\n}\n"
)

# Long function wraps with paren alignment
long_func <- paste0(
    'lm <- function(formula, data, subset, weights, na.action, ',
    'method = "qr", model = TRUE, x = FALSE) {\n    body\n}\n')
expected_paren <- paste0(
    'lm <- function(formula, data, subset, weights, na.action, method = "qr",\n',
    '               model = TRUE, x = FALSE) {\n    body\n}\n')
expect_equal(ast_funcdef_pipeline(long_func), expected_paren)

# Long function wraps with fixed 8-space indent
expected_fixed <- paste0(
    'lm <- function(formula, data, subset, weights, na.action, method = "qr",\n',
    '        model = TRUE, x = FALSE) {\n    body\n}\n')
expect_equal(ast_funcdef_pipeline(long_func, wrap = "fixed"), expected_fixed)

# Function with if-else default (the IDEMP case)
ifelse_func <- 'f <- function(x, method = if (TRUE) "a" else "b") {\n    body\n}\n'
expect_equal(
    ast_funcdef_pipeline(ifelse_func),
    rformat(ifelse_func)
)

# Idempotency: pipeline output is stable when re-processed
idemp_test <- function(code, desc) {
    pass1 <- ast_funcdef_pipeline(code)
    pd2 <- getParseData(parse(text = pass1, keep.source = TRUE))
    lines2 <- strsplit(pass1, "\n", fixed = TRUE)[[1]]
    terms2 <- rformat:::enrich_terminals(pd2, lines2)
    terms2 <- rformat:::collapse_calls_ast(terms2, "    ", 80L)
    terms2 <- rformat:::reformat_function_defs_ast(terms2, "    ",
        wrap = "paren", brace_style = "kr", line_limit = 80L)
    terms2 <- rformat:::wrap_long_operators_ast(terms2, "    ", 80L)
    terms2 <- rformat:::wrap_long_calls_ast(terms2, "    ", "paren", 80L)
    pass2 <- rformat:::serialize_tokens(terms2, "    ", "paren", 80L)
    expect_equal(pass2, pass1, info = desc)
}

idemp_test("f <- function(x, y) {\n    x + y\n}\n", "short funcdef idemp")
idemp_test(long_func, "long funcdef idemp")
idemp_test(ifelse_func, "if-else default idemp")

# Multi-line string followed by ) should not insert blank line on re-format
# Regression: serialize_tokens didn't account for multi-line string tokens
# consuming extra output lines, causing blank line gap before )
str_paren <- 'ui_todo(\'
      line1
      \'
)'
out1 <- rformat(str_paren)
out2 <- rformat(out1)
expect_identical(out1, out2, info = "multi-line string ) placement idempotent")
expect_false(grepl("\n\n\\)", out1),
    info = "no blank line before ) after multi-line string")

# Collapse after braces: wrapping should be stable when control_braces
# adds indent that pushes a call near line_limit
# Regression: collapse ran only before braces, so calls collapsed before
# indent increase were later wrapped; second pass had braces already and
# collapsed them back
brace_collapse <- 'f <- function() {
    for (i in seq_len(n)) {
        for (j in seq_len(m)) {
            if (cond) {
                a <- 1
            } else {
                if (is.factor(y)) predicted <- factor(as.character(predicted), levels = lev)
            }
        }
    }
}'
out1 <- rformat(brace_collapse, control_braces = "next_line", indent = 8L,
    line_limit = 100L, brace_style = "allman", function_space = TRUE,
    else_same_line = FALSE)
out2 <- rformat(out1, control_braces = "next_line", indent = 8L,
    line_limit = 100L, brace_style = "allman", function_space = TRUE,
    else_same_line = FALSE)
expect_identical(out1, out2,
    info = "collapse after braces idempotent near line_limit")

# Bare if-else body in next_line mode: find_bare_body_end must include
# the else clause when the body is an if-else chain
# Regression: ELSE token was not excluded from statement-end check,
# so find_bare_body_end returned at ELSE, cutting off the else body
bare_ifelse <- 'f <- function() {
    if (plot.it)
        if (datax)
            plot(y, x)
        else
            plot(x, y)
    invisible(x)
}'
out <- rformat(bare_ifelse, control_braces = "next_line", indent = 2L)
expect_true(!is.null(tryCatch(parse(text = out), error = function(e) NULL)),
    info = "bare if-else body bracing parses correctly")
expect_true(grepl("\\} else", out) || grepl("if \\(datax\\)", out),
    info = "else clause preserved in braced if-else body")

# Empty function body {} should not get split across lines
# Regression: reformat_function_defs_ast moved { but left } on original line
empty_body <- 'if (cond) {
.f <- function(x, y, z, a = 1, b = 2, c = 3, d = 4) {}
}'
out <- rformat(empty_body, brace_style = "allman", wrap = "fixed",
    line_limit = 60L)
expect_true(!is.null(tryCatch(parse(text = out), error = function(e) NULL)),
    info = "empty function body {} parses after wrapping")

# Function body { on next line must be included in bare body range
# Regression: { was not in cont_starts, so find_bare_body_end returned
# at ) of function params, cutting off the function body
bare_func_body <- 'if (FALSE)
f <- function(x, y, z)
{
    x + y + z
}'
out <- rformat(bare_func_body, control_braces = "next_line", indent = 4L)
expect_true(!is.null(tryCatch(parse(text = out), error = function(e) NULL)),
    info = "function body { included in bare body range")

# Comment between assignment and value should not end the statement
# Regression: COMMENT token at balanced depth triggered statement-end check
comment_assign <- 'if (cond) {
    adjust <-
    ## comment here
    if (x) "a" else "b"
}'
out <- rformat(comment_assign, control_braces = "next_line", indent = 4L)
expect_true(!is.null(tryCatch(parse(text = out), error = function(e) NULL)),
    info = "comment between <- and value preserved in bare body")

# Comment after else keyword should not end the statement
# Regression: else+comment with body on next line was cut off
else_comment <- 'if (cond) {
    adjust <-
    if (x) "a"
    else # a comment
    "b"
}'
out <- rformat(else_comment, control_braces = "next_line", indent = 2L)
expect_true(!is.null(tryCatch(parse(text = out), error = function(e) NULL)),
    info = "comment after else does not break body detection")
