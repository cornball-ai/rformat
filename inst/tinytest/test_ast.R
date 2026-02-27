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
