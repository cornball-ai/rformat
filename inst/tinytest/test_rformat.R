# Test rformat package

# Basic spacing
expect_equal(
  rformat("x<-1"),
  "x <- 1\n"
)

expect_equal(
  rformat("y=2"),
  "y = 2\n"
)

# Operator spacing
expect_equal(
  rformat("x+y"),
  "x + y\n"
)

# Function call spacing
expect_equal(
  rformat("foo(x,y,z)"),
  "foo(x, y, z)\n"
)

# Control flow spacing
expect_equal(
  rformat("if(x>0){y}"),
  "if (x > 0) { y }\n"
)

# String preservation - hyphen should NOT be spaced inside strings
expect_equal(
  rformat("x <- \"hello-world\""),
  "x <- \"hello-world\"\n"
)

# Error on non-character input
expect_error(
  rformat(123),
  "must be a character"
)

# Inline function body preservation (regression test)
# Functions without braces should preserve their body
result <- rformat("`%||%` <- function(x, y) if (is.null(x)) y else x")
expect_true(
  grepl("if \\(is\\.null\\(x\\)\\) y else x", result),
  info = "Inline function body should be preserved"
)

# Nested parentheses indentation (regression test)
# Each level of parens should add one level of indent
code <- "tryCatch(
  foo(
    x,
    y
  )
)"
result <- rformat(code)
# x should be indented 8 spaces (4 for tryCatch, 4 for foo)
expect_true(
  grepl("\n        x,", result),
  info = "Nested parens should increase indentation"
)
