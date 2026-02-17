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

# Nested parentheses collapse (regression test)
# Short nested calls should collapse to a single line
code <- "tryCatch(
  foo(
    x,
    y
  )
)"
result <- rformat(code)
expect_equal(
  result,
  "tryCatch(foo(x, y))\n",
  info = "Nested short calls should collapse to one line"
)

# Multi-line string preservation (regression test)
# Multi-line strings should not have their content duplicated
code <- 'DBI::dbExecute(con, "
    CREATE TABLE foo (
        id INTEGER
    )
")'
result <- rformat(code)
# Count occurrences of CREATE TABLE - should be exactly 1
matches <- gregexpr("CREATE TABLE", result)[[1]]
expect_equal(
  length(matches[matches > 0]),
  1L,
  info = "Multi-line strings should not be duplicated"
)

# Multi-line string in function (regression test)
code <- 'f <- function(x) {
    sql <- "
        SELECT *
        FROM table
    "
    sql
}'
result <- rformat(code)
# Should have exactly one SELECT
matches <- gregexpr("SELECT", result)[[1]]
expect_equal(
  length(matches[matches > 0]),
  1L,
  info = "Multi-line strings in functions should not be duplicated"
)

# Collapse multi-line c() to single line
expect_equal(
  rformat("c(x,\n  y,\n  z\n)"),
  "c(x, y, z)\n"
)

# Collapse multi-line list() with named args
expect_equal(
  rformat("list(a = 1,\n  b = 2,\n  c = 3\n)"),
  "list(a = 1, b = 2, c = 3)\n"
)

# Don't collapse when result would exceed line limit
long_args <- paste(paste0("very_long_name_", 1:5), collapse = ",\n  ")
code <- paste0("c(\n  ", long_args, "\n)")
result <- rformat(code)
expect_true(
  grepl("\n", sub("\n$", "", result)),
  info = "Long calls should stay multi-line"
)

# Don't collapse calls containing comments
code <- "c(x,\n  # keep this comment\n  y\n)"
result <- rformat(code)
expect_true(
  grepl("\n", sub("\n$", "", result)),
  info = "Calls with comments should stay multi-line"
)

# Don't collapse calls containing function definitions
code <- "lapply(x,\n  function(i) i + 1\n)"
result <- rformat(code)
expect_true(
  grepl("\n", sub("\n$", "", result)),
  info = "Calls with function defs should stay multi-line"
)
