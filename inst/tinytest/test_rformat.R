# Test rformat package

# Basic spacing
expect_equal(
  rformat("x<-1"),
  "x <- 1\n"
)

# = assignment gets converted to <-
expect_equal(
  rformat("y=2"),
  "y <- 2\n"
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

# Long multi-line calls get collapsed then re-wrapped
long_args <- paste(paste0("very_long_name_", 1:5), collapse = ",\n  ")
code <- paste0("c(\n  ", long_args, "\n)")
result <- rformat(code)
expect_true(
  grepl("\n", sub("\n$", "", result)),
  info = "Long calls should be wrapped across lines"
)
# Should be compactly wrapped, not one-arg-per-line
result_lines <- strsplit(sub("\n$", "", result), "\n")[[1]]
expect_true(
  length(result_lines) < 5,
  info = "Long calls should wrap compactly, not one-arg-per-line"
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

# Wrap long function calls at commas with paren alignment
expect_equal(
  rformat("x <- c(very_long_a, very_long_b, very_long_c, very_long_d, very_long_e, very_long_f)"),
  "x <- c(very_long_a, very_long_b, very_long_c, very_long_d, very_long_e,\n       very_long_f)\n"
)

# Wrap long named-arg calls
expect_equal(
  rformat("result <- list(alpha = 1, beta = 2, gamma = 3, delta = 4, epsilon = 5, zeta = 6, eta = 7)"),
  "result <- list(alpha = 1, beta = 2, gamma = 3, delta = 4, epsilon = 5,\n               zeta = 6, eta = 7)\n"
)

# Short calls stay on one line
expect_equal(
  rformat("x <- c(1, 2, 3)"),
  "x <- c(1, 2, 3)\n"
)

# = to <- conversion (named args stay as =)
expect_equal(
  rformat("x = 1"),
  "x <- 1\n"
)
expect_equal(
  rformat("foo(x = 1)"),
  "foo(x = 1)\n"
)

# Braces on one-liner if
expect_equal(
  rformat("if (x) y"),
  "if (x) { y }\n"
)

# Braces on one-liner if-else
expect_equal(
  rformat("if (x > 0) y else z"),
  "if (x > 0) { y } else { z }\n"
)

# Braces on one-liner for
expect_equal(
  rformat("for (i in 1:10) print(i)"),
  "for (i in 1:10) { print(i) }\n"
)

# Preserve if-else as expression (RHS of assignment)
expect_equal(
  rformat("x <- if (a) b else c"),
  "x <- if (a) b else c\n"
)

# } else { on same line
expect_equal(
  rformat("if (x) {\n  y\n}\nelse {\n  z\n}"),
  "if (x) {\n    y\n} else {\n    z\n}\n"
)

# Trailing whitespace removal
expect_false(
  grepl(" \n", rformat("x <- 1   \ny <- 2  ")),
  info = "Trailing whitespace should be removed"
)

# Collapse multi-line if condition, wrap at || (issue #8)
code <- "f <- function(fn, path, vn) {
    if (
        fs::file_exists(fn) ||
        fs::file_exists(fs::path_join(c(path, \"_quarto\", vn)))
    ) {
        TRUE
    }
}"
result <- rformat(code)
# Condition should be on 2 lines max, wrapped at ||
cond_lines <- grep("file_exists", strsplit(result, "\n")[[1]])
expect_equal(
  length(cond_lines),
  2L,
  info = "Multi-line if condition should collapse and wrap at ||"
)

# Multi-line condition with bare body (regression: add_control_braces corruption)
code <- "if (!is.numeric(breaks) || !is.finite(breaks) || breaks < 1L) stop(\"invalid\")"
result <- rformat(code)
expect_true(
  grepl("is.numeric\\(breaks\\)", result),
  info = "Multi-line condition should not lose content when adding braces"
)
expect_true(
  grepl("\\{", result) && grepl("stop\\(", result),
  info = "Bare body after multi-line condition should get braces"
)

# Short if condition collapses to one line
expect_true(
  grepl("if \\(x > 0\\) \\{", rformat("if (\n  x > 0\n) {\n  y\n}")),
  info = "Short if condition should collapse to one line"
)

# Brace body inside open paren: Map(function() { ... })
code <- "Map(function(x) {\n    x + 1\n}, xs)"
result <- rformat(code)
expect_true(
  grepl("\n    x \\+ 1\n", result),
  info = "Map(function() { body }) should indent body 1 level, not 2"
)

# Brace body inside open paren: tryCatch({ ... })
code <- "tryCatch({\n    x + 1\n}, error = function(e) NULL)"
result <- rformat(code)
expect_true(
  grepl("\n    x \\+ 1\n", result),
  info = "tryCatch({ body }) should indent body 1 level, not 2"
)

# Nested: brace-inside-paren with multiple body lines
code <- "lapply(x, function(i) {\n    a <- 1\n    b <- 2\n})"
result <- rformat(code)
expect_true(
  grepl("\n    a <- 1\n    b <- 2\n", result),
  info = "Multiple body lines inside brace-in-paren should indent 1 level"
)

# Bare if body with trailing comment expands to multi-line (regression)
code <- "if (x) y # comment"
result <- rformat(code)
expect_true(
  grepl("\\{\n", result),
  info = "Bare body with trailing comment should expand to multi-line braces"
)
expect_true(
  !is.null(tryCatch(parse(text = result), error = function(e) NULL)),
  info = "Bare body with trailing comment should produce valid R code"
)

# if-else expression inside function call stays unbraced (regression)
code <- "foo(if (x) 1 else 2, y)"
result <- rformat(code)
expect_true(
  grepl("if \\(x\\) 1 else 2", result),
  info = "if-else expression inside function call should not get braces"
)

# Double bracket [[ ]] preserved in bare if body (regression)
code <- "if (is.null(x)) x <- lst[[\"key\"]]"
result <- rformat(code)
expect_true(
  grepl('\\[\\["key"\\]\\]', result),
  info = "Double brackets should be preserved when adding control braces"
)
