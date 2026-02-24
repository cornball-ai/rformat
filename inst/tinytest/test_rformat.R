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

# Long multi-line strings should not break later collapse/wrap passes
# (regression: getParseData STR_CONST truncation + pass-2 call indent drift)
css_lines <- rep("  .main-sidebar, .left-side { width: _WIDTH_; }", 40)
css_text <- paste(css_lines, collapse = "\n")
code <- paste0(
  "f <- function(width, collapsed) {\n",
  "    custom_css <- tags$head(tags$style(HTML(gsub(\n",
  "        \"_WIDTH_\",\n",
  "        width,\n",
  "        fixed = TRUE,\n",
  "        '\n", css_text, "\n'\n",
  "    ))))\n",
  "    dataValue <- shiny::restoreInput(id = \"sidebarCollapsed\", default = collapsed)\n",
  "    dataValue\n",
  "}\n"
)
fmt1 <- rformat(code)
fmt2 <- rformat(fmt1)
expect_equal(
  fmt2,
  fmt1,
  info = "Pass 2 should preserve wrapped call indentation even with long STR_CONST elsewhere"
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

# Collapse short calls with inline function definitions
code <- "lapply(x,\n  function(i) i + 1\n)"
expect_equal(rformat(code), "lapply(x, function(i) i + 1)\n")

# Wrap long function calls at commas with paren-aligned continuation
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

# Inline if-else expansion doesn't absorb subsequent statements (regression)
code <- "f <- function() {
    xlevels <- if (is.numeric(xlevels)) levels(datapoints$x)[xlevels] else xlevels
    next_statement <- 3
}"
result <- rformat(code, expand_if = TRUE)
# next_statement should NOT be inside the else block
expect_true(
  grepl("    next_statement <- 3", result),
  info = "Statements after inline if-else expansion should keep correct indent"
)
# The else block should close before the next statement
expect_true(
  grepl("\\}\n    next_statement", result),
  info = "Inline if-else expansion should close else block before next statement"
)

# Double bracket [[ ]] preserved in bare if body (regression)
code <- "if (is.null(x)) x <- lst[[\"key\"]]"
result <- rformat(code)
expect_true(
  grepl('\\[\\["key"\\]\\]', result),
  info = "Double brackets should be preserved when adding control braces"
)

# No space before [[ (LBB token) (regression: dogfooding bug 1)
expect_equal(
  rformat("x <- strsplit(s, \",\")[[1]]"),
  "x <- strsplit(s, \",\")[[1]]\n",
  info = "No space before [[ after )"
)
expect_equal(
  rformat("y <- lst[[1]][[2]]"),
  "y <- lst[[1]][[2]]\n",
  info = "No space before chained [["
)

# No space around : operator (regression: dogfooding bug 2)
expect_equal(
  rformat("x <- (a + 1):b"),
  "x <- (a + 1):b\n",
  info = "No space before : after )"
)
expect_equal(
  rformat("x <- 1:10"),
  "x <- 1:10\n",
  info = "No space around : in sequence"
)

# Trailing comment keeps space (regression: dogfooding bug 4)
expect_true(
  grepl(" # comment", rformat("x <- 1 # comment")),
  info = "Space preserved before trailing comment"
)

# Unary minus not spaced (regression: dogfooding bug 5)
expect_equal(
  rformat("x[-length(x)]"),
  "x[-length(x)]\n",
  info = "Unary minus after [ not spaced"
)
expect_equal(
  rformat("x <- -1"),
  "x <- -1\n",
  info = "Unary minus after <- not spaced"
)
expect_equal(
  rformat("f(-x, -y)"),
  "f(-x, -y)\n",
  info = "Unary minus after ( and , not spaced"
)
# Binary minus still spaced
expect_equal(
  rformat("x-y"),
  "x - y\n",
  info = "Binary minus still gets spaces"
)

# c(if-else) not expanded (regression: dogfooding bug 3)
code <- "x <- c(if (a) b else d, e)"
result <- rformat(code, expand_if = TRUE)
expect_true(
  grepl("c\\(if", result),
  info = "if-else inside c() should not be expanded"
)

# Don't collapse calls containing braces (regression: brace body collapse)
code <- "tryCatch({\n    x + 1\n    y + 2\n}, error = function(e) NULL)"
result <- rformat(code)
expect_true(
  !is.null(tryCatch(parse(text = result), error = function(e) NULL)),
  info = "Calls containing braces should not be collapsed into one line"
)
expect_true(
  grepl("\n", sub("\n$", "", result)),
  info = "Calls containing braces should stay multi-line"
)

# Operator wrap uses depth-based continuation indent (idempotent)
code <- "terminals[some_long_condition & another_long_condition & yet_another_condition & one_more_thing,]"
result <- rformat(code)
result_lines <- strsplit(sub("\n$", "", result), "\n")[[1]]
if (length(result_lines) > 1) {
  # Continuation should be one indent level deeper (inside [)
  expect_true(
    grepl("^    ", result_lines[2]),
    info = "Operator wrap continuation should be indented one level inside ["
  )
}

# Chained if-else expression preserved (regression: torchlang discover.R)
code <- "status <- if (a) \"x\" else if (b) \"y\" else \"z\""
result <- rformat(code)
expect_true(
  grepl("status <- if", result),
  info = "Chained if-else expression should not be expanded to control flow"
)

# Comments inside function formals skipped (regression: torch nnf-activation.R)
code <- "f <- function(batch_first = FALSE, # type: bool\n              x = 1) {\n    x\n}"
result <- rformat(code)
expect_true(
  !is.null(tryCatch(parse(text = result), error = function(e) NULL)),
  info = "Comments inside function formals should not corrupt signature"
)

# Nested call wrapping doesn't corrupt multiline strings (regression: Rcpp)
code <- "sprintf(\"hello\\n%s\\n\\t\\t}\", paste(sprintf(\"item %s\", names(x), names(x)), collapse = \"\\n\"))"
result <- rformat(code)
expect_true(
  !is.null(tryCatch(parse(text = result), error = function(e) NULL)),
  info = "Nested calls with multiline strings should not corrupt code"
)

# Trailing comment between if-condition and body (regression: Rcpp)
# Note: else on new line only parses inside braces
code <- "{\nif (!x) # eval now\n    stop(\"error\")\nelse {\n    y\n}\n}"
result <- rformat(code)
expect_true(
  !is.null(tryCatch(parse(text = result), error = function(e) NULL)),
  info = "Comment between if-condition and body should produce valid code"
)
expect_true(
  grepl("# eval now", result),
  info = "Comment between if-condition and body should be preserved"
)

# --- Idempotency tests ---
# Formatting the same code twice should produce identical output

# Simple assignment + operator wrap
code <- "f <- function() {\n    deemphasise <- deemphasise || !is_string(data$node_type[[num_root]], \"main\")\n}"
r1 <- rformat(code)
r2 <- rformat(r1)
expect_equal(r1, r2, info = "Operator wrap should be idempotent")

# Function call wrap
code <- "x <- c(very_long_a, very_long_b, very_long_c, very_long_d, very_long_e, very_long_f)"
r1 <- rformat(code)
r2 <- rformat(r1)
expect_equal(r1, r2, info = "Function call wrap should be idempotent")

# Operator inside brackets
code <- "terminals[some_long_condition & another_long_condition & yet_another_condition & one_more_thing,]"
r1 <- rformat(code)
r2 <- rformat(r1)
expect_equal(r1, r2, info = "Operator wrap inside brackets should be idempotent")

# Bare if with long condition + body
code <- "if (inherits(module, \"DLLInfo\") && missing(mustStart)) mustStart <- TRUE\t\t\t\t\t\t# nocov"
r1 <- rformat(code)
r2 <- rformat(r1)
expect_equal(r1, r2, info = "Brace addition + operator wrap should be idempotent")

# Default: no space before ( in function definitions
expect_true(
  grepl("function\\(x\\)", rformat("sapply(xs, function(x) x + 1)")),
  info = "Default: no space before ( in function definitions"
)

# function_space = TRUE: space before ( in function definitions
expect_true(
  grepl("function \\(x\\)", rformat("sapply(xs, function(x) x + 1)", function_space = TRUE)),
  info = "function_space = TRUE adds space before ("
)

# --- Bug A: Long string truncation ---
# STR_CONST tokens >1000 chars get truncated by getParseData();
# rformat should recover original text from source lines
long_str <- paste0('"', paste(rep("x", 1200), collapse = ""), '"')
code <- paste0("x <- ", long_str)
result <- rformat(code)
expect_true(
  !is.null(tryCatch(parse(text = result), error = function(e) NULL)),
  info = "Long string constants should parse after formatting"
)
expect_true(
  grepl(substring(long_str, 1, 50), result, fixed = TRUE),
  info = "Long string content should be preserved, not truncated"
)

# --- Bug B: ELSE search crosses brace boundaries ---
# Inner `x <- if (cond) value` inside `if (outer) { ... } else { ... }`
# should NOT find the outer else
code <- "if (outer) {\n    space <- if (cond) padding\n    use(space)\n} else {\n    other()\n}"
result <- rformat(code)
expect_true(
  !is.null(tryCatch(parse(text = result), error = function(e) NULL)),
  info = "ELSE search should not cross brace boundaries"
)
expect_true(
  grepl("use\\(space\\)", result),
  info = "Statement after inner if should be preserved"
)

# --- Bug C: Braced false body not collapsed ---
# x <- if (cond) a else { stmt1; stmt2 } should not be expanded
code <- "x <- if (cond) a else {\n    b <- 1\n    b + 1\n}"
result <- rformat(code)
expect_true(
  !is.null(tryCatch(parse(text = result), error = function(e) NULL)),
  info = "Braced false body in if-else expression should parse"
)
expect_true(
  grepl("else \\{", result),
  info = "Braced false body should be preserved, not inlined"
)

# --- Bug D: nested if-assignment with outer else stays idempotent ---
code <- "if (is.logical(labels))
    labels <- if (labels) format(at, trim = TRUE) else NULL
else if (at.missing)
    warning(\"specifying 'labels' but not 'at' may lead to unexpected results\")"
r1 <- rformat(code)
r2 <- rformat(r1)
expect_equal(
  r1, r2,
  info = "Nested if-assignment with outer else should be idempotent"
)

# --- Bug E: expanded inline-if keeps formatted LHS ---
code <- "cmap[ii+k, j] <- if(tmap[i+1,j]==0) 0L else tmap[i+1,j]*mult +k"
r1 <- rformat(code, expand_if = TRUE)
r2 <- rformat(r1, expand_if = TRUE)
expect_equal(
  r1, r2,
  info = "Inline-if expansion should keep LHS spacing/idempotence"
)
expect_true(
  grepl("cmap\\[ii \\+ k, j\\]", r1),
  info = "Expanded inline-if should format subscript spacing in LHS"
)

# --- Bug F: inline anonymous function argument stays idempotent ---
code <- "lines.Lines <- function (x, y = NULL, ...) invisible(lapply(x@Lines, function (x, ...) lines(x, ...), ...))"
r1 <- rformat(code)
r2 <- rformat(r1)
expect_equal(
  r1, r2,
  info = "Inline anonymous function arguments should stay idempotent"
)

# --- Bare if-else in function call expansion ---
# Long call with bare if-else arg should expand to braced form
code <- "new_code_lines <- c(pre_lines, new_lines_vec, if (end < length(lines)) lines[seq(end + 1, length(lines))] else character(0))"
r1 <- rformat(code)
expect_true(
  grepl("\\} else \\{", r1),
  info = "Long bare if-else in call should expand to braced form"
)
expect_true(
  !is.null(tryCatch(parse(text = r1), error = function(e) NULL)),
  info = "Expanded bare if-else in call should produce valid R code"
)
r2 <- rformat(r1)
expect_equal(r1, r2, info = "Expanded bare if-else in call should be idempotent")

# --- Paren-aligned call continuation is idempotent ---
code <- 'f <- function () {\n    dataValue <- shiny::restoreInput(id = "sidebarCollapsed", default = collapsed)\n}'
r1 <- rformat(code)
r2 <- rformat(r1)
expect_equal(r1, r2,
             info = "Paren-aligned call continuation should be idempotent")
expect_true(
    grepl("restoreInput\\(id = \"sidebarCollapsed\",\n\\s+default",
          r1),
    info = "Long call should be wrapped at comma"
)

# Short if-else in call stays inline
code <- "x <- c(if (a) b else d, e)"
r1 <- rformat(code)
expect_true(
  grepl("c\\(if \\(a\\) b else d", r1),
  info = "Short bare if-else in call should stay inline"
)
