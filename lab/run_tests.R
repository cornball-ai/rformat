library(rformat)
results <- tinytest::run_test_file("inst/tinytest/test_rformat.R")
for (r in results) {
    if (!isTRUE(r)) print(r)
}
