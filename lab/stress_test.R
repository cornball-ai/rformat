#!/usr/bin/env r
#
# Stress test rformat against top CRAN packages
#
# Downloads source tarballs, extracts, formats R/ directory,
# verifies all formatted files still parse. Records failures.
#
# Usage:
#   r lab/stress_test.R                    # run all 100
#   N=10 r lab/stress_test.R               # run first 10
#   PKGS="dplyr rlang" r lab/stress_test.R # run specific packages

library(rformat)

packages <- c(
    # Core / Infrastructure
    "Rcpp", "rlang", "vctrs", "glue", "cli",
    "withr", "lifecycle", "magrittr", "pillar", "crayon",
    # Data Manipulation
    "dplyr", "tidyr", "tibble", "readr", "stringr",
    "forcats", "lubridate", "purrr", "data.table", "janitor",
    # Visualization
    "ggplot2", "scales", "patchwork", "cowplot", "plotly",
    "lattice", "leaflet", "ggridges", "viridis", "DT",
    # Shiny / Web
    "shiny", "shinydashboard", "bslib", "htmltools", "httpuv",
    "httr", "httr2", "plumber", "rvest", "xml2",
    # Modeling / Stats
    "caret", "recipes", "parsnip", "workflows", "yardstick",
    "glmnet", "lme4", "survival", "brms", "xgboost",
    # Devtools / Packaging
    "testthat", "tinytest", "devtools", "usethis", "roxygen2",
    "pkgdown", "covr", "remotes", "pak", "renv",
    # Data Import / Storage
    "readxl", "writexl", "DBI", "RSQLite", "duckdb",
    "arrow", "sparklyr", "pins", "qs", "fst",
    # Time Series / Specialized
    "forecast", "zoo", "xts", "tsibble", "fable",
    "igraph", "sf", "terra", "sp", "rmarkdown",
    # String / Parsing / Lang
    "stringi", "jsonlite", "yaml", "digest", "R6",
    "xmlparsedata", "evaluate", "callr", "processx", "here",
    # Performance / Parallel
    "future", "furrr", "parallelly", "RcppParallel", "bench",
    "profvis", "memoise", "progress", "curl", "openssl"
)

# Parse env vars for subsetting
n_env <- Sys.getenv("N", "")
pkgs_env <- Sys.getenv("PKGS", "")

if (nzchar(pkgs_env)) {
    packages <- strsplit(trimws(pkgs_env), "\\s+")[[1]]
} else if (nzchar(n_env)) {
    packages <- head(packages, as.integer(n_env))
}

# Per-file timeout (seconds) — skip files that take too long
file_timeout <- as.integer(Sys.getenv("TIMEOUT", "10"))

cat(sprintf("Stress testing rformat against %d CRAN packages\n", length(packages)))
cat(sprintf("Per-file timeout: %ds\n\n", file_timeout))

# Setup temp directory
work_dir <- file.path(tempdir(), "rformat_stress")
dir.create(work_dir, showWarnings = FALSE, recursive = TRUE)

# Track results
results <- data.frame(
    package = character(),
    r_files = integer(),
    parsed_before = integer(),
    formatted_ok = integer(),
    parse_failures = integer(),
    not_idempotent = integer(),
    timeouts = integer(),
    status = character(),
    errors = character(),
    stringsAsFactors = FALSE
)

for (pkg in packages) {
    cat(sprintf("[%d/%d] %s ... ", match(pkg, packages), length(packages), pkg))

    row <- list(
        package = pkg, r_files = 0L, parsed_before = 0L,
        formatted_ok = 0L, parse_failures = 0L, not_idempotent = 0L,
        timeouts = 0L,
        status = "OK", errors = ""
    )

    # Download source tarball
    pkg_dir <- file.path(work_dir, pkg)
    if (dir.exists(pkg_dir)) unlink(pkg_dir, recursive = TRUE)

    dl <- tryCatch(
        download.packages(pkg, destdir = work_dir, type = "source",
                          repos = "https://cloud.r-project.org",
                          quiet = TRUE),
        error = function(e) NULL
    )

    if (is.null(dl) || nrow(dl) == 0) {
        cat("SKIP (download failed)\n")
        row$status <- "SKIP"
        row$errors <- "download failed"
        results <- rbind(results, as.data.frame(row, stringsAsFactors = FALSE))
        next
    }

    tarball <- dl[1, 2]

    # Extract
    untar(tarball, exdir = work_dir)
    unlink(tarball)

    # Find R/ directory
    r_dir <- file.path(pkg_dir, "R")
    if (!dir.exists(r_dir)) {
        cat("SKIP (no R/ directory)\n")
        row$status <- "SKIP"
        row$errors <- "no R/ directory"
        results <- rbind(results, as.data.frame(row, stringsAsFactors = FALSE))
        unlink(pkg_dir, recursive = TRUE)
        next
    }

    r_files <- list.files(r_dir, pattern = "\\.[Rr]$", full.names = TRUE,
                          recursive = TRUE)
    row$r_files <- length(r_files)

    if (length(r_files) == 0) {
        cat("SKIP (no .R files)\n")
        row$status <- "SKIP"
        row$errors <- "no .R files"
        results <- rbind(results, as.data.frame(row, stringsAsFactors = FALSE))
        unlink(pkg_dir, recursive = TRUE)
        next
    }

    # Check which files parse before formatting
    parseable <- vapply(r_files, function(f) {
        code <- paste(readLines(f, warn = FALSE), collapse = "\n")
        !is.null(tryCatch(parse(text = code, keep.source = TRUE),
                           error = function(e) NULL))
    }, logical(1))

    row$parsed_before <- sum(parseable)
    r_files <- r_files[parseable]

    if (length(r_files) == 0) {
        cat("SKIP (no parseable files)\n")
        row$status <- "SKIP"
        row$errors <- "no parseable files"
        results <- rbind(results, as.data.frame(row, stringsAsFactors = FALSE))
        unlink(pkg_dir, recursive = TRUE)
        next
    }

    # Format each file and check parsing + idempotence
    failures <- character()
    not_idempotent_files <- character()
    timed_out <- character()

    for (f in r_files) {
        original <- paste(readLines(f, warn = FALSE), collapse = "\n")

        # Format with timeout
        formatted <- tryCatch({
            setTimeLimit(elapsed = file_timeout)
            on.exit(setTimeLimit(elapsed = Inf), add = TRUE)
            rformat(original)
        },
        error = function(e) {
            if (grepl("time limit|elapsed", e$message, ignore.case = TRUE)) {
                structure("TIMEOUT", class = "timeout_marker")
            } else {
                NULL
            }
        },
        warning = function(w) {
            suppressWarnings(rformat(original))
        })

        if (inherits(formatted, "timeout_marker")) {
            timed_out <- c(timed_out, basename(f))
            next
        }

        if (is.null(formatted)) {
            failures <- c(failures, basename(f))
            next
        }

        # Verify formatted code parses
        parsed <- tryCatch(
            parse(text = formatted, keep.source = TRUE),
            error = function(e) e
        )

        if (inherits(parsed, "error")) {
            failures <- c(failures, basename(f))
        } else {
            row$formatted_ok <- row$formatted_ok + 1L

            # Idempotence check: fmt(fmt(x)) == fmt(x)
            formatted2 <- tryCatch(
                suppressWarnings(rformat(formatted)),
                error = function(e) NULL
            )
            if (!is.null(formatted2) && formatted2 != formatted) {
                not_idempotent_files <- c(not_idempotent_files, basename(f))
            }
        }
    }

    row$parse_failures <- length(failures)
    row$not_idempotent <- length(not_idempotent_files)
    row$timeouts <- length(timed_out)

    if (length(failures) > 0) {
        row$status <- "FAIL"
        row$errors <- paste(failures, collapse = ", ")
        cat(sprintf("FAIL (%d/%d broken: %s)\n",
                    length(failures), length(r_files),
                    paste(head(failures, 3), collapse = ", ")))
    } else if (length(not_idempotent_files) > 0) {
        row$status <- "IDEMP"
        row$errors <- paste(not_idempotent_files, collapse = ", ")
        cat(sprintf("IDEMP (%d files, %d not idempotent: %s)\n",
                    row$formatted_ok, length(not_idempotent_files),
                    paste(head(not_idempotent_files, 3), collapse = ", ")))
    } else if (length(timed_out) > 0) {
        row$status <- "OK*"
        row$errors <- paste("timeout:", paste(timed_out, collapse = ", "))
        cat(sprintf("OK* (%d files, %d timed out)\n",
                    row$formatted_ok, length(timed_out)))
    } else {
        cat(sprintf("OK (%d files)\n", row$formatted_ok))
    }

    results <- rbind(results, as.data.frame(row, stringsAsFactors = FALSE))
    unlink(pkg_dir, recursive = TRUE)
}

# Summary
cat("\n")
cat(strrep("=", 60), "\n")
cat("RESULTS\n")
cat(strrep("=", 60), "\n\n")

ok <- results[results$status %in% c("OK", "OK*"),]
fail <- results[results$status == "FAIL",]
idemp <- results[results$status == "IDEMP",]
skip <- results[results$status == "SKIP",]

cat(sprintf("  OK:   %d packages (%d files total)\n",
            nrow(ok), sum(ok$formatted_ok)))
cat(sprintf("  FAIL: %d packages (parse errors)\n", nrow(fail)))
cat(sprintf("  IDEMP: %d packages (not idempotent)\n", nrow(idemp)))
cat(sprintf("  SKIP: %d packages\n", nrow(skip)))

if (sum(results$timeouts) > 0) {
    cat(sprintf("  Timeouts: %d files across %d packages\n",
                sum(results$timeouts), sum(results$timeouts > 0)))
}
cat("\n")

if (nrow(fail) > 0) {
    cat("PARSE FAILURES:\n")
    for (i in seq_len(nrow(fail))) {
        cat(sprintf("  %s: %s\n", fail$package[i], fail$errors[i]))
    }
    cat("\n")
}

if (nrow(idemp) > 0) {
    cat("NOT IDEMPOTENT:\n")
    for (i in seq_len(nrow(idemp))) {
        cat(sprintf("  %s: %s\n", idemp$package[i], idemp$errors[i]))
    }
    cat("\n")
}

total_files <- sum(results$formatted_ok) + sum(results$parse_failures)
cat(sprintf("Total: %d/%d files formatted successfully (%.1f%%)\n",
            sum(results$formatted_ok), total_files,
            100 * sum(results$formatted_ok) / max(total_files, 1)))

# Save results
out_file <- file.path("lab", "stress_test_results.csv")
write.csv(results, out_file, row.names = FALSE)
cat(sprintf("\nDetailed results saved to %s\n", out_file))
