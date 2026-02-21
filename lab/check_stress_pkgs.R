#!/usr/bin/env r
#
# Check cached CRAN tarballs for parse + idempotency.
# Usage:
#   r lab/check_stress_pkgs.R
#   N=10 r lab/check_stress_pkgs.R
#   PKGS="dplyr rlang" r lab/check_stress_pkgs.R
#   CACHE_DIR=~/.cache/rformat_cran_src r lab/check_stress_pkgs.R
#   FAIL_DIR=lab/stress_failures r lab/check_stress_pkgs.R
#   OUT_FILE=lab/stress_test_results.csv r lab/check_stress_pkgs.R
#   TIMEOUT=10 r lab/check_stress_pkgs.R

library(rformat)
source("lab/stress_packages.R")

subset_packages <- function(pkgs) {
    n_env <- Sys.getenv("N", "")
    pkgs_env <- Sys.getenv("PKGS", "")
    if (nzchar(pkgs_env)) {
        return(strsplit(trimws(pkgs_env), "\\s+")[[1]])
    }
    if (nzchar(n_env)) {
        return(head(pkgs, as.integer(n_env)))
    }
    pkgs
}

packages <- subset_packages(stress_packages)

cache_dir <- Sys.getenv("CACHE_DIR", file.path(path.expand("~"), ".cache", "rformat_cran_src"))
fail_dir <- Sys.getenv("FAIL_DIR", file.path("lab", "stress_failures"))
out_file <- Sys.getenv("OUT_FILE", file.path("lab", "stress_test_results.csv"))
file_timeout <- as.integer(Sys.getenv("TIMEOUT", "10"))

dir.create(fail_dir, showWarnings = FALSE, recursive = TRUE)

cat(sprintf("Checking %d packages from %s\n", length(packages), cache_dir))
cat(sprintf("Per-file timeout: %ds\n\n", file_timeout))

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

    tarball <- file.path(cache_dir, paste0(pkg, ".tar.gz"))
    if (!file.exists(tarball)) {
        # Support versioned tarballs: pkg_1.2.3.tar.gz
        candidates <- list.files(cache_dir,
                                 pattern = paste0("^", pkg, "_.*\\.tar\\.gz$"),
                                 full.names = TRUE)
        if (length(candidates) > 0) {
            info <- file.info(candidates)
            tarball <- candidates[which.max(info$mtime)]
        }
    }
    if (!file.exists(tarball)) {
        cat("SKIP (missing tarball)\n")
        row$status <- "SKIP"
        row$errors <- "missing tarball"
        results <- rbind(results, as.data.frame(row, stringsAsFactors = FALSE))
        next
    }

    work_dir <- file.path(tempdir(), "rformat_stress", pkg)
    if (dir.exists(work_dir)) unlink(work_dir, recursive = TRUE)
    dir.create(work_dir, showWarnings = FALSE, recursive = TRUE)
    untar(tarball, exdir = work_dir)

    pkg_dir <- file.path(work_dir, pkg)
    r_dir <- file.path(pkg_dir, "R")
    if (!dir.exists(r_dir)) {
        cat("SKIP (no R/ directory)\n")
        row$status <- "SKIP"
        row$errors <- "no R/ directory"
        results <- rbind(results, as.data.frame(row, stringsAsFactors = FALSE))
        unlink(work_dir, recursive = TRUE)
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
        unlink(work_dir, recursive = TRUE)
        next
    }

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
        unlink(work_dir, recursive = TRUE)
        next
    }

    failures <- character()
    not_idempotent_files <- character()
    timed_out <- character()

    for (f in r_files) {
        original <- paste(readLines(f, warn = FALSE), collapse = "\n")

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

        parsed <- tryCatch(
            parse(text = formatted, keep.source = TRUE),
            error = function(e) e
        )

        if (inherits(parsed, "error")) {
            failures <- c(failures, basename(f))
            # Save artifacts for parse failures
            fail_base <- paste(pkg, basename(f), sep = "__")
            fail_subdir <- file.path(fail_dir, pkg)
            dir.create(fail_subdir, showWarnings = FALSE, recursive = TRUE)
            writeLines(original, file.path(fail_subdir, paste0(fail_base, ".orig.R")))
            writeLines(formatted, file.path(fail_subdir, paste0(fail_base, ".fmt1.R")))
            writeLines(conditionMessage(parsed),
                       file.path(fail_subdir, paste0(fail_base, ".parse_error.txt")))
        } else {
            row$formatted_ok <- row$formatted_ok + 1L

            formatted2 <- tryCatch(
                suppressWarnings(rformat(formatted)),
                error = function(e) NULL
            )
            if (!is.null(formatted2) && formatted2 != formatted) {
                not_idempotent_files <- c(not_idempotent_files, basename(f))
                # Save artifacts for idempotency failures
                fail_base <- paste(pkg, basename(f), sep = "__")
                fail_subdir <- file.path(fail_dir, pkg)
                dir.create(fail_subdir, showWarnings = FALSE, recursive = TRUE)
                writeLines(original, file.path(fail_subdir, paste0(fail_base, ".orig.R")))
                writeLines(formatted, file.path(fail_subdir, paste0(fail_base, ".fmt1.R")))
                writeLines(formatted2, file.path(fail_subdir, paste0(fail_base, ".fmt2.R")))
                diff_file <- file.path(fail_subdir, paste0(fail_base, ".diff"))
                cmd <- sprintf("diff -u %s %s > %s",
                               shQuote(file.path(fail_subdir, paste0(fail_base, ".fmt1.R"))),
                               shQuote(file.path(fail_subdir, paste0(fail_base, ".fmt2.R"))),
                               shQuote(diff_file))
                system(cmd)
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
    unlink(work_dir, recursive = TRUE)
}

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

write.csv(results, out_file, row.names = FALSE)
cat(sprintf("\nDetailed results saved to %s\n", out_file))
