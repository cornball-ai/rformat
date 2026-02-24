#!/usr/bin/env r
#
# Stress test rformat against CRAN packages
#
# Takes a package name as argument, downloads source, formats all R/ files,
# checks parsing and idempotency. Outputs one TSV line per file to stdout.
#
# Usage:
#   r lab/stress_test.R dplyr                          # one package
#   echo dplyr | r lab/stress_test.R                   # from stdin
#   cat packages.txt | parallel -j8 --timeout 30 r lab/stress_test.R {}  # parallel
#
# Output columns (tab-separated):
#   package  file  status  lines  bytes  fmt_ms  idemp_ms

library(rformat)

# Package name from argv (littler) or stdin
if (exists("argv") && length(argv) > 0) {
    pkg <- argv[1]
} else {
    pkg <- trimws(readLines("stdin", n = 1, warn = FALSE))
}
if (!nzchar(pkg)) {
    stop("Usage: r lab/stress_test.R <package>")
}

# Download source tarball (cached)
cache_dir <- path.expand("~/.cache/rformat_cran_src")
dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

cached <- list.files(cache_dir, pattern = paste0("^", pkg, "_.*\\.tar\\.gz$"),
                     full.names = TRUE)
if (length(cached) > 0) {
    tarball <- cached[1]
} else {
    dl <- tryCatch(
        download.packages(pkg, destdir = cache_dir, type = "source",
                          repos = "https://cloud.r-project.org",
                          quiet = TRUE),
        error = function(e) NULL
    )
    if (is.null(dl) || nrow(dl) == 0) {
        cat(paste(pkg, "SKIP", "", 0, 0, 0, 0, sep = "\t"), "\n", sep = "")
        quit(save = "no", status = 0)
    }
    tarball <- dl[1, 2]
}

# Extract to temp (per-package dir avoids race conditions under parallel)
work_dir <- file.path(tempdir(), paste0("rformat_stress_", pkg, "_", Sys.getpid()))
dir.create(work_dir, showWarnings = FALSE, recursive = TRUE)
pkg_dir <- file.path(work_dir, pkg)
if (dir.exists(pkg_dir)) unlink(pkg_dir, recursive = TRUE)
untar(tarball, exdir = work_dir)

r_dir <- file.path(pkg_dir, "R")
if (!dir.exists(r_dir)) {
    cat(paste(pkg, "SKIP", "", 0, 0, 0, 0, sep = "\t"), "\n", sep = "")
    unlink(pkg_dir, recursive = TRUE)
    quit(save = "no", status = 0)
}

r_files <- list.files(r_dir, pattern = "\\.[Rr]$", full.names = TRUE,
                      recursive = TRUE)
if (length(r_files) == 0) {
    cat(paste(pkg, "SKIP", "", 0, 0, 0, 0, sep = "\t"), "\n", sep = "")
    unlink(pkg_dir, recursive = TRUE)
    quit(save = "no", status = 0)
}

# Format each file: output one TSV line per file
# Columns: package  file  status  lines  bytes  fmt_ms  idemp_ms
for (f in r_files) {
    bn <- basename(f)
    original <- paste(readLines(f, warn = FALSE), collapse = "\n")
    n_lines <- length(strsplit(original, "\n", fixed = TRUE)[[1]])
    n_bytes <- nchar(original, type = "bytes")

    # Check original parses
    if (is.null(tryCatch(parse(text = original, keep.source = TRUE),
                         error = function(e) NULL))) {
        cat(paste(pkg, bn, "NOPARSE", n_lines, n_bytes, 0, 0, sep = "\t"),
            "\n", sep = "")
        next
    }

    # Format pass 1
    t0 <- proc.time()[["elapsed"]]
    formatted <- tryCatch(
        withCallingHandlers(rformat(original),
            warning = function(w) invokeRestart("muffleWarning")),
        error = function(e) NULL
    )
    fmt_ms <- round((proc.time()[["elapsed"]] - t0) * 1000)

    if (is.null(formatted)) {
        cat(paste(pkg, bn, "ERROR", n_lines, n_bytes, fmt_ms, 0, sep = "\t"),
            "\n", sep = "")
        next
    }

    # Verify formatted code parses
    parsed <- tryCatch(parse(text = formatted, keep.source = TRUE),
                       error = function(e) e)

    if (inherits(parsed, "error")) {
        cat(paste(pkg, bn, "FAIL", n_lines, n_bytes, fmt_ms, 0, sep = "\t"),
            "\n", sep = "")
        next
    }

    # Idempotence check (format pass 2)
    t1 <- proc.time()[["elapsed"]]
    formatted2 <- tryCatch(
        withCallingHandlers(rformat(formatted),
            warning = function(w) invokeRestart("muffleWarning")),
        error = function(e) NULL)
    idemp_ms <- round((proc.time()[["elapsed"]] - t1) * 1000)

    if (!is.null(formatted2) && formatted2 != formatted) {
        cat(paste(pkg, bn, "IDEMP", n_lines, n_bytes, fmt_ms, idemp_ms,
                  sep = "\t"), "\n", sep = "")
    } else {
        cat(paste(pkg, bn, "OK", n_lines, n_bytes, fmt_ms, idemp_ms,
                  sep = "\t"), "\n", sep = "")
    }
}

# Sentinel: signals package completed (not killed by --timeout)
cat(paste(pkg, "_DONE_", "DONE", length(r_files), 0, 0, 0, sep = "\t"),
    "\n", sep = "")

unlink(work_dir, recursive = TRUE)
