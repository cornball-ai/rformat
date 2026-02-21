#!/usr/bin/env r
#
# Download CRAN source tarballs into a local cache.
# Usage:
#   r lab/download_stress_pkgs.R
#   N=10 r lab/download_stress_pkgs.R
#   PKGS="dplyr rlang" r lab/download_stress_pkgs.R
#   CACHE_DIR=~/.cache/rformat_cran_src r lab/download_stress_pkgs.R

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
dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

repos <- getOption("repos")
if (is.null(repos) || length(repos) == 0 || identical(repos[1], "@CRAN@")) {
    repos <- "https://cloud.r-project.org"
}

cat(sprintf("Downloading %d packages to %s\n", length(packages), cache_dir))

for (pkg in packages) {
    tarball <- file.path(cache_dir, paste0(pkg, ".tar.gz"))
    if (file.exists(tarball)) {
        cat(sprintf("SKIP %s (cached)\n", pkg))
        next
    }
    cat(sprintf("GET  %s\n", pkg))
    dl <- tryCatch(
        download.packages(pkg, destdir = cache_dir, type = "source",
                          repos = repos, quiet = TRUE),
        error = function(e) NULL
    )
    if (is.null(dl) || nrow(dl) == 0) {
        cat(sprintf("FAIL %s (download failed)\n", pkg))
        next
    }
}
