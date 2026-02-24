# Analyze R Core style conventions from source files
# Reports: brace style, indentation, function signatures

analyze_file <- function (path) {
    lines <- tryCatch(readLines(path, warn = FALSE), error = function(e) NULL)
    if (is.null(lines)) return(NULL)

    parsed <- tryCatch(parse(text = lines, keep.source = TRUE),
                       error = function(e) NULL)
    if (is.null(parsed)) return(NULL)

    pd <- getParseData(parsed)
    if (is.null(pd) || nrow(pd) == 0) return(NULL)

    terminals <- pd[pd$terminal, ]
    terminals <- terminals[order(terminals$line1, terminals$col1), ]

    n_lines <- length(lines)

    # --- Brace style: look for FUNCTION tokens and where { appears ---
    func_indices <- which(terminals$token == "FUNCTION")
    n_kr <- 0L
    n_allman <- 0L
    n_oneliner <- 0L

    for (fi in func_indices) {
        # Find the opening paren after FUNCTION
        op_idx <- fi + 1L
        if (op_idx > nrow(terminals)) next
        if (terminals$token[op_idx] != "'('") next

        # Find matching close paren
        depth <- 1L
        cp_idx <- op_idx + 1L
        while (cp_idx <= nrow(terminals) && depth > 0L) {
            if (terminals$token[cp_idx] == "'('") depth <- depth + 1L
            if (terminals$token[cp_idx] == "')'") depth <- depth - 1L
            if (depth > 0L) cp_idx <- cp_idx + 1L
        }
        if (cp_idx > nrow(terminals)) next

        # Find next meaningful token after close paren
        next_idx <- cp_idx + 1L
        if (next_idx > nrow(terminals)) next

        close_line <- terminals$line1[cp_idx]
        next_tok <- terminals[next_idx, ]

        if (next_tok$token == "'{'") {
            if (next_tok$line1 == close_line) {
                n_kr <- n_kr + 1L
            } else {
                n_allman <- n_allman + 1L
            }
        } else {
            # No brace — one-liner function
            n_oneliner <- n_oneliner + 1L
        }
    }

    # --- Indentation: tabs vs spaces ---
    indented <- lines[grepl("^\\s+\\S", lines)]
    n_tab_indent <- sum(grepl("^\t", indented))
    n_space_indent <- sum(grepl("^ ", indented))

    # Measure space indent sizes (first non-blank char position)
    space_indents <- integer(0)
    for (l in indented) {
        m <- regexpr("^( +)", l)
        if (m > 0) {
            space_indents <- c(space_indents, attr(m, "match.length"))
        }
    }

    # --- Function signature continuation ---
    # Check if function args span multiple lines
    n_single_sig <- 0L
    n_multi_sig <- 0L
    n_paren_align <- 0L
    n_tab_cont <- 0L
    n_other_cont <- 0L

    for (fi in func_indices) {
        op_idx <- fi + 1L
        if (op_idx > nrow(terminals)) next
        if (terminals$token[op_idx] != "'('") next

        depth <- 1L
        cp_idx <- op_idx + 1L
        while (cp_idx <= nrow(terminals) && depth > 0L) {
            if (terminals$token[cp_idx] == "'('") depth <- depth + 1L
            if (terminals$token[cp_idx] == "')'") depth <- depth - 1L
            if (depth > 0L) cp_idx <- cp_idx + 1L
        }
        if (cp_idx > nrow(terminals)) next

        open_line <- terminals$line1[op_idx]
        close_line <- terminals$line1[cp_idx]

        if (close_line == open_line) {
            n_single_sig <- n_single_sig + 1L
        } else {
            n_multi_sig <- n_multi_sig + 1L
            # Check continuation style on line after open paren
            cont_line_num <- open_line + 1L
            if (cont_line_num <= n_lines) {
                cont_line <- lines[cont_line_num]
                leading <- sub("^(\\s*).*", "\\1", cont_line)
                if (grepl("^\t", leading)) {
                    n_tab_cont <- n_tab_cont + 1L
                } else {
                    open_col <- terminals$col1[op_idx]
                    lead_len <- nchar(leading)
                    if (lead_len == open_col) {
                        n_paren_align <- n_paren_align + 1L
                    } else {
                        n_other_cont <- n_other_cont + 1L
                    }
                }
            }
        }
    }

    # --- Space after function ---
    n_space_after_func <- 0L
    n_nospace_after_func <- 0L
    for (fi in func_indices) {
        func_tok <- terminals[fi, ]
        op_idx <- fi + 1L
        if (op_idx > nrow(terminals)) next
        if (terminals$token[op_idx] != "'('") next
        op_tok <- terminals[op_idx, ]
        if (op_tok$line1 != func_tok$line1) next
        gap <- op_tok$col1 - func_tok$col2 - 1L
        if (gap > 0L) {
            n_space_after_func <- n_space_after_func + 1L
        } else {
            n_nospace_after_func <- n_nospace_after_func + 1L
        }
    }

    list(
        file = path,
        n_lines = n_lines,
        n_functions = length(func_indices),
        brace_kr = n_kr,
        brace_allman = n_allman,
        brace_none = n_oneliner,
        indent_tab = n_tab_indent,
        indent_space = n_space_indent,
        space_indents = space_indents,
        sig_single = n_single_sig,
        sig_multi = n_multi_sig,
        cont_paren = n_paren_align,
        cont_tab = n_tab_cont,
        cont_other = n_other_cont,
        func_space = n_space_after_func,
        func_nospace = n_nospace_after_func
    )
}

# --- Gather files ---
cache <- path.expand("~/.cache/rformat_cran_src")
base_src <- file.path(cache, "base_r_src")

base_pkgs <- c("base", "compiler", "graphics", "grDevices", "grid",
               "methods", "parallel", "splines", "stats", "stats4",
               "tcltk", "tools", "utils")

rec_pkgs <- c("codetools", "lattice", "MASS", "Matrix", "mgcv",
              "nlme", "nnet", "survival")

# Base package files
base_files <- character(0)
for (pkg in base_pkgs) {
    pkg_dir <- file.path(base_src, pkg, "R")
    if (dir.exists(pkg_dir)) {
        base_files <- c(base_files, list.files(pkg_dir, pattern = "\\.R$",
                                               full.names = TRUE))
    }
}

# Recommended package files (from tarballs)
rec_files <- character(0)
rec_tmp <- file.path(tempdir(), "rec_src")
dir.create(rec_tmp, showWarnings = FALSE)
for (pkg in rec_pkgs) {
    tarball <- list.files(cache, pattern = paste0("^", pkg, "_.*\\.tar\\.gz$"),
                          full.names = TRUE)[1]
    if (!is.na(tarball)) {
        untar(tarball, exdir = rec_tmp)
        pkg_r_dir <- file.path(rec_tmp, pkg, "R")
        if (dir.exists(pkg_r_dir)) {
            rec_files <- c(rec_files, list.files(pkg_r_dir, pattern = "\\.R$",
                                                 full.names = TRUE))
        }
    }
}

cat("Base files:", length(base_files), "\n")
cat("Recommended files:", length(rec_files), "\n\n")

# --- Analyze ---
report <- function (results, label) {
    cat("=== ", label, " ===\n\n", sep = "")

    tot_lines <- sum(sapply(results, `[[`, "n_lines"))
    tot_funcs <- sum(sapply(results, `[[`, "n_functions"))
    cat("Files:", length(results), "\n")
    cat("Lines:", tot_lines, "\n")
    cat("Function definitions:", tot_funcs, "\n\n")

    # Brace style
    kr <- sum(sapply(results, `[[`, "brace_kr"))
    allman <- sum(sapply(results, `[[`, "brace_allman"))
    none <- sum(sapply(results, `[[`, "brace_none"))
    braced <- kr + allman
    cat("Brace style (functions with braces):\n")
    cat("  K&R (same line):    ", kr, " (",
        round(100 * kr / braced, 1), "%)\n", sep = "")
    cat("  Allman (own line):  ", allman, " (",
        round(100 * allman / braced, 1), "%)\n", sep = "")
    cat("  No braces (1-liner):", none, "\n\n")

    # Indentation
    tab_ind <- sum(sapply(results, `[[`, "indent_tab"))
    space_ind <- sum(sapply(results, `[[`, "indent_space"))
    tot_ind <- tab_ind + space_ind
    cat("Indentation:\n")
    cat("  Tab-indented lines:   ", tab_ind, " (",
        round(100 * tab_ind / tot_ind, 1), "%)\n", sep = "")
    cat("  Space-indented lines: ", space_ind, " (",
        round(100 * space_ind / tot_ind, 1), "%)\n", sep = "")

    # Space indent sizes
    all_spaces <- unlist(lapply(results, `[[`, "space_indents"))
    if (length(all_spaces) > 0) {
        tab <- sort(table(all_spaces), decreasing = TRUE)
        cat("  Most common space indents: ")
        top <- head(tab, 5)
        cat(paste0(names(top), "sp(", top, ")"), sep = ", ")
        cat("\n")
    }
    cat("\n")

    # Signatures
    single <- sum(sapply(results, `[[`, "sig_single"))
    multi <- sum(sapply(results, `[[`, "sig_multi"))
    cat("Function signatures:\n")
    cat("  Single-line:", single, " (",
        round(100 * single / (single + multi), 1), "%)\n", sep = "")
    cat("  Multi-line: ", multi, " (",
        round(100 * multi / (single + multi), 1), "%)\n", sep = "")

    # Continuation style
    paren <- sum(sapply(results, `[[`, "cont_paren"))
    tab_c <- sum(sapply(results, `[[`, "cont_tab"))
    other <- sum(sapply(results, `[[`, "cont_other"))
    if (multi > 0) {
        cat("  Continuation style:\n")
        cat("    Paren-aligned:", paren, "\n")
        cat("    Tab-indented: ", tab_c, "\n")
        cat("    Other:        ", other, "\n")
    }
    cat("\n")

    # Space after function
    sp <- sum(sapply(results, `[[`, "func_space"))
    nosp <- sum(sapply(results, `[[`, "func_nospace"))
    cat("Space after 'function':\n")
    cat("  function (: ", sp, " (",
        round(100 * sp / (sp + nosp), 1), "%)\n", sep = "")
    cat("  function(:  ", nosp, " (",
        round(100 * nosp / (sp + nosp), 1), "%)\n", sep = "")
    cat("\n")
}

cat("Analyzing base packages...\n")
base_results <- lapply(base_files, analyze_file)
base_results <- Filter(Negate(is.null), base_results)

cat("Analyzing recommended packages...\n")
rec_results <- lapply(rec_files, analyze_file)
rec_results <- Filter(Negate(is.null), rec_results)

cat("\n")
report(base_results, "Base R (14 packages)")
report(c(base_results, rec_results), "Base + Recommended (22 packages)")
