code <- "status <- if (traceable &&\n              isTRUE(correct)) \"PASS\" else if (traceable) sprintf(\"TRACE-OK (%d breaks)\",\n                                                                  n_breaks) else sprintf(\"%d breaks\",\n                                                                                         n_breaks)"

parsed <- parse(text = code, keep.source = TRUE)
pd <- getParseData(parsed)
terminals <- pd[pd$terminal,]
terminals <- terminals[order(terminals$line1, terminals$col1),]

close_brackets <- c("')'", "']'", "'}'", "']]'")
open_brackets <- c("'('", "'['", "'{'")

for (i in seq_len(nrow(terminals))) {
    tok <- terminals[i,]
    if (!(tok$token %in% c("IF", "FOR", "WHILE"))) next

    ctrl_line <- tok$line1
    cat(sprintf("Token %d: %s at L%d C%d\n", i, tok$token, tok$line1, tok$col1))

    if (tok$token == "IF") {
        before <- terminals[terminals$line1 == ctrl_line & terminals$col1 < tok$col1,]
        cat("  before tokens:", paste(before$token, collapse = ", "), "\n")
        if (nrow(before) > 0 && any(before$token %in% c("LEFT_ASSIGN", "EQ_ASSIGN", "RIGHT_ASSIGN"))) {
            cat("  -> SKIP (assignment)\n")
            next
        }

        if (i > 1 && terminals$token[i - 1] == "ELSE") {
            cat("  Preceded by ELSE, tracing chain...\n")
            chain_idx <- i - 1
            iter <- 0
            while (chain_idx > 1 && terminals$token[chain_idx] == "ELSE") {
                iter <- iter + 1
                if (iter > 50) { cat("  CHAIN LOOP EXCEEDED\n"); break }
                scan <- chain_idx - 1
                depth <- 0
                while (scan >= 1) {
                    st <- terminals$token[scan]
                    if (st %in% close_brackets) {
                        depth <- depth + 1
                    } else if (st %in% open_brackets) {
                        depth <- depth - 1
                    } else if (st == "LBB") {
                        depth <- depth - 2
                    }
                    if (depth <= 0 && st == "IF") { break }
                    scan <- scan - 1
                }
                cat(sprintf("  scan stopped at %d: %s\n", scan,
                            if (scan >= 1) terminals$token[scan] else "OOB"))
                if (scan < 1 || terminals$token[scan] != "IF") { break }
                if (scan > 1 && terminals$token[scan - 1] == "ELSE") {
                    cat(sprintf("  Found IF at %d preceded by ELSE, continuing\n", scan))
                    chain_idx <- scan - 1
                } else {
                    root_line <- terminals$line1[scan]
                    root_before <- terminals[terminals$line1 == root_line &
                        terminals$col1 < terminals$col1[scan],]
                    cat(sprintf("  Root IF at %d (L%d), before: %s\n", scan, root_line,
                                paste(root_before$token, collapse = ", ")))
                    if (nrow(root_before) > 0 &&
                        any(root_before$token %in% c("LEFT_ASSIGN", "EQ_ASSIGN", "RIGHT_ASSIGN"))) {
                        cat("  -> SKIP (root is assignment)\n")
                    }
                    break
                }
            }
        }
    }
}
