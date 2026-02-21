# Test getParseData column behavior with tabs

# Test 1: tab at start of line
code1 <- "\tx <- 1"
p1 <- getParseData(parse(text = code1, keep.source = TRUE))
t1 <- p1[p1$terminal & p1$token == "SYMBOL",]
cat("Tab test: x at col", t1$col1,
    "(9 if tab-expanded, 2 if char-based)\n")

# Test 2: multi-byte char (em-dash) in string
code2 <- 'x <- "\u2014y"'
p2 <- getParseData(parse(text = code2, keep.source = TRUE))
t2 <- p2[p2$terminal,]
t2 <- t2[order(t2$col1),]
cat("\nEm-dash in string test:\n")
for (i in seq_len(nrow(t2))) {
    cat(sprintf("  C%d-%d %s [%s]\n",
                t2$col1[i], t2$col2[i], t2$token[i], substr(t2$text[i], 1, 30)))
}

# Test 3: tab on line inside multiline string (the Rcpp case)
code3 <- paste0('sprintf("hello\n', '\t\tworld", x)')
cat("\nTest 3 code:\n", code3, "\n\n")
p3 <- getParseData(parse(text = code3, keep.source = TRUE))
t3 <- p3[p3$terminal,]
t3 <- t3[order(t3$line1, t3$col1),]
cat("Tokens:\n")
for (i in seq_len(nrow(t3))) {
    cat(sprintf("  L%d C%d-%d %s [%s]\n",
                t3$line1[i], t3$col1[i], t3$col2[i],
                t3$token[i], substr(t3$text[i], 1, 30)))
}

# Test 4: actual tab in code (not in string)
code4 <- "x\t<- 1"
p4 <- getParseData(parse(text = code4, keep.source = TRUE))
t4 <- p4[p4$terminal,]
t4 <- t4[order(t4$col1),]
cat("\nTab in code test:\n")
for (i in seq_len(nrow(t4))) {
    cat(sprintf("  C%d-%d %s [%s]\n",
                t4$col1[i], t4$col2[i], t4$token[i], t4$text[i]))
}
