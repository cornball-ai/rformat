if (!is.numeric(breaks) || !is.finite(breaks) ||
    breaks < 1L) { stop("invalid number of 'breaks'") }

