# rformat 0.2.0

* `wrap_long_calls()` re-packs an already-wrapped call so its first argument lands on the call line rather than a hanging continuation (#30).
* Close the gap between a collapsed function signature and a bare single-expression body (#29).
* `rformat_file()` and `rformat_dir()` no longer append a blank line at end of file (#34).
* Fix a literal-conversion warning and a codoc (documentation/usage) mismatch (#25).
* Add a formatting-options vignette and refresh the existing vignettes (#23).

# rformat 0.1.0

* Initial CRAN release: a base-R code formatter giving consistent spacing, indentation, and structure, with no runtime dependency beyond 'Rcpp'.
