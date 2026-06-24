## Submission

This is an update to the CRAN release 0.1.0, collecting formatting fixes:

* `wrap_long_calls()` re-packs an already-wrapped call so its first argument
  lands on the call line (#30); the gap between a collapsed signature and a
  bare body is closed (#29); `rformat_file()` / `rformat_dir()` no longer
  append a trailing blank line (#34).

See NEWS.md for the full list.

## Test environments

* local: Ubuntu 24.04, R 4.5.x

## R CMD check results

0 errors | 0 warnings | 1 note

* The note is "non-portable compilation flags", produced by the local system
  R configuration (the CFLAGS Debian/Ubuntu sets), not by the package.
  Installed size is ~6 Mb, the Rcpp-compiled shared library.

## Additional testing

rformat's test suite covers idempotency and parse round-trip behaviour. This
0.2.0 release was additionally stress-tested against all R files from 126 CRAN
and base R packages with randomized style parameters (0 parse failures, 0
idempotency exceptions).

## Reverse dependencies

None on CRAN.
