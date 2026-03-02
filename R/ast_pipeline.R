#' AST-Based Format Pipeline
#'
#' Single-pass pipeline: parse once, enrich the terminal DataFrame, run
#' all transforms as DataFrame operations, serialize to text once.
#'
#' @param code Code string for one top-level expression.
#' @param indent Indent string or integer.
#' @param wrap Continuation style: `"paren"` or `"fixed"`.
#' @param expand_if Whether to expand all inline if-else.
#' @param brace_style `"kr"` or `"allman"`.
#' @param line_limit Maximum line length.
#' @param function_space Add space after `function`.
#' @param control_braces Control brace mode.
#' @return Formatted code string.
#' @keywords internal
format_pipeline <- function(code, indent, wrap, expand_if, brace_style,
                            line_limit, function_space = FALSE,
                            control_braces = FALSE) {
    if (is.numeric(indent)) {
        indent_str <- strrep(" ", indent)
    } else {
        indent_str <- indent
    }

    # Parse once
    pd <- tryCatch(getParseData(parse(text = code, keep.source = TRUE)),
                   error = function(e) NULL)
    if (is.null(pd) || nrow(pd) == 0L) {
        return(code)
    }

    orig_lines <- strsplit(code, "\n", fixed = TRUE)[[1]]
    terms <- enrich_terminals(pd, orig_lines)

    # --- AST transforms (single DataFrame, no re-parsing) ---

    terms <- collapse_calls(terms, indent_str, line_limit)
    if (!isFALSE(control_braces)) {
        terms <- add_control_braces(terms, control_braces, indent_str,
                                    line_limit)
    }

    terms <- expand_call_if_args(terms, indent_str, line_limit)
    if (!isFALSE(control_braces)) {
        terms <- add_control_braces(terms, control_braces, indent_str,
                                    line_limit)
    }

    terms <- wrap_long_operators(terms, indent_str, line_limit)
    terms <- wrap_long_calls(terms, indent_str, wrap, line_limit)
    terms <- reformat_function_defs(terms, indent_str, wrap = wrap,
                                    brace_style = brace_style,
                                    line_limit = line_limit,
                                    function_space = function_space)
    if (!isFALSE(control_braces)) {
        terms <- add_control_braces(terms, control_braces, indent_str,
                                    line_limit)
    }

    if (isTRUE(expand_if)) {
        terms <- reformat_inline_if(terms, indent_str, line_limit = 0L)
    } else {
        terms <- reformat_inline_if(terms, indent_str, line_limit = line_limit)
    }

    if (!isFALSE(control_braces)) {
        terms <- add_control_braces(terms, control_braces, indent_str,
                                    line_limit)
    }

    terms <- expand_call_if_args(terms, indent_str, line_limit)
    if (!isFALSE(control_braces)) {
        terms <- add_control_braces(terms, control_braces, indent_str,
                                    line_limit)
    }

    terms <- collapse_calls(terms, indent_str, line_limit)
    terms <- wrap_long_operators(terms, indent_str, line_limit)
    terms <- wrap_long_calls(terms, indent_str, wrap, line_limit)
    terms <- wrap_long_operators(terms, indent_str, line_limit)
    if (!isFALSE(control_braces)) {
        terms <- add_control_braces(terms, control_braces, indent_str,
                                    line_limit)
    }

    serialize_tokens(terms, indent_str, wrap, line_limit)
}

