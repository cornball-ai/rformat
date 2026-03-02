#include <Rcpp.h>
#include "token.h"
#include <map>

// Convert R DataFrame (from getParseData) to vector<Token>
static std::vector<Token> dataframe_to_tokens(Rcpp::DataFrame pd) {
    Rcpp::IntegerVector line1 = pd["line1"];
    Rcpp::IntegerVector col1 = pd["col1"];
    Rcpp::IntegerVector line2 = pd["line2"];
    Rcpp::IntegerVector col2 = pd["col2"];
    Rcpp::IntegerVector id = pd["id"];
    Rcpp::IntegerVector parent = pd["parent"];
    Rcpp::CharacterVector token = pd["token"];
    Rcpp::LogicalVector terminal = pd["terminal"];
    Rcpp::CharacterVector text = pd["text"];

    int n = line1.size();
    std::vector<Token> tokens;
    tokens.reserve(n);

    for (int i = 0; i < n; i++) {
        if (!terminal[i]) continue; // Only terminal tokens

        Token t;
        t.token = Rcpp::as<std::string>(token[i]);
        t.text = Rcpp::as<std::string>(text[i]);
        t.out_text = t.text;
        t.line1 = line1[i];
        t.col1 = col1[i];
        t.line2 = line2[i];
        t.col2 = col2[i];
        t.id = id[i];
        t.parent = parent[i];
        t.terminal = true;
        t.out_line = 0;
        t.out_order = 0;
        t.brace_depth = 0;
        t.paren_depth = 0;
        t.pab = 0;
        t.nesting_level = 0;
        tokens.push_back(t);
    }

    return tokens;
}

// Split code string into lines
static std::vector<std::string> split_lines(const std::string& code) {
    std::vector<std::string> lines;
    std::string line;
    for (char c : code) {
        if (c == '\n') {
            lines.push_back(line);
            line.clear();
        } else {
            line += c;
        }
    }
    lines.push_back(line); // last line (no trailing newline)
    return lines;
}

// Run the full AST transform pipeline on enriched tokens
static void run_pipeline_transforms(std::vector<Token>& tokens,
                                    const FormatOptions& opts) {
    collapse_calls(tokens, opts);
    recompute_nesting(tokens);
    if (!opts.control_braces.empty()) {
        add_control_braces(tokens, opts);
    }

    expand_call_if_args(tokens, opts);
    if (!opts.control_braces.empty()) {
        add_control_braces(tokens, opts);
    }

    wrap_long_operators(tokens, opts);
    wrap_long_calls(tokens, opts);
    reformat_function_defs(tokens, opts);
    if (!opts.control_braces.empty()) {
        add_control_braces(tokens, opts);
    }

    if (opts.expand_if) {
        FormatOptions if_opts = opts;
        if_opts.line_limit = 0;
        reformat_inline_if(tokens, if_opts);
    } else {
        reformat_inline_if(tokens, opts);
    }

    if (!opts.control_braces.empty()) {
        add_control_braces(tokens, opts);
    }

    expand_call_if_args(tokens, opts);
    if (!opts.control_braces.empty()) {
        add_control_braces(tokens, opts);
    }

    wrap_long_operators(tokens, opts);
    wrap_long_calls(tokens, opts);
    wrap_long_operators(tokens, opts);
    collapse_calls(tokens, opts);
    recompute_nesting(tokens);
    if (!opts.control_braces.empty()) {
        add_control_braces(tokens, opts);
        wrap_long_operators(tokens, opts);
        wrap_long_calls(tokens, opts);
        wrap_long_operators(tokens, opts);
        collapse_calls(tokens, opts);
        recompute_nesting(tokens);
    }
}

// [[Rcpp::export]]
Rcpp::String cpp_format_pipeline(std::string code,
                                 std::string indent_str,
                                 std::string wrap,
                                 bool expand_if,
                                 std::string brace_style,
                                 int line_limit,
                                 bool function_space,
                                 std::string control_braces) {
    // Parse code via R
    Rcpp::Function parse_fn("parse");
    Rcpp::Function getParseData_fn("getParseData");

    SEXP parsed;
    try {
        parsed = parse_fn(Rcpp::Named("text") = code,
                          Rcpp::Named("keep.source") = true);
    } catch (...) {
        return Rcpp::String(code);
    }

    Rcpp::DataFrame pd;
    try {
        pd = Rcpp::as<Rcpp::DataFrame>(getParseData_fn(parsed));
    } catch (...) {
        return Rcpp::String(code);
    }

    if (pd.nrows() == 0) {
        return Rcpp::String(code);
    }

    // Convert to C++ tokens
    std::vector<Token> tokens = dataframe_to_tokens(pd);
    if (tokens.empty()) {
        return Rcpp::String(code);
    }

    std::vector<std::string> orig_lines = split_lines(code);
    restore_truncated_str_const(tokens, orig_lines);
    enrich_terminals(tokens);

    FormatOptions opts;
    opts.indent_str = indent_str;
    opts.wrap = wrap;
    opts.expand_if = expand_if;
    opts.brace_style = brace_style;
    opts.line_limit = line_limit;
    opts.function_space = function_space;
    opts.control_braces = control_braces;

    run_pipeline_transforms(tokens, opts);

    std::string result = serialize_tokens(tokens, opts);
    return Rcpp::String(result);
}

// [[Rcpp::export]]
Rcpp::String cpp_format_all(std::string code,
                            std::string indent_str,
                            std::string wrap,
                            bool expand_if,
                            std::string brace_style,
                            int line_limit,
                            bool function_space,
                            std::string control_braces) {
    // Parse full code once
    Rcpp::Function parse_fn("parse");
    Rcpp::Function getParseData_fn("getParseData");

    SEXP parsed;
    try {
        parsed = parse_fn(Rcpp::Named("text") = code,
                          Rcpp::Named("keep.source") = true);
    } catch (...) {
        return Rcpp::String(code);
    }

    Rcpp::DataFrame pd;
    try {
        pd = Rcpp::as<Rcpp::DataFrame>(getParseData_fn(parsed));
    } catch (...) {
        return Rcpp::String(code);
    }

    if (pd.nrows() == 0) {
        return Rcpp::String(code);
    }

    // Access parse data columns
    Rcpp::IntegerVector pd_line1 = pd["line1"];
    Rcpp::IntegerVector pd_col1 = pd["col1"];
    Rcpp::IntegerVector pd_line2 = pd["line2"];
    Rcpp::IntegerVector pd_col2 = pd["col2"];
    Rcpp::IntegerVector pd_id = pd["id"];
    Rcpp::IntegerVector pd_parent = pd["parent"];
    Rcpp::CharacterVector pd_token = pd["token"];
    Rcpp::LogicalVector pd_terminal = pd["terminal"];
    Rcpp::CharacterVector pd_text = pd["text"];
    int n_pd = pd_line1.size();

    // Split code into lines (0-based vector, 1-based line numbers)
    std::vector<std::string> all_lines = split_lines(code);
    int n_lines = static_cast<int>(all_lines.size());

    // Find top-level expression boundaries
    struct ExprRange { int start; int end; }; // 1-based line numbers
    std::vector<ExprRange> expr_ranges;

    for (int i = 0; i < n_pd; i++) {
        if (pd_parent[i] != 0) continue;
        std::string tok = Rcpp::as<std::string>(pd_token[i]);
        if (tok == "expr" || tok == "expr_or_assign_or_help") {
            expr_ranges.push_back({pd_line1[i], pd_line2[i]});
        }
    }

    if (expr_ranges.empty()) {
        return Rcpp::String(code);
    }

    // Sort by start line
    std::sort(expr_ranges.begin(), expr_ranges.end(),
              [](const ExprRange& a, const ExprRange& b) {
                  return a.start < b.start;
              });

    // Merge overlapping/adjacent expressions (semicolons on same line)
    std::vector<ExprRange> merged;
    merged.push_back(expr_ranges[0]);
    for (size_t i = 1; i < expr_ranges.size(); i++) {
        if (expr_ranges[i].start <= merged.back().end) {
            merged.back().end = std::max(merged.back().end,
                                         expr_ranges[i].end);
        } else {
            merged.push_back(expr_ranges[i]);
        }
    }

    // Build format options
    FormatOptions opts;
    opts.indent_str = indent_str;
    opts.wrap = wrap;
    opts.expand_if = expand_if;
    opts.brace_style = brace_style;
    opts.line_limit = line_limit;
    opts.function_space = function_space;
    opts.control_braces = control_braces;

    // Collect terminal tokens per expression, with adjusted line numbers
    std::vector<std::vector<Token>> expr_tokens(merged.size());

    for (int i = 0; i < n_pd; i++) {
        if (!pd_terminal[i]) continue;

        int tl1 = pd_line1[i];

        // Find which merged expression this terminal belongs to
        int expr_idx = -1;
        for (size_t k = 0; k < merged.size(); k++) {
            if (tl1 >= merged[k].start && tl1 <= merged[k].end) {
                expr_idx = static_cast<int>(k);
                break;
            }
        }
        if (expr_idx < 0) continue; // not in any expression (shouldn't happen)

        int base_line = merged[expr_idx].start;

        Token t;
        t.token = Rcpp::as<std::string>(pd_token[i]);
        t.text = Rcpp::as<std::string>(pd_text[i]);
        t.out_text = t.text;
        t.line1 = pd_line1[i] - base_line + 1; // 1-based relative
        t.col1 = pd_col1[i];
        t.line2 = pd_line2[i] - base_line + 1;
        t.col2 = pd_col2[i];
        t.id = pd_id[i];
        t.parent = pd_parent[i];
        t.terminal = true;
        t.out_line = 0;
        t.out_order = 0;
        t.brace_depth = 0;
        t.paren_depth = 0;
        t.pab = 0;
        t.nesting_level = 0;
        expr_tokens[expr_idx].push_back(t);
    }

    // Build result by concatenating gaps and formatted expressions
    std::string result;
    int prev_end = 0; // 1-based: last processed line number

    for (size_t k = 0; k < merged.size(); k++) {
        int expr_start = merged[k].start; // 1-based
        int expr_end = merged[k].end;     // 1-based

        // Gap before this expression
        if (expr_start > prev_end + 1) {
            for (int ln = prev_end + 1; ln < expr_start; ln++) {
                if (!result.empty()) result += "\n";
                result += all_lines[ln - 1]; // 0-based index
            }
        }

        std::vector<Token>& tokens = expr_tokens[k];

        if (tokens.empty()) {
            // No terminals — include raw text
            for (int ln = expr_start; ln <= expr_end; ln++) {
                if (!result.empty()) result += "\n";
                result += all_lines[ln - 1];
            }
        } else {
            // Get expression's source lines for string restoration
            std::vector<std::string> expr_lines;
            for (int ln = expr_start; ln <= expr_end; ln++) {
                expr_lines.push_back(all_lines[ln - 1]);
            }

            restore_truncated_str_const(tokens, expr_lines);
            enrich_terminals(tokens);
            run_pipeline_transforms(tokens, opts);

            std::string formatted = serialize_tokens(tokens, opts);

            // Remove trailing newline for concatenation
            while (!formatted.empty() && formatted.back() == '\n') {
                formatted.pop_back();
            }

            if (!result.empty()) result += "\n";
            result += formatted;
        }

        prev_end = expr_end;
    }

    // Trailing gap after last expression
    if (prev_end < n_lines) {
        for (int ln = prev_end + 1; ln <= n_lines; ln++) {
            if (!result.empty()) result += "\n";
            result += all_lines[ln - 1];
        }
    }

    if (!result.empty() && result.back() != '\n') {
        result += "\n";
    }

    return Rcpp::String(result);
}
