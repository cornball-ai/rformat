#include "token.h"
#include <set>

// Helper: find matching ELSE for an IF at close_idx+1..n
// Returns index of ELSE or -1
static int find_matching_else(const std::vector<Token>& tokens,
                              int close_idx) {
    int n = static_cast<int>(tokens.size());
    int search_idx = close_idx + 1;
    int nest_depth = 0, brace_depth = 0, paren_depth = 0;

    while (search_idx < n) {
        const std::string& st = tokens[search_idx].token;
        if (st == "'('" || st == "'['") {
            paren_depth++;
        } else if (st == "LBB") {
            paren_depth += 2;
        } else if (st == "')'" || st == "']'") {
            paren_depth--;
            if (paren_depth < 0) return -1;
        } else if (st == "']]'") {
            paren_depth -= 2;
            if (paren_depth < 0) return -1;
        }
        if (st == "'{'") brace_depth++;
        else if (st == "'}'") {
            brace_depth--;
            if (brace_depth < 0) return -1;
        }
        if (st == "IF") {
            nest_depth++;
        } else if (st == "ELSE") {
            if (nest_depth == 0 && brace_depth == 0 && paren_depth == 0)
                return search_idx;
            else if (nest_depth > 0)
                nest_depth--;
        }
        search_idx++;
    }
    return -1;
}

// Helper: find end of false expression
static int find_false_end(const std::vector<Token>& tokens, int false_start) {
    int n = static_cast<int>(tokens.size());
    int false_end = false_start;
    int fp_depth = 0, fb_depth = 0, fif_depth = 0;
    int false_start_line = tokens[false_start].out_line;

    while (false_end < n) {
        const std::string& ft = tokens[false_end].token;
        if (ft == "IF" && tokens[false_end].out_line == false_start_line)
            fif_depth++;
        if (ft == "ELSE" && tokens[false_end].out_line == false_start_line)
            fif_depth = std::max(0, fif_depth - 1);

        int prev_fp = fp_depth;
        if (ft == "'('" || ft == "'['") fp_depth++;
        else if (ft == "LBB") fp_depth += 2;
        if (ft == "','") {
            if (fp_depth == 0 && fb_depth == 0 && fif_depth == 0) {
                return false_end - 1;
            }
        }
        if (ft == "')'" || ft == "']'") {
            if (fp_depth == 0 && fb_depth == 0 && fif_depth == 0) {
                return false_end - 1;
            }
            fp_depth--;
        }
        if (ft == "']]'") fp_depth -= 2;
        if (ft == "'{'") fb_depth++;
        if (ft == "'}'") fb_depth--;

        if (prev_fp > 0 && fp_depth == 0 && fb_depth == 0 && fif_depth == 0) {
            bool next_ok = false_end + 1 < n &&
                tok_in(tokens[false_end + 1].token,
                       {"'['", "LBB", "'('", "'$'", "'@'", "'+'", "'-'",
                        "'*'", "'/'", "'^'", "PIPE", "SPECIAL", "OR2",
                        "AND2", "OR", "AND", "'~'", "EQ", "NE", "GE",
                        "LE", "GT", "LT"});
            if (!next_ok) return false_end;
            false_start_line = tokens[false_end].out_line;
        }

        if (tokens[false_end].out_line > false_start_line &&
            fp_depth <= 0 && fb_depth <= 0 && fif_depth == 0) {
            return false_end - 1;
        }
        false_end++;
    }
    return n - 1;
}

void expand_call_if_args(std::vector<Token>& tokens, const FormatOptions& opts) {
    bool changed = true;
    int max_iter = 200;

    while (changed && max_iter > 0) {
        max_iter--;
        changed = false;
        reorder_tokens(tokens);
        int n = static_cast<int>(tokens.size());

        for (int ii = 0; ii < n; ii++) {
            if (tokens[ii].token != "IF") continue;
            if (tokens[ii].paren_depth < 1) continue;

            int if_line = tokens[ii].out_line;
            if (ast_line_width(tokens, if_line, opts.indent_str,
                               opts.function_space) <= opts.line_limit)
                continue;

            // Check enclosing paren
            bool skip_if = false;
            int scan_depth = 0;
            int enc_paren_idx = -1;
            for (int j = ii - 1; j >= 0; j--) {
                if (tokens[j].token == "')'") scan_depth++;
                else if (tokens[j].token == "'('") {
                    if (scan_depth == 0) {
                        enc_paren_idx = j;
                        if (j > 0 && tokens[j - 1].token == "FUNCTION")
                            skip_if = true;
                        break;
                    }
                    scan_depth--;
                }
            }
            if (enc_paren_idx >= 0 &&
                tokens[ii].brace_depth > tokens[enc_paren_idx].brace_depth)
                skip_if = true;
            if (skip_if) continue;

            // Find condition
            int open_idx = ii + 1;
            if (open_idx >= n || tokens[open_idx].token != "'('") continue;
            int close_idx = find_matching_paren(tokens, open_idx);
            if (close_idx < 0) continue;

            int body_first = close_idx + 1;
            if (body_first >= n || tokens[body_first].token == "'{'") continue;

            int else_idx = find_matching_else(tokens, close_idx);
            if (else_idx < 0) continue;

            // True range
            if (close_idx + 1 >= else_idx) continue;

            int false_start = else_idx + 1;
            if (false_start >= n || tokens[false_start].token == "'{'")
                continue;

            int false_end = find_false_end(tokens, false_start);
            if (false_end < false_start) continue;

            // Skip complex branches
            bool complex = false;
            for (int k = close_idx + 1; k < else_idx; k++) {
                if (tokens[k].token == "FUNCTION" ||
                    tokens[k].token == "COMMENT") {
                    complex = true; break;
                }
            }
            for (int k = false_start; k <= false_end && !complex; k++) {
                if (tokens[k].token == "FUNCTION" ||
                    tokens[k].token == "COMMENT") {
                    complex = true; break;
                }
            }
            if (complex) continue;

            // Skip if already multi-line
            bool all_same_line = true;
            for (int k = close_idx + 1; k < else_idx; k++) {
                if (tokens[k].out_line != if_line) {
                    all_same_line = false; break;
                }
            }
            for (int k = false_start; k <= false_end && all_same_line; k++) {
                if (tokens[k].out_line != if_line) {
                    all_same_line = false; break;
                }
            }
            if (!all_same_line) continue;

            // Restructure to multi-line braced form
            int if_level = tokens[ii].nesting_level;
            int body_level = if_level + 1;

            // Find prefix and suffix
            std::vector<int> line_toks;
            for (int k = 0; k < n; k++) {
                if (tokens[k].out_line == if_line) line_toks.push_back(k);
            }
            std::vector<int> prefix_idx, suffix_idx;
            for (int k : line_toks) {
                if (k < ii) prefix_idx.push_back(k);
                if (k > false_end) suffix_idx.push_back(k);
            }

            bool has_comma_before = !prefix_idx.empty() &&
                tokens[prefix_idx.back()].token == "','";

            int lines_needed = 5;

            // Shift later
            for (int k = 0; k < n; k++) {
                if (tokens[k].out_line > if_line)
                    tokens[k].out_line += lines_needed;
            }

            int base_line;
            if (has_comma_before) {
                base_line = if_line + 1;
            } else {
                base_line = if_line;
                for (int k = 0; k < n; k++) {
                    if (tokens[k].out_line > if_line)
                        tokens[k].out_line -= 1;
                }
            }

            // if (cond) on base_line
            tokens[ii].out_line = base_line;
            for (int k = open_idx; k <= close_idx; k++)
                tokens[k].out_line = base_line;

            // true expr
            for (int k = close_idx + 1; k < else_idx; k++) {
                tokens[k].out_line = base_line + 1;
                tokens[k].nesting_level = body_level;
            }

            // else
            tokens[else_idx].out_line = base_line + 2;

            // false expr
            for (int k = false_start; k <= false_end; k++) {
                tokens[k].out_line = base_line + 3;
                tokens[k].nesting_level = body_level;
            }

            // suffix
            for (int k : suffix_idx)
                tokens[k].out_line = base_line + 4;

            // Insert braces
            tokens.push_back(make_token("'{'", "{", base_line,
                tokens[close_idx].out_order + 0.5));
            tokens.push_back(make_token("'}'", "}", base_line + 2,
                tokens[else_idx].out_order - 0.5));
            tokens.push_back(make_token("'{'", "{", base_line + 2,
                tokens[else_idx].out_order + 0.5));
            int false_first_order = (false_start < n) ?
                tokens[false_start].out_order - 0.5 : 0.5;
            tokens.push_back(make_token("'}'", "}", base_line + 4,
                false_first_order));

            recompute_nesting(tokens);
            reorder_tokens(tokens);
            changed = true;
            break;
        }
    }
}

void reformat_inline_if(std::vector<Token>& tokens, const FormatOptions& opts) {
    int line_limit = opts.expand_if ? 0 : opts.line_limit;
    bool changed = true;
    int max_iter = 200;

    while (changed && max_iter > 0) {
        max_iter--;
        changed = false;
        reorder_tokens(tokens);
        int n = static_cast<int>(tokens.size());

        for (int ai = 0; ai < n; ai++) {
            if (tokens[ai].token != "LEFT_ASSIGN" &&
                tokens[ai].token != "EQ_ASSIGN")
                continue;

            int assign_line = tokens[ai].out_line;

            // Find IF after assignment
            int if_idx = -1;
            bool found_function = false;
            for (int j = ai + 1; j < n; j++) {
                if (tokens[j].out_line != assign_line) break;
                const std::string& jt = tokens[j].token;
                if (jt == "FUNCTION") { found_function = true; break; }
                if (tok_in(jt, {"';'", "'{'", "'}'", "'['", "LBB",
                                "LEFT_ASSIGN", "EQ_ASSIGN"}))
                    break;
                if (jt == "IF") { if_idx = j; break; }
            }
            if (found_function || if_idx < 0) continue;

            if (tokens[ai].paren_depth > 0) continue;

            // Skip if preceded by ELSE
            bool has_else_before = false;
            for (int k = 0; k < n; k++) {
                if (tokens[k].out_line == assign_line &&
                    tokens[k].out_order < tokens[ai].out_order &&
                    tokens[k].token == "ELSE") {
                    has_else_before = true; break;
                }
            }
            if (has_else_before) continue;

            // Check paren balance between assign and IF
            int paren_bal = 0;
            for (int j = ai + 1; j < if_idx; j++) {
                if (tokens[j].token == "'('") paren_bal++;
                if (tokens[j].token == "')'") paren_bal--;
            }
            if (paren_bal > 0) continue;

            // Check line width
            if (line_limit > 0 &&
                ast_line_width(tokens, assign_line, opts.indent_str,
                               opts.function_space) <= line_limit)
                continue;

            // Find condition
            int open_idx = if_idx + 1;
            if (open_idx >= n || tokens[open_idx].token != "'('") continue;
            int close_idx = find_matching_paren(tokens, open_idx);
            if (close_idx < 0) continue;
            if (close_idx + 1 >= n ||
                tokens[close_idx + 1].token == "'{'")
                continue;

            int else_idx = find_matching_else(tokens, close_idx);
            if (else_idx < 0) continue;

            if (close_idx + 1 >= else_idx) continue;

            int false_start = else_idx + 1;
            if (false_start >= n || tokens[false_start].token == "'{'")
                continue;

            int false_end = find_false_end(tokens, false_start);
            if (false_end < false_start) continue;

            // Skip complex
            bool complex = false;
            for (int k = close_idx + 1; k < else_idx; k++) {
                if (tokens[k].token == "FUNCTION" ||
                    tokens[k].token == "COMMENT") {
                    complex = true; break;
                }
            }
            for (int k = false_start; k <= false_end && !complex; k++) {
                if (tokens[k].token == "FUNCTION" ||
                    tokens[k].token == "COMMENT") {
                    complex = true; break;
                }
            }
            if (complex) continue;

            // Skip multi-line when line_limit > 0
            bool spans_lines = false;
            for (int k = close_idx + 1; k < else_idx; k++) {
                if (tokens[k].out_line != assign_line) {
                    spans_lines = true; break;
                }
            }
            for (int k = false_start; k <= false_end && !spans_lines; k++) {
                if (tokens[k].out_line != assign_line) {
                    spans_lines = true; break;
                }
            }
            if (spans_lines && line_limit > 0) continue;

            // Skip chained else-if
            if (tokens[false_start].token == "IF") continue;

            // Var tokens before assignment
            std::vector<int> line_toks;
            for (int k = 0; k < n; k++) {
                if (tokens[k].out_line == assign_line)
                    line_toks.push_back(k);
            }
            std::vector<int> var_idx;
            for (int k : line_toks) {
                if (k < ai) var_idx.push_back(k);
            }
            if (var_idx.empty()) continue;

            // Skip if var has control flow
            bool var_complex = false;
            for (int k : var_idx) {
                if (tok_in(tokens[k].token,
                           {"IF", "FOR", "WHILE", "REPEAT", "ELSE",
                            "'{'", "'}'", "FUNCTION"})) {
                    var_complex = true; break;
                }
            }
            if (var_complex) continue;

            // Trailing tokens
            std::vector<int> trailing_idx;
            for (int k : line_toks) {
                if (k > false_end) trailing_idx.push_back(k);
            }

            int base_level = tokens[ai].nesting_level;
            int body_level = base_level + 1;
            int lines_needed = 4 + (trailing_idx.empty() ? 0 : 1);

            // Shift later
            for (int k = 0; k < n; k++) {
                if (tokens[k].out_line > assign_line)
                    tokens[k].out_line += lines_needed;
            }

            int base_line = assign_line;

            // Place if (cond) on base_line
            tokens[if_idx].out_line = base_line;
            tokens[if_idx].nesting_level = base_level;
            for (int k = open_idx; k <= close_idx; k++)
                tokens[k].out_line = base_line;

            // True expr
            for (int k = close_idx + 1; k < else_idx; k++) {
                tokens[k].out_line = base_line + 1;
                tokens[k].nesting_level = body_level;
            }

            // Else
            tokens[else_idx].out_line = base_line + 2;

            // False expr
            for (int k = false_start; k <= false_end; k++) {
                tokens[k].out_line = base_line + 3;
                tokens[k].nesting_level = body_level;
            }

            // Trailing
            for (int k : trailing_idx)
                tokens[k].out_line = base_line + 4;

            // Move var+assign to true branch, duplicate for false
            std::vector<int> var_assign_idx = var_idx;
            var_assign_idx.push_back(ai);
            for (int k : var_assign_idx) {
                tokens[k].out_line = base_line + 1;
                tokens[k].nesting_level = body_level;
            }
            // Set out_order for var+assign before true expr tokens
            double order = 0.01;
            for (int k : var_assign_idx) {
                tokens[k].out_order = order;
                order += 0.01;
            }

            // Duplicate var+assign for false branch
            for (int k : var_assign_idx) {
                Token dup = tokens[k];
                dup.out_line = base_line + 3;
                dup.out_order = order;
                order += 0.01;
                if (dup.token == "EQ_ASSIGN") {
                    dup.token = "LEFT_ASSIGN";
                    dup.out_text = "<-";
                }
                tokens.push_back(dup);
            }

            // Insert braces
            double max_cond_order = 0;
            for (int k = 0; k < static_cast<int>(tokens.size()); k++) {
                if (tokens[k].out_line == base_line)
                    max_cond_order = std::max(max_cond_order,
                                              tokens[k].out_order);
            }
            double else_order = tokens[else_idx].out_order;

            tokens.push_back(make_token("'{'", "{", base_line,
                max_cond_order + 1));
            tokens.push_back(make_token("'}'", "}", base_line + 2, 0.001));
            tokens.push_back(make_token("'{'", "{", base_line + 2,
                else_order + 0.5));
            tokens.push_back(make_token("'}'", "}", base_line + 4, 0.001));

            recompute_nesting(tokens);
            reorder_tokens(tokens);
            changed = true;
            break;
        }
    }
}
