#include "token.h"
#include <algorithm>
#include <cstring>

void restore_truncated_str_const(std::vector<Token>& tokens,
                                 const std::vector<std::string>& orig_lines) {
    for (auto& tok : tokens) {
        if (tok.token != "STR_CONST") continue;
        // Check for truncated marker: "[N chars quoted..."
        if (tok.text.size() < 2 || tok.text[0] != '[') continue;
        bool is_truncated = false;
        for (size_t i = 1; i < tok.text.size(); i++) {
            if (tok.text[i] >= '0' && tok.text[i] <= '9') continue;
            if (tok.text.substr(i, 13) == " chars quoted") {
                is_truncated = true;
            }
            break;
        }
        if (!is_truncated) continue;

        int l1 = tok.line1 - 1; // 0-based line index
        int l2 = tok.line2 - 1;
        if (l1 < 0 || l1 >= static_cast<int>(orig_lines.size())) continue;
        if (l2 < 0 || l2 >= static_cast<int>(orig_lines.size())) continue;

        if (l1 == l2) {
            int c1 = col_to_charpos(orig_lines[l1], tok.col1) - 1;
            int c2 = col_to_charpos(orig_lines[l1], tok.col2);
            tok.text = orig_lines[l1].substr(c1, c2 - c1);
        } else {
            int c1 = col_to_charpos(orig_lines[l1], tok.col1) - 1;
            std::string result = orig_lines[l1].substr(c1);
            for (int ln = l1 + 1; ln < l2; ln++) {
                result += "\n" + orig_lines[ln];
            }
            int c2 = col_to_charpos(orig_lines[l2], tok.col2);
            result += "\n" + orig_lines[l2].substr(0, c2);
            tok.text = result;
        }
    }
}

// Shared nesting walk used by both enrich_terminals and recompute_nesting
static void compute_nesting_walk(std::vector<Token>& tokens) {
    int n = static_cast<int>(tokens.size());
    if (n == 0) return;

    int brace_depth = 0;
    int paren_depth = 0;
    std::vector<int> pab_stack;

    for (int i = 0; i < n; i++) {
        int cur_pab = pab_stack.empty() ? 0 : pab_stack.back();
        tokens[i].brace_depth = brace_depth;
        tokens[i].paren_depth = paren_depth;
        tokens[i].pab = cur_pab;

        const std::string& tok = tokens[i].token;
        if (tok == "'{'") {
            bool is_ctrl = false;
            if (i >= 1) {
                const std::string& pt = tokens[i - 1].token;
                if (pt == "ELSE" || pt == "REPEAT") {
                    is_ctrl = true;
                } else if (pt == "')'") {
                    int pd2 = 1;
                    int k = i - 2;
                    while (k >= 0 && pd2 > 0) {
                        if (tokens[k].token == "')'") pd2++;
                        if (tokens[k].token == "'('") pd2--;
                        if (pd2 > 0) k--;
                    }
                    if (k >= 1 && tok_in(tokens[k - 1].token,
                                         {"IF", "FOR", "WHILE"})) {
                        is_ctrl = true;
                    }
                }
            }
            if (is_ctrl) {
                int enc_pab = pab_stack.empty() ? 0 : pab_stack.back();
                pab_stack.push_back(enc_pab);
            } else {
                pab_stack.push_back(paren_depth);
            }
            brace_depth++;
        } else if (tok == "'}'") {
            brace_depth = std::max(0, brace_depth - 1);
            if (!pab_stack.empty()) pab_stack.pop_back();
        } else if (tok == "'('" || tok == "'['") {
            paren_depth++;
        } else if (tok == "LBB") {
            paren_depth += 2;
        } else if (tok == "')'" || tok == "']'") {
            paren_depth = std::max(0, paren_depth - 1);
        } else if (tok == "']]'") {
            paren_depth = std::max(0, paren_depth - 2);
        }
    }

    // Compute nesting_level
    for (auto& t : tokens) {
        t.nesting_level = t.brace_depth +
                          std::max(0, t.paren_depth - t.pab);
    }
}

void enrich_terminals(std::vector<Token>& tokens) {
    if (tokens.empty()) return;

    // Sort by source position
    std::sort(tokens.begin(), tokens.end(),
              [](const Token& a, const Token& b) {
                  if (a.line1 != b.line1) return a.line1 < b.line1;
                  return a.col1 < b.col1;
              });

    int n = static_cast<int>(tokens.size());

    // Initialize output metadata
    for (int i = 0; i < n; i++) {
        tokens[i].out_line = tokens[i].line1;
        tokens[i].out_order = static_cast<double>(i + 1);
        tokens[i].out_text = tokens[i].text;
        // Convert = assignment to <-
        if (tokens[i].token == "EQ_ASSIGN") {
            tokens[i].out_text = "<-";
        }
    }

    compute_nesting_walk(tokens);
}

void recompute_nesting(std::vector<Token>& tokens) {
    if (tokens.empty()) return;
    sort_tokens(tokens);
    compute_nesting_walk(tokens);
}

void renumber_lines(std::vector<Token>& tokens) {
    sort_tokens(tokens);
    if (tokens.empty()) return;

    // Collect unique out_line values in order
    std::vector<int> unique_lines;
    for (const auto& t : tokens) {
        if (unique_lines.empty() || unique_lines.back() != t.out_line) {
            unique_lines.push_back(t.out_line);
        }
    }

    // Build mapping: old_line -> new_line (1-based sequential)
    // Use a scan approach since unique_lines is sorted
    size_t ui = 0;
    for (auto& t : tokens) {
        while (ui < unique_lines.size() && unique_lines[ui] < t.out_line) {
            ui++;
        }
        t.out_line = static_cast<int>(ui + 1);
    }
}
