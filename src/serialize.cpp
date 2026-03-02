#include "token.h"
#include <sstream>
#include <map>
#include <cstring>

std::string serialize_tokens(std::vector<Token>& tokens,
                             const FormatOptions& opts) {
    if (tokens.empty()) return "\n";

    sort_tokens(tokens);

    // Group tokens by out_line
    std::map<int, std::vector<int>> lines_out;
    for (int i = 0; i < static_cast<int>(tokens.size()); i++) {
        lines_out[tokens[i].out_line].push_back(i);
    }

    // Paren-alignment state
    std::vector<int> call_paren_stack;
    std::vector<bool> funcdef_paren_stack;
    std::vector<int> brace_at_call;
    int cur_brace_depth = 0;

    std::vector<std::string> result_lines;
    int prev_line_num = 0;

    for (auto& [ln, idx] : lines_out) {
        // Insert blank lines for gaps
        if (prev_line_num > 0 && ln > prev_line_num + 1) {
            for (int gap = 0; gap < ln - prev_line_num - 1; gap++) {
                result_lines.push_back("");
            }
        }
        prev_line_num = ln;

        // Compute indent
        int first_level = token_indent_level(tokens, idx[0]);
        std::string depth_prefix;
        for (int d = 0; d < first_level; d++) {
            depth_prefix += opts.indent_str;
        }

        // Paren-aligned continuation
        std::string line_prefix = depth_prefix;
        bool has_active_paren = false;
        for (int cp : call_paren_stack) {
            if (cp > 0) { has_active_paren = true; break; }
        }
        if (has_active_paren &&
            (opts.wrap == "paren" ||
             std::any_of(funcdef_paren_stack.begin(),
                         funcdef_paren_stack.end(),
                         [](bool b) { return b; }))) {
            bool inside_brace = !brace_at_call.empty() &&
                                cur_brace_depth > brace_at_call.back();
            if (!inside_brace) {
                // Find last non-zero open col
                int paren_col = 0;
                for (int cp : call_paren_stack) {
                    if (cp > 0) paren_col = cp;
                }
                if (paren_col > 0) {
                    const std::string& first_tok = tokens[idx[0]].token;
                    if (!tok_in(first_tok, {"')'", "']'", "']]'", "IF",
                                            "FOR", "WHILE", "REPEAT",
                                            "ELSE", "'}'"})) {
                        if (paren_col <= opts.line_limit / 2) {
                            std::string paren_prefix(paren_col, ' ');
                            if (paren_prefix.size() > depth_prefix.size()) {
                                line_prefix = paren_prefix;
                            }
                        }
                    }
                }
            }
        }

        // Build line content
        std::string content;
        const Token* prev = nullptr;
        const Token* prev_prev = nullptr;
        for (int i : idx) {
            const Token& tok = tokens[i];
            if (prev != nullptr &&
                needs_space(*prev, tok, prev_prev, opts.function_space)) {
                content += " ";
            }
            content += tok.out_text;
            prev_prev = prev;
            prev = &tok;
        }

        std::string full_line = line_prefix + content;
        // Trim trailing whitespace
        size_t end = full_line.find_last_not_of(" \t");
        if (end != std::string::npos) {
            full_line = full_line.substr(0, end + 1);
        } else {
            full_line.clear();
        }
        result_lines.push_back(full_line);

        // Account for multi-line tokens (strings with embedded newlines)
        int extra_newlines = 0;
        for (int i : idx) {
            for (char c : tokens[i].out_text) {
                if (c == '\n') extra_newlines++;
            }
        }
        if (extra_newlines > 0) {
            prev_line_num += extra_newlines;
        }

        // Update call-paren stack
        int prefix_len = static_cast<int>(line_prefix.size());
        int pos = prefix_len + 1; // 1-based position
        prev = nullptr;
        prev_prev = nullptr;
        int li_idx = 0; // index into current line's tokens
        for (int i : idx) {
            const Token& tok = tokens[i];
            if (prev != nullptr &&
                needs_space(*prev, tok, prev_prev, opts.function_space)) {
                pos += 1;
            }
            const std::string& tt = tok.token;
            if (tt == "'('") {
                bool is_call = false;
                bool is_funcdef = false;
                if (li_idx > 0) {
                    int prev_i = idx[li_idx - 1];
                    is_call = tokens[prev_i].token == "SYMBOL_FUNCTION_CALL";
                    is_funcdef = tokens[prev_i].token == "FUNCTION";
                }
                if (!is_call && !is_funcdef && li_idx == 0) {
                    // Check last token from previous line
                    // Find previous line
                    auto it = lines_out.find(ln);
                    if (it != lines_out.begin()) {
                        auto prev_it = std::prev(it);
                        if (!prev_it->second.empty()) {
                            int prev_idx = prev_it->second.back();
                            is_call = tokens[prev_idx].token ==
                                      "SYMBOL_FUNCTION_CALL";
                            is_funcdef = tokens[prev_idx].token == "FUNCTION";
                        }
                    }
                }
                int paren_pos;
                if (is_call) {
                    paren_pos = pos;
                } else if (is_funcdef) {
                    paren_pos = (opts.wrap == "fixed") ? 8 : pos;
                } else {
                    paren_pos = 0;
                }
                call_paren_stack.push_back(paren_pos);
                funcdef_paren_stack.push_back(is_funcdef);
                brace_at_call.push_back(cur_brace_depth);
            } else if (tt == "')'") {
                if (!call_paren_stack.empty()) {
                    call_paren_stack.pop_back();
                    funcdef_paren_stack.pop_back();
                    brace_at_call.pop_back();
                }
            } else if (tt == "'{'") {
                cur_brace_depth++;
            } else if (tt == "'}'") {
                cur_brace_depth = std::max(0, cur_brace_depth - 1);
            }
            pos += static_cast<int>(tok.out_text.size());
            prev_prev = prev;
            prev = &tok;
            li_idx++;
        }
    }

    // Join lines
    std::string result;
    for (size_t i = 0; i < result_lines.size(); i++) {
        if (i > 0) result += "\n";
        result += result_lines[i];
    }
    if (!result.empty() && result.back() != '\n') {
        result += "\n";
    }
    return result;
}
