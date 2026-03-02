#include "token.h"
#include <set>

void reformat_function_defs(std::vector<Token>& tokens,
                            const FormatOptions& opts) {
    bool changed = true;
    int max_iter = 200;

    while (changed && max_iter > 0) {
        max_iter--;
        changed = false;
        reorder_tokens(tokens);
        int n = static_cast<int>(tokens.size());

        for (int fi = 0; fi < n; fi++) {
            if (tokens[fi].token != "FUNCTION") continue;

            // Only named function definitions
            int prev_idx = fi - 1;
            while (prev_idx >= 0 && tokens[prev_idx].token == "COMMENT")
                prev_idx--;
            if (prev_idx < 0) continue;
            if (!tok_in(tokens[prev_idx].token,
                        {"LEFT_ASSIGN", "EQ_ASSIGN", "RIGHT_ASSIGN"}))
                continue;

            int open_idx = fi + 1;
            if (open_idx >= n || tokens[open_idx].token != "'('") continue;

            int close_idx = find_matching_paren(tokens, open_idx);
            if (close_idx < 0) continue;

            // Check for { after )
            bool has_brace = close_idx + 1 < n &&
                             tokens[close_idx + 1].token == "'{'";
            int brace_idx = has_brace ? close_idx + 1 : -1;

            int func_line = tokens[fi].out_line;

            // Measure prefix width
            std::vector<int> line_idx;
            for (int k = 0; k < n; k++) {
                if (tokens[k].out_line == func_line) line_idx.push_back(k);
            }

            int prefix_w = static_cast<int>(opts.indent_str.size()) *
                           token_indent_level(tokens, line_idx[0]);
            const Token* prev = nullptr;
            const Token* prev_prev = nullptr;
            for (int k : line_idx) {
                if (k >= fi) break;
                if (prev != nullptr &&
                    needs_space(*prev, tokens[k], prev_prev,
                                opts.function_space))
                    prefix_w += 1;
                prefix_w += static_cast<int>(tokens[k].out_text.size());
                prev_prev = prev;
                prev = &tokens[k];
            }
            if (!line_idx.empty() && prev != nullptr &&
                needs_space(*prev, tokens[fi], prev_prev, opts.function_space))
                prefix_w += 1;

            int func_open_w = opts.function_space ? 10 : 9; // "function(" or "function ("
            int open_col = prefix_w + func_open_w;

            // Collect formal arg groups
            std::vector<std::vector<int>> arg_groups;
            std::vector<int> comma_indices;
            std::vector<int> current_group;
            int formal_depth = 0;

            for (int i = open_idx + 1; i < close_idx; i++) {
                if (tokens[i].token == "COMMENT") continue;
                if (tokens[i].token == "'('") formal_depth++;
                if (tokens[i].token == "')'") formal_depth--;
                if (tokens[i].token == "','" && formal_depth == 0) {
                    arg_groups.push_back(current_group);
                    comma_indices.push_back(i);
                    current_group.clear();
                    continue;
                }
                current_group.push_back(i);
            }
            if (!current_group.empty()) arg_groups.push_back(current_group);
            if (arg_groups.empty()) continue;

            // Measure arg widths
            std::vector<int> arg_widths(arg_groups.size());
            for (size_t ai = 0; ai < arg_groups.size(); ai++) {
                int aw = 0;
                const Token* aprev = nullptr;
                const Token* aprev_prev = nullptr;
                for (int j : arg_groups[ai]) {
                    if (aprev != nullptr &&
                        needs_space(*aprev, tokens[j], aprev_prev,
                                    opts.function_space))
                        aw += 1;
                    aw += static_cast<int>(tokens[j].out_text.size());
                    aprev_prev = aprev;
                    aprev = &tokens[j];
                }
                arg_widths[ai] = aw;
            }

            // Skip comments/braces in formals
            bool has_comment = false, has_braces_inner = false;
            for (int k = open_idx + 1; k < close_idx; k++) {
                if (tokens[k].token == "COMMENT") has_comment = true;
                if (tokens[k].token == "'{'") has_braces_inner = true;
            }
            if (has_comment || has_braces_inner) continue;

            // Single-line width
            int single_w = open_col;
            for (size_t ai = 0; ai < arg_groups.size(); ai++) {
                single_w += arg_widths[ai];
            }
            single_w += static_cast<int>(arg_groups.size() - 1) * 2; // ", "
            single_w += 1; // ")"

            int sig_limit = (has_brace && opts.brace_style == "kr")
                            ? opts.line_limit - 2 : opts.line_limit;

            if (single_w <= sig_limit) {
                // Everything fits on one line
                bool need_change = false;
                for (int k = fi; k <= close_idx; k++) {
                    if (tokens[k].out_line != func_line) {
                        need_change = true; break;
                    }
                }
                if (has_brace && tokens[brace_idx].out_line != func_line)
                    need_change = true;
                if (!need_change) continue;

                for (int k = fi; k <= close_idx; k++)
                    tokens[k].out_line = func_line;
                for (int c : comma_indices)
                    tokens[c].out_line = func_line;
                if (has_brace)
                    tokens[brace_idx].out_line = func_line;

                reorder_tokens(tokens);
                changed = true;
                break;
            } else {
                // Need to wrap
                int cont_width = (opts.wrap == "fixed") ? 8 : open_col;

                int current_w = open_col;
                int lines_inserted = 0;
                bool first_on_line = true;
                std::vector<int> arg_line_offset(arg_groups.size());

                for (size_t ai = 0; ai < arg_groups.size(); ai++) {
                    int aw = arg_widths[ai];
                    int extra = (ai < arg_groups.size() - 1) ? 2 : 1;
                    int test_w = current_w + aw + extra;

                    if (test_w > sig_limit && !first_on_line) {
                        lines_inserted++;
                        arg_line_offset[ai] = lines_inserted;
                        current_w = cont_width + aw + extra;
                        first_on_line = false;
                    } else {
                        arg_line_offset[ai] = lines_inserted;
                        current_w = test_w;
                        first_on_line = false;
                    }
                }

                if (lines_inserted == 0) continue;

                // Collect signature token indices
                std::set<int> sig_set;
                sig_set.insert(fi);
                sig_set.insert(open_idx);
                for (auto& group : arg_groups)
                    for (int k : group) sig_set.insert(k);
                for (int c : comma_indices) sig_set.insert(c);
                sig_set.insert(close_idx);
                if (has_brace) sig_set.insert(brace_idx);

                int old_sig_end = func_line;
                for (int k : sig_set) {
                    old_sig_end = std::max(old_sig_end, tokens[k].out_line);
                }
                int old_sig_lines = old_sig_end - func_line;

                // Move all sig tokens to func_line temporarily
                for (int k : sig_set) tokens[k].out_line = func_line;

                // Adjust later lines
                int net_shift = lines_inserted - old_sig_lines;
                if (net_shift != 0) {
                    for (int k = 0; k < n; k++) {
                        if (tokens[k].out_line > func_line &&
                            sig_set.find(k) == sig_set.end()) {
                            tokens[k].out_line += net_shift;
                        }
                    }
                }

                // Place function + ( on func_line
                tokens[fi].out_line = func_line;
                tokens[open_idx].out_line = func_line;

                // Place args and commas
                for (size_t ai = 0; ai < arg_groups.size(); ai++) {
                    int target = func_line + arg_line_offset[ai];
                    for (int k : arg_groups[ai]) tokens[k].out_line = target;
                    if (ai < comma_indices.size())
                        tokens[comma_indices[ai]].out_line = target;
                }

                // Place )
                int last_line = func_line + arg_line_offset.back();
                tokens[close_idx].out_line = last_line;

                // Place { based on brace style
                if (has_brace) {
                    bool empty_body = brace_idx + 1 < n &&
                                     tokens[brace_idx + 1].token == "'}'";
                    if (opts.brace_style == "kr") {
                        tokens[brace_idx].out_line = last_line;
                        if (empty_body)
                            tokens[brace_idx + 1].out_line = last_line;
                    } else {
                        // Allman
                        std::set<int> excl;
                        excl.insert(brace_idx);
                        if (empty_body) excl.insert(brace_idx + 1);
                        for (int k = 0; k < n; k++) {
                            if (tokens[k].out_line > last_line &&
                                excl.find(k) == excl.end()) {
                                tokens[k].out_line += 1;
                            }
                        }
                        tokens[brace_idx].out_line = last_line + 1;
                        if (empty_body)
                            tokens[brace_idx + 1].out_line = last_line + 1;
                    }
                }

                // Move bare body tokens still on func_line to last_line
                int body_start = has_brace ? brace_idx + 1 : close_idx + 1;
                for (int k = body_start; k < n; k++) {
                    if (sig_set.find(k) != sig_set.end()) continue;
                    if (tokens[k].out_line == func_line) {
                        tokens[k].out_line = last_line;
                    }
                }

                reorder_tokens(tokens);
                changed = true;
                break;
            }
        }
    }
}
