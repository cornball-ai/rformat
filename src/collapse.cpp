#include "token.h"

void collapse_calls(std::vector<Token>& tokens, const FormatOptions& opts) {
    bool changed = true;
    int max_iter = 10000;

    while (changed && max_iter > 0) {
        max_iter--;
        changed = false;

        reorder_tokens(tokens);
        int n = static_cast<int>(tokens.size());
        LineIndex lidx;
        lidx.build(tokens);

        for (int ci = 0; ci < n; ci++) {
            if (!tok_in(tokens[ci].token,
                        {"SYMBOL_FUNCTION_CALL", "IF", "FOR", "WHILE"}))
                continue;

            int open_idx = ci + 1;
            if (open_idx >= n || tokens[open_idx].token != "'('") continue;

            int open_line = tokens[open_idx].out_line;
            int close_idx = find_matching_paren(tokens, open_idx);
            if (close_idx < 0) continue;

            int close_line = tokens[close_idx].out_line;
            if (close_line == open_line) continue;

            // Skip if group contains comments or braces
            bool has_comment = false, has_braces = false;
            for (int k = open_idx; k <= close_idx; k++) {
                if (tokens[k].token == "COMMENT") has_comment = true;
                if (tokens[k].token == "'{'" || tokens[k].token == "'}'")
                    has_braces = true;
            }
            if (has_comment || has_braces) continue;

            // Collect call range and suffix tokens
            // Suffix: tokens after ) on close_line
            const std::vector<int>& close_line_toks = lidx.get(close_line);
            std::vector<int> suffix_idx;
            for (int k : close_line_toks) {
                if (tokens[k].out_order > tokens[close_idx].out_order) {
                    suffix_idx.push_back(k);
                }
            }

            // Filter suffix: keep only continuation tokens
            if (!suffix_idx.empty()) {
                int keep_count = 0;
                int si = 0;
                int ns = static_cast<int>(suffix_idx.size());
                while (si < ns) {
                    const std::string& st = tokens[suffix_idx[si]].token;
                    if (tok_in(st, {"')'", "']'", "']]'", "','", "';'",
                                    "'{'", "COMMENT", "'+'", "'-'", "'*'",
                                    "'/'", "'?'", "PIPE", "SPECIAL", "OR2",
                                    "AND2", "OR", "AND", "'~'", "EQ", "NE",
                                    "GE", "LE", "GT", "LT", "LEFT_ASSIGN",
                                    "EQ_ASSIGN", "RIGHT_ASSIGN"})) {
                        keep_count = si + 1;
                        si++;
                    } else if (st == "'$'" || st == "'@'") {
                        keep_count = std::min(si + 2, ns);
                        si += 2;
                    } else if (st == "'('") {
                        int call_depth = 1;
                        si++;
                        while (si < ns && call_depth > 0) {
                            if (tokens[suffix_idx[si]].token == "'('")
                                call_depth++;
                            if (tokens[suffix_idx[si]].token == "')'")
                                call_depth--;
                            si++;
                        }
                        keep_count = si;
                    } else if (st == "'['" || st == "LBB") {
                        int idx_depth = 1;
                        si++;
                        while (si < ns && idx_depth > 0) {
                            const std::string& ist =
                                tokens[suffix_idx[si]].token;
                            if (ist == "'['" || ist == "LBB") idx_depth++;
                            if (ist == "']'") idx_depth--;
                            if (ist == "']]'") idx_depth -= 2;
                            si++;
                        }
                        keep_count = si;
                    } else {
                        break;
                    }
                }
                if (keep_count < ns) {
                    suffix_idx.resize(keep_count);
                }
            }

            // Remaining tokens on close_line (not in call or suffix)
            std::vector<int> remaining_close;
            for (int k : close_line_toks) {
                if (tokens[k].out_order <= tokens[close_idx].out_order)
                    continue;
                // Check not in suffix
                bool in_suffix = false;
                for (int s : suffix_idx) {
                    if (k == s) { in_suffix = true; break; }
                }
                if (!in_suffix) remaining_close.push_back(k);
            }

            // Check fit with remaining
            if (!remaining_close.empty()) {
                std::vector<int> all_check;
                for (int k = ci; k <= close_idx; k++) all_check.push_back(k);
                for (int s : suffix_idx) all_check.push_back(s);
                for (int r : remaining_close) all_check.push_back(r);

                // Save and test
                std::vector<int> saved(all_check.size());
                for (size_t j = 0; j < all_check.size(); j++) {
                    saved[j] = tokens[all_check[j]].out_line;
                    tokens[all_check[j]].out_line = open_line;
                }
                // Rebuild index for the trial width check
                LineIndex trial;
                trial.build(tokens);
                bool too_wide = trial.width(tokens, open_line,
                    opts.indent_str, opts.function_space) > opts.line_limit;
                for (size_t j = 0; j < all_check.size(); j++) {
                    tokens[all_check[j]].out_line = saved[j];
                }
                if (too_wide) continue;
            }

            // Move all tokens to open_line
            std::vector<int> all_move;
            for (int k = ci; k <= close_idx; k++) all_move.push_back(k);
            for (int s : suffix_idx) all_move.push_back(s);
            for (int r : remaining_close) all_move.push_back(r);

            std::vector<int> saved_lines(all_move.size());
            for (size_t j = 0; j < all_move.size(); j++) {
                saved_lines[j] = tokens[all_move[j]].out_line;
                tokens[all_move[j]].out_line = open_line;
            }

            // Never collapse to overlong lines
            LineIndex check;
            check.build(tokens);
            if (check.width(tokens, open_line, opts.indent_str,
                            opts.function_space) > opts.line_limit) {
                for (size_t j = 0; j < all_move.size(); j++) {
                    tokens[all_move[j]].out_line = saved_lines[j];
                }
                continue;
            }

            // Handle remaining tokens on close_line
            std::vector<int> still_on_close;
            for (int k : close_line_toks) {
                if (tokens[k].out_line == close_line) {
                    still_on_close.push_back(k);
                }
            }
            int lines_freed;
            if (!still_on_close.empty()) {
                for (int k : still_on_close) {
                    tokens[k].out_line = open_line + 1;
                }
                lines_freed = close_line - open_line - 1;
            } else {
                lines_freed = close_line - open_line;
            }
            if (lines_freed > 0) {
                for (int k = 0; k < n; k++) {
                    if (tokens[k].out_line > close_line) {
                        tokens[k].out_line -= lines_freed;
                    }
                }
            }

            reorder_tokens(tokens);
            changed = true;
            break;
        }
    }
}
