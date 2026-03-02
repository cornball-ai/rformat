#include "token.h"
#include <set>

int find_bare_body_end(const std::vector<Token>& tokens, int body_start) {
    int n = static_cast<int>(tokens.size());
    int pd = 0, bd = 0, id = 0;
    std::string last_real_tok;
    int i = body_start;

    while (i < n) {
        const std::string& tok = tokens[i].token;
        if (tok == "'('" || tok == "'['") pd++;
        else if (tok == "LBB") pd += 2;
        else if (tok == "')'" || tok == "']'") pd--;
        else if (tok == "']]'") pd -= 2;
        else if (tok == "'{'") bd++;
        else if (tok == "'}'") {
            bd--;
            if (bd < 0) return i - 1;
        } else if (tok == "IF" && pd == 0 && bd == 0) {
            id++;
        } else if (tok == "ELSE" && pd == 0 && bd == 0) {
            if (id > 0) id--;
            else return i - 1;
        }

        if (pd == 0 && bd == 0 && id == 0 && i > body_start &&
            tok != "ELSE") {
            if (i + 1 >= n) return i;

            const std::string& next_tok = tokens[i + 1].token;
            if (next_tok == "ELSE" || next_tok == "'}'") return i;

            if (tokens[i + 1].out_line != tokens[i].out_line) {
                std::string check_tok = (tok == "COMMENT") ?
                    last_real_tok : tok;
                if (!tok_in(check_tok,
                        {"LEFT_ASSIGN", "EQ_ASSIGN", "'+'", "'-'", "'*'",
                         "'/'", "'^'", "'~'", "SPECIAL", "OR2", "AND2",
                         "OR", "AND", "GT", "GE", "LT", "LE", "EQ", "NE",
                         "PIPE", "'$'", "'@'", "ELSE"}) &&
                    !tok_in(next_tok,
                        {"'+'", "'-'", "'*'", "'/'", "'^'", "'~'", "SPECIAL",
                         "OR2", "AND2", "OR", "AND", "GT", "GE", "LT", "LE",
                         "EQ", "NE", "PIPE", "'['", "LBB", "'('", "'$'",
                         "'@'", "'{'"}) &&
                    next_tok != "COMMENT") {
                    return i;
                }
            }
        }
        if (tok != "COMMENT") last_real_tok = tok;
        i++;
    }
    return n - 1;
}

bool body_is_complete(const std::vector<Token>& tokens,
                      int body_start, int body_end) {
    int bal = 0;
    for (int i = body_start; i <= body_end; i++) {
        const std::string& bt = tokens[i].token;
        if (bt == "'('" || bt == "'['") bal++;
        else if (bt == "LBB") bal += 2;
        else if (bt == "')'" || bt == "']'") bal--;
        else if (bt == "']]'") bal -= 2;
    }
    if (bal != 0) return false;

    const std::string& last_tok = tokens[body_end].token;
    if (tok_in(last_tok, {"LEFT_ASSIGN", "EQ_ASSIGN", "RIGHT_ASSIGN",
                           "'+'", "'-'", "'*'", "'/'", "'^'", "'~'",
                           "SPECIAL", "','", "OR2", "AND2", "OR", "AND",
                           "GT", "GE", "LT", "LE", "EQ", "NE"}))
        return false;

    return true;
}

void add_control_braces(std::vector<Token>& tokens, const FormatOptions& opts) {
    std::string mode = opts.control_braces;
    if (mode.empty()) return;
    if (mode == "TRUE" || mode == "true") mode = "single";

    bool changed = true;
    int max_iter = 5000;

    while (changed && max_iter > 0) {
        max_iter--;
        changed = false;
        reorder_tokens(tokens);
        int n = static_cast<int>(tokens.size());

        for (int ci = 0; ci < n; ci++) {
            if (!tok_in(tokens[ci].token,
                        {"IF", "FOR", "WHILE", "REPEAT", "ELSE"}))
                continue;

            const std::string& tok = tokens[ci].token;
            int ctrl_line = tokens[ci].out_line;

            // Find condition close paren
            int cond_close;
            if (tok == "REPEAT" || tok == "ELSE") {
                cond_close = ci;
            } else {
                int open_idx = ci + 1;
                if (open_idx >= n || tokens[open_idx].token != "'('")
                    continue;
                cond_close = find_matching_paren(tokens, open_idx);
                if (cond_close < 0) continue;
            }

            // Find body start (skip comments)
            int body_start = cond_close + 1;
            while (body_start < n && tokens[body_start].token == "COMMENT")
                body_start++;
            if (body_start >= n) continue;

            // Skip else-if chains
            if (tok == "ELSE" && tokens[body_start].token == "IF") continue;

            // Skip expression-context if-else
            if (tok == "IF" || tok == "ELSE") {
                if (tokens[ci].paren_depth > 0) continue;

                int check_idx = ci;
                if (tok == "ELSE") {
                    int parent_if = ci - 1;
                    int bd = 0;
                    while (parent_if >= 0) {
                        if (tokens[parent_if].token == "'}'") bd++;
                        else if (tokens[parent_if].token == "'{'") bd--;
                        if (bd == 0 && tokens[parent_if].token == "IF")
                            break;
                        parent_if--;
                    }
                    check_idx = (parent_if >= 0) ? parent_if : -1;
                }

                if (check_idx >= 0) {
                    int check_line = tokens[check_idx].out_line;
                    bool skip = false;
                    for (int k = check_idx - 1; k >= 0; k--) {
                        if (tokens[k].out_line != check_line) break;
                        if (tok_in(tokens[k].token,
                                   {"LEFT_ASSIGN", "EQ_ASSIGN",
                                    "RIGHT_ASSIGN"})) {
                            skip = true; break;
                        }
                    }
                    if (skip) continue;
                }
            }

            int cond_close_line = tokens[cond_close].out_line;
            int body_start_line = tokens[body_start].out_line;

            if (mode == "single" || mode == "multi") {
                if (tokens[body_start].token == "'{'") continue;

                // Find body end
                int body_end = body_start;
                while (body_end + 1 < n &&
                       tokens[body_end + 1].out_line == body_start_line &&
                       tokens[body_end + 1].token != "COMMENT" &&
                       tokens[body_end + 1].token != "ELSE" &&
                       tokens[body_end + 1].token != "'}'") {
                    body_end++;
                }

                // Skip if more tokens on same line after body
                if (body_end + 1 < n &&
                    tokens[body_end + 1].out_line == body_start_line &&
                    tokens[body_end + 1].token != "ELSE" &&
                    tokens[body_end + 1].token != "'}'" &&
                    tokens[body_end + 1].token != "COMMENT")
                    continue;

                // Skip complex bodies
                bool complex = false;
                for (int k = body_start; k <= body_end; k++) {
                    if (tok_in(tokens[k].token,
                               {"IF", "FOR", "WHILE", "REPEAT", "FUNCTION"})) {
                        complex = true; break;
                    }
                }
                if (complex) continue;

                if (!body_is_complete(tokens, body_start, body_end)) continue;

                bool has_else = body_end + 1 < n &&
                                tokens[body_end + 1].token == "ELSE";

                // Check trailing comment on body line
                bool has_comment_on_line = false;
                for (int k = 0; k < n; k++) {
                    if (tokens[k].token == "COMMENT" &&
                        tokens[k].out_line == body_start_line &&
                        tokens[k].out_order > tokens[body_start].out_order) {
                        has_comment_on_line = true; break;
                    }
                }

                Token open_brace = make_token("'{'", "{", body_start_line,
                    tokens[body_start].out_order - 0.5);
                Token close_brace = make_token("'}'", "}", body_start_line,
                    tokens[body_end].out_order + 0.5);

                if (mode == "multi" ||
                    body_start_line != cond_close_line ||
                    has_comment_on_line) {
                    // Multi-line layout
                    for (int k = 0; k < n; k++) {
                        if (tokens[k].out_line > cond_close_line)
                            tokens[k].out_line += 2;
                    }

                    // Move structural tokens after body
                    for (int k = 0; k < n; k++) {
                        if (tokens[k].out_line == cond_close_line &&
                            tokens[k].out_order > tokens[body_end].out_order &&
                            k < body_start && tokens[k].token != "COMMENT") {
                            // This check is complex; simplified version
                        }
                    }
                    // Move after-body non-comment tokens
                    for (int k = 0; k < n; k++) {
                        if (tokens[k].out_line == cond_close_line &&
                            tokens[k].out_order > tokens[body_end].out_order) {
                            bool in_body = (k >= body_start && k <= body_end);
                            if (!in_body && tokens[k].token != "COMMENT") {
                                tokens[k].out_line = cond_close_line + 2;
                            }
                        }
                    }

                    // Body tokens go to cond_close_line + 1
                    for (int k = body_start; k <= body_end; k++) {
                        tokens[k].out_line = cond_close_line + 1;
                    }

                    // Comments trailing body
                    if (body_start_line == cond_close_line) {
                        for (int k = 0; k < n; k++) {
                            if (tokens[k].token == "COMMENT" &&
                                tokens[k].out_line == cond_close_line &&
                                tokens[k].out_order >
                                    tokens[body_start].out_order) {
                                tokens[k].out_line = cond_close_line + 1;
                            }
                        }
                    }

                    // Move ELSE to close-brace line
                    if (has_else) {
                        tokens[body_end + 1].out_line = cond_close_line + 2;
                    }

                    open_brace.out_line = cond_close_line;
                    open_brace.out_order = tokens[cond_close].out_order + 0.5;
                    close_brace.out_line = cond_close_line + 2;
                    close_brace.out_order =
                        tokens[body_start].out_order - 0.3;
                } else {
                    // Single-line: check fit
                    int w = ast_line_width(tokens, body_start_line,
                                           opts.indent_str,
                                           opts.function_space) + 4;
                    if (w > opts.line_limit) {
                        // Fall back to multi-line
                        for (int k = 0; k < n; k++) {
                            if (tokens[k].out_line > cond_close_line)
                                tokens[k].out_line += 2;
                        }
                        for (int k = 0; k < n; k++) {
                            if (tokens[k].out_line == cond_close_line &&
                                tokens[k].out_order >
                                    tokens[body_end].out_order) {
                                bool in_body =
                                    (k >= body_start && k <= body_end);
                                if (!in_body &&
                                    tokens[k].token != "COMMENT") {
                                    tokens[k].out_line = cond_close_line + 2;
                                }
                            }
                        }
                        for (int k = body_start; k <= body_end; k++)
                            tokens[k].out_line = cond_close_line + 1;
                        open_brace.out_line = cond_close_line;
                        close_brace.out_line = cond_close_line + 2;
                        close_brace.out_order =
                            tokens[body_start].out_order - 0.3;
                    }
                    // else: keep single line (default values)
                }

                tokens.push_back(open_brace);
                tokens.push_back(close_brace);
                recompute_nesting(tokens);
                for (size_t k = 0; k < tokens.size(); k++)
                    tokens[k].out_order = static_cast<double>(k + 1);
                changed = true;
                break;

            } else if (mode == "next_line") {
                if (tokens[body_start].token == "'{'") continue;

                int target_level = tokens[ci].nesting_level + 1;

                if (body_start_line != cond_close_line) {
                    // Body already on different line
                    int body_end = find_bare_body_end(tokens, body_start);
                    if (tokens[body_end].out_line > body_start_line) {
                        // Multi-line bare body - add braces
                        Token ob = make_token("'{'", "{", cond_close_line,
                            tokens[cond_close].out_order + 0.5);
                        Token cb = make_token("'}'", "}",
                            tokens[body_end].out_line + 1,
                            tokens[body_start].out_order - 0.3);

                        int body_end_line = tokens[body_end].out_line;
                        // Collect tokens after body_end on body_end_line
                        // (ELSE + else body) — must move with body
                        std::vector<int> after_body_idx;
                        for (int k = 0; k < n; k++) {
                            if (tokens[k].out_line == body_end_line &&
                                tokens[k].out_order >
                                    tokens[body_end].out_order &&
                                (k < body_start || k > body_end)) {
                                after_body_idx.push_back(k);
                            }
                        }
                        for (int k = 0; k < n; k++) {
                            if (tokens[k].out_line > body_end_line)
                                tokens[k].out_line++;
                        }
                        for (int k : after_body_idx) {
                            tokens[k].out_line = body_end_line + 1;
                        }

                        tokens.push_back(ob);
                        tokens.push_back(cb);
                        recompute_nesting(tokens);
                        for (size_t k = 0; k < tokens.size(); k++)
                            tokens[k].out_order = static_cast<double>(k + 1);
                        changed = true;
                        break;
                    }

                    // Single-line body with ELSE - add braces
                    // (prevents oscillation when wrapping makes
                    // body multi-line on next pass)
                    int body_end_sl = find_bare_body_end(tokens,
                                                         body_start);
                    bool has_else_sl = body_end_sl + 1 < n &&
                        tokens[body_end_sl + 1].token == "ELSE";
                    if (has_else_sl) {
                        Token ob_sl = make_token("'{'", "{",
                            cond_close_line,
                            tokens[cond_close].out_order + 0.5);
                        Token cb_sl = make_token("'}'", "}",
                            body_start_line + 1,
                            tokens[body_start].out_order - 0.3);
                        // Move ELSE + else body on body_start_line
                        std::vector<int> after_body_sl;
                        for (int k = 0; k < n; k++) {
                            if (tokens[k].out_line == body_start_line &&
                                tokens[k].out_order >
                                    tokens[body_end_sl].out_order &&
                                (k < body_start ||
                                 k > body_end_sl)) {
                                after_body_sl.push_back(k);
                            }
                        }
                        for (int k = 0; k < n; k++) {
                            if (tokens[k].out_line > body_start_line)
                                tokens[k].out_line++;
                        }
                        for (int k : after_body_sl) {
                            tokens[k].out_line =
                                body_start_line + 1;
                        }
                        tokens.push_back(ob_sl);
                        tokens.push_back(cb_sl);
                        recompute_nesting(tokens);
                        for (size_t k = 0; k < tokens.size(); k++)
                            tokens[k].out_order =
                                static_cast<double>(k + 1);
                        changed = true;
                        break;
                    }

                    // Single-line body - fix nesting
                    bool nesting_changed = false;
                    for (int k = 0; k < n; k++) {
                        if (tokens[k].out_line == body_start_line &&
                            tokens[k].out_order >=
                                tokens[body_start].out_order &&
                            tokens[k].token != "ELSE" &&
                            tokens[k].token != "'}'") {
                            if (tokens[k].nesting_level != target_level) {
                                tokens[k].nesting_level = target_level;
                                nesting_changed = true;
                            }
                        }
                    }
                    if (nesting_changed) {
                        reorder_tokens(tokens);
                        changed = true;
                        break;
                    }
                    continue;
                }

                // Body on same line - move to next line
                int body_end = body_start;
                while (body_end + 1 < n &&
                       tokens[body_end + 1].out_line == body_start_line &&
                       tokens[body_end + 1].token != "COMMENT" &&
                       tokens[body_end + 1].token != "ELSE" &&
                       tokens[body_end + 1].token != "'}'") {
                    body_end++;
                }

                bool complex = false;
                for (int k = body_start; k <= body_end; k++) {
                    if (tok_in(tokens[k].token,
                               {"IF", "FOR", "WHILE", "REPEAT", "FUNCTION"})) {
                        complex = true; break;
                    }
                }
                if (complex) continue;

                for (int k = 0; k < n; k++) {
                    if (tokens[k].out_line > cond_close_line)
                        tokens[k].out_line++;
                }
                for (int k = body_start; k <= body_end; k++) {
                    tokens[k].out_line = cond_close_line + 1;
                    tokens[k].nesting_level = target_level;
                }

                // Move trailing tokens
                for (int k = 0; k < n; k++) {
                    if (tokens[k].out_line == cond_close_line &&
                        tokens[k].out_order > tokens[body_end].out_order) {
                        tokens[k].out_line = cond_close_line + 1;
                    }
                }

                reorder_tokens(tokens);
                changed = true;
                break;

            } else if (mode == "same_line") {
                // Skip if condition line has trailing comment
                bool has_cond_comment = false;
                for (int k = 0; k < n; k++) {
                    if (tokens[k].out_line == cond_close_line &&
                        tokens[k].token == "COMMENT") {
                        has_cond_comment = true; break;
                    }
                }
                if (has_cond_comment) continue;

                if (tokens[body_start].token == "'{'") {
                    // Strip braces if single-statement body
                    int open_brace_idx = body_start;
                    int close_brace_idx =
                        find_matching_brace(tokens, open_brace_idx);
                    if (close_brace_idx < 0) continue;

                    int inner_start = open_brace_idx + 1;
                    int inner_end = close_brace_idx - 1;
                    if (inner_start > inner_end) continue;

                    // Skip multi-line
                    int inner_line = tokens[inner_start].out_line;
                    bool multi_line = false;
                    for (int k = inner_start; k <= inner_end; k++) {
                        if (tokens[k].out_line != inner_line) {
                            multi_line = true; break;
                        }
                    }
                    if (multi_line) continue;

                    // Skip complex
                    bool complex = false;
                    for (int k = inner_start; k <= inner_end; k++) {
                        if (tok_in(tokens[k].token,
                                   {"IF", "FOR", "WHILE", "REPEAT",
                                    "FUNCTION", "COMMENT", "';'"})) {
                            complex = true; break;
                        }
                    }
                    if (complex) continue;

                    // Skip if has else
                    bool has_else = close_brace_idx + 1 < n &&
                                   tokens[close_brace_idx + 1].token == "ELSE";
                    if (has_else) continue;

                    // Check width
                    int cond_w = ast_line_width(tokens, cond_close_line,
                                               opts.indent_str,
                                               opts.function_space);
                    int body_w = ast_line_width(tokens, inner_line,
                                               opts.indent_str,
                                               opts.function_space);
                    if (cond_w + 1 + body_w > opts.line_limit) continue;

                    // Move inner tokens to condition line
                    for (int k = inner_start; k <= inner_end; k++)
                        tokens[k].out_line = cond_close_line;

                    // Remove brace tokens
                    std::set<int> remove_set;
                    remove_set.insert(open_brace_idx);
                    remove_set.insert(close_brace_idx);
                    std::vector<Token> new_tokens;
                    for (int k = 0; k < n; k++) {
                        if (remove_set.find(k) == remove_set.end())
                            new_tokens.push_back(tokens[k]);
                    }
                    tokens = new_tokens;
                    renumber_lines(tokens);
                    recompute_nesting(tokens);
                    changed = true;
                    break;
                }

                // Bare body on next line
                if (tokens[body_start].out_line != cond_close_line + 1)
                    continue;

                int body_end = body_start;
                while (body_end + 1 < n &&
                       tokens[body_end + 1].out_line == body_start_line &&
                       tokens[body_end + 1].token != "COMMENT" &&
                       tokens[body_end + 1].token != "ELSE" &&
                       tokens[body_end + 1].token != "'}'") {
                    body_end++;
                }

                bool complex = false;
                for (int k = body_start; k <= body_end; k++) {
                    if (tok_in(tokens[k].token,
                               {"IF", "FOR", "WHILE", "REPEAT", "FUNCTION",
                                "COMMENT"})) {
                        complex = true; break;
                    }
                }
                if (complex) continue;

                if (!body_is_complete(tokens, body_start, body_end)) continue;

                int cond_w = ast_line_width(tokens, cond_close_line,
                                           opts.indent_str,
                                           opts.function_space);
                int body_w = ast_line_width(tokens, body_start_line,
                                           opts.indent_str,
                                           opts.function_space);
                if (cond_w + 1 + body_w > opts.line_limit) continue;

                for (int k = body_start; k <= body_end; k++)
                    tokens[k].out_line = cond_close_line;
                renumber_lines(tokens);
                reorder_tokens(tokens);
                changed = true;
                break;
            }
        }
    }
}
