#include "token.h"
#include <set>

void wrap_long_operators(std::vector<Token>& tokens, const FormatOptions& opts) {
    bool changed = true;
    int max_iter = 100;

    while (changed && max_iter > 0) {
        max_iter--;
        changed = false;
        reorder_tokens(tokens);
        int n = static_cast<int>(tokens.size());
        LineIndex lidx;
        lidx.build(tokens);

        // Collect unique out_lines
        std::set<int> out_lines_set;
        for (const auto& t : tokens) out_lines_set.insert(t.out_line);

        for (int ln : out_lines_set) {
            int width = lidx.width(tokens, ln, opts.indent_str,
                                   opts.function_space);
            if (width <= opts.line_limit) continue;

            const std::vector<int>& idx = lidx.get(ln);
            if (idx.empty()) continue;

            // Skip semicolons
            bool has_semi = false;
            for (int i : idx) {
                if (tokens[i].token == "';'") { has_semi = true; break; }
            }
            if (has_semi) continue;

            int start_paren = tokens[idx[0]].paren_depth;
            int best_break = -1;
            int pos = static_cast<int>(opts.indent_str.size()) *
                      token_indent_level(tokens, idx[0]);
            const Token* prev = nullptr;
            const Token* prev_prev = nullptr;
            int cur_paren = start_paren;

            for (size_t j = 0; j < idx.size(); j++) {
                const Token& tok = tokens[idx[j]];
                if (prev != nullptr &&
                    needs_space(*prev, tok, prev_prev, opts.function_space)) {
                    pos += 1;
                }
                if (tok.token == "'('" || tok.token == "'['") cur_paren++;
                else if (tok.token == "LBB") cur_paren += 2;
                else if (tok.token == "')'" || tok.token == "']'") cur_paren--;
                else if (tok.token == "']]'") cur_paren -= 2;

                int end_pos = pos + static_cast<int>(tok.out_text.size());
                if (tok_in(tok.token, {"OR2", "AND2", "OR", "AND"}) &&
                    cur_paren <= start_paren + 1 &&
                    end_pos <= opts.line_limit) {
                    best_break = static_cast<int>(j);
                }
                pos = end_pos;
                prev_prev = prev;
                prev = &tok;
            }

            if (best_break < 0) continue;

            int new_line = ln + 1;
            // Shift later lines
            for (int k = 0; k < n; k++) {
                if (tokens[k].out_line > ln) tokens[k].out_line++;
            }
            // Move tokens after break
            for (size_t j = best_break + 1; j < idx.size(); j++) {
                tokens[idx[j]].out_line = new_line;
            }

            reorder_tokens(tokens);
            changed = true;
            break;
        }
    }
}

void wrap_long_calls(std::vector<Token>& tokens, const FormatOptions& opts) {
    bool changed = true;
    int max_iter = 100;

    while (changed && max_iter > 0) {
        max_iter--;
        changed = false;
        reorder_tokens(tokens);
        int n = static_cast<int>(tokens.size());
        LineIndex lidx;
        lidx.build(tokens);

        for (int ci = 0; ci < n; ci++) {
            if (tokens[ci].token != "SYMBOL_FUNCTION_CALL") continue;

            int open_idx = ci + 1;
            if (open_idx >= n || tokens[open_idx].token != "'('") continue;

            int close_idx = find_matching_paren(tokens, open_idx);
            if (close_idx < 0) continue;

            int call_line = tokens[ci].out_line;
            bool already_wrapped =
                (tokens[close_idx].out_line != call_line);
            // Re-pack already-wrapped calls so the first arg lands on
            // the call line. A call that's already single-line and
            // fits within the limit needs no work.
            if (!already_wrapped &&
                lidx.width(tokens, call_line, opts.indent_str,
                           opts.function_space) <= opts.line_limit)
                continue;


            // Skip calls with braces or comments. Comments need their
            // own line, so collapsing/re-packing would corrupt them;
            // collapse_calls has the same guard.
            bool has_braces = false;
            bool has_comment = false;
            for (int k = open_idx; k <= close_idx; k++) {
                if (tokens[k].token == "'{'" || tokens[k].token == "'}'") {
                    has_braces = true;
                }
                if (tokens[k].token == "COMMENT") {
                    has_comment = true;
                }
                if (has_braces && has_comment) break;
            }
            if (has_braces || has_comment) continue;

            // Skip semicolons on line
            const std::vector<int>& line_idx = lidx.get(call_line);
            bool has_semi = false;
            for (int k : line_idx) {
                if (tokens[k].token == "';'") { has_semi = true; break; }
            }
            if (has_semi) continue;

            // Skip inner calls when outer is wrappable
            double func_order = tokens[ci].out_order;
            int pd_before = 0;
            for (int k : line_idx) {
                if (tokens[k].out_order >= func_order) break;
                if (tokens[k].token == "'('") pd_before++;
                if (tokens[k].token == "')'") pd_before--;
            }
            if (pd_before > 0) {
                // Check if outer call has unwrapped commas
                bool skip = false;
                for (int k : line_idx) {
                    if (tokens[k].token != "SYMBOL_FUNCTION_CALL") continue;
                    if (tokens[k].out_order >= func_order) continue;
                    int oc_open = -1;
                    for (int m : line_idx) {
                        if (m > k && tokens[m].token == "'('") {
                            oc_open = m; break;
                        }
                    }
                    if (oc_open < 0) continue;
                    int oc_close = find_matching_paren(tokens, oc_open);
                    if (oc_close < 0 ||
                        tokens[oc_close].out_line != call_line)
                        continue;
                    int d2 = 0;
                    for (int ki = oc_open + 1; ki < oc_close; ki++) {
                        if (tokens[ki].token == "'('") d2++;
                        if (tokens[ki].token == "')'") d2--;
                        if (tokens[ki].token == "','") {
                            if (d2 == 0) { skip = true; break; }
                        }
                    }
                    if (skip) break;
                }
                if (skip) continue;
            }

            // Need depth-0 commas
            bool has_comma = false;
            int d2 = 0;
            for (int k = open_idx + 1; k < close_idx; k++) {
                const std::string& tt = tokens[k].token;
                if (tt == "'('" || tt == "'['") d2++;
                else if (tt == "LBB") d2 += 2;
                else if (tt == "')'" || tt == "']'") d2--;
                else if (tt == "']]'") d2 -= 2;
                if (tt == "','") {
                    if (d2 == 0) { has_comma = true; break; }
                }
            }
            if (!has_comma) continue;

            // Collect arg groups and comma indices
            std::vector<std::vector<int>> arg_groups;
            std::vector<int> comma_indices;
            int current_start = open_idx + 1;
            d2 = 0;
            for (int k = open_idx + 1; k < close_idx; k++) {
                const std::string& tt = tokens[k].token;
                if (tt == "'('" || tt == "'['") d2++;
                else if (tt == "LBB") d2 += 2;
                else if (tt == "')'" || tt == "']'") d2--;
                else if (tt == "']]'") d2 -= 2;
                if (tt == "','") {
                    if (d2 == 0) {
                        std::vector<int> group;
                        for (int j = current_start; j < k; j++)
                            group.push_back(j);
                        arg_groups.push_back(group);
                        comma_indices.push_back(k);
                        current_start = k + 1;
                    }
                }
            }
            // Last arg
            {
                std::vector<int> group;
                for (int j = current_start; j < close_idx; j++)
                    group.push_back(j);
                arg_groups.push_back(group);
            }
            if (arg_groups.size() < 2) continue;

            // Compute arg widths
            int indent_size = static_cast<int>(opts.indent_str.size());
            int cont_level = tokens[open_idx].nesting_level + 1;
            int cont_width = cont_level * indent_size;

            if (opts.wrap == "paren") {
                // Compute paren column
                int prefix_w = static_cast<int>(opts.indent_str.size()) *
                               token_indent_level(tokens, line_idx[0]);
                const Token* prev = nullptr;
                const Token* prev_prev = nullptr;
                for (int k : line_idx) {
                    if (k >= ci) break;
                    if (prev != nullptr &&
                        needs_space(*prev, tokens[k], prev_prev,
                                    opts.function_space))
                        prefix_w += 1;
                    prefix_w += static_cast<int>(tokens[k].out_text.size());
                    prev_prev = prev;
                    prev = &tokens[k];
                }
                if (prev != nullptr &&
                    needs_space(*prev, tokens[ci], prev_prev,
                                opts.function_space))
                    prefix_w += 1;
                int paren_col = prefix_w +
                                static_cast<int>(tokens[ci].out_text.size()) +
                                1;
                if (paren_col <= opts.line_limit / 2) {
                    cont_width = paren_col;
                }
            }

            std::vector<int> arg_widths(arg_groups.size());
            for (size_t ai = 0; ai < arg_groups.size(); ai++) {
                int aw = 0;
                const Token* aprev = nullptr;
                const Token* aprev_prev = nullptr;
                for (int aidx : arg_groups[ai]) {
                    if (aprev != nullptr &&
                        needs_space(*aprev, tokens[aidx], aprev_prev,
                                    opts.function_space))
                        aw += 1;
                    aw += static_cast<int>(tokens[aidx].out_text.size());
                    aprev_prev = aprev;
                    aprev = &tokens[aidx];
                }
                arg_widths[ai] = aw;
            }

            // Greedy packing - compute first-line width up to (
            int first_line_w = 0;
            {
                const Token* prev = nullptr;
                const Token* prev_prev = nullptr;
                for (int k : line_idx) {
                    if (k > open_idx) break;
                    if (k == line_idx[0]) {
                        first_line_w = static_cast<int>(opts.indent_str.size()) *
                                       token_indent_level(tokens, k);
                    }
                    if (prev != nullptr &&
                        needs_space(*prev, tokens[k], prev_prev,
                                    opts.function_space))
                        first_line_w += 1;
                    first_line_w +=
                        static_cast<int>(tokens[k].out_text.size());
                    prev_prev = prev;
                    prev = &tokens[k];
                }
            }

            // Pass 1: compute line offsets
            int current_w = first_line_w;
            int lines_inserted = 0;
            bool first_on_line = true;
            std::vector<int> arg_line_offset(arg_groups.size());

            for (size_t ai = 0; ai < arg_groups.size(); ai++) {
                int aw = arg_widths[ai];
                int extra = (ai < arg_groups.size() - 1) ? 2 : 1;
                int space_w = first_on_line ? 0 : 1;
                int test_w = current_w + space_w + aw + extra;

                if (test_w > opts.line_limit && !first_on_line) {
                    lines_inserted++;
                    arg_line_offset[ai] = lines_inserted;
                    current_w = cont_width + aw + extra;
                    first_on_line = current_w > opts.line_limit;
                } else {
                    arg_line_offset[ai] = lines_inserted;
                    current_w = test_w;
                    first_on_line = false;
                }
            }

            // Capture the original close-line before any mutation so we
            // can compute the delta for trailing tokens. For an
            // already-wrapped call, old_close_line > call_line.
            int old_close_line = tokens[close_idx].out_line;
            int new_close_line = call_line + arg_line_offset.back();

            // Already-wrapped calls reach this point even when
            // lines_inserted == 0 (everything fits on one line),
            // because we want to collapse them. For never-wrapped
            // calls, lines_inserted == 0 means no work to do.
            if (lines_inserted == 0 && !already_wrapped) continue;

            // Don't re-pack an already-wrapped call if the first arg
            // alone won't fit on the call line within the limit, or
            // if any resulting line would exceed the limit. Pulling
            // the first arg up only helps when it actually makes the
            // call more compact; for deeply-nested calls with huge
            // args, the greedy packer would otherwise produce
            // grotesque jam-onto-call-line output.
            if (already_wrapped) {
                bool repack_ok = true;
                if (first_line_w + arg_widths[0] +
                    (arg_groups.size() > 1 ? 2 : 1) > opts.line_limit) {
                    repack_ok = false;
                }
                if (repack_ok) {
                    // Re-walk arg_line_offset to check each resulting
                    // line's width.
                    int line_w = first_line_w;
                    int cur_offset = 0;
                    bool first = true;
                    for (size_t ai = 0; ai < arg_groups.size() && repack_ok;
                         ai++) {
                        int extra = (ai < arg_groups.size() - 1) ? 2 : 1;
                        if (arg_line_offset[ai] != cur_offset) {
                            cur_offset = arg_line_offset[ai];
                            line_w = cont_width + arg_widths[ai] + extra;
                            first = false;
                        } else {
                            int space_w = first ? 0 : 1;
                            line_w += space_w + arg_widths[ai] + extra;
                            first = false;
                        }
                        if (line_w > opts.line_limit) repack_ok = false;
                    }
                }
                if (!repack_ok) continue;
            }

            // Pass 2: apply line assignments
            std::set<int> all_call_idx_set;
            for (auto& group : arg_groups)
                for (int k : group) all_call_idx_set.insert(k);
            for (int k : comma_indices) all_call_idx_set.insert(k);
            all_call_idx_set.insert(close_idx);

            // Capture tokens trailing the close paren on its current
            // line BEFORE the shift pass — otherwise we'd pick up
            // tokens that get shifted INTO old_close_line.
            std::vector<int> old_close_trailing;
            if (already_wrapped) {
                for (int k = 0; k < n; k++) {
                    if (tokens[k].out_line == old_close_line &&
                        tokens[k].out_order > tokens[close_idx].out_order &&
                        all_call_idx_set.find(k) == all_call_idx_set.end()) {
                        old_close_trailing.push_back(k);
                    }
                }
            }

            // Shift later tokens by (new_close_line - old_close_line).
            // For never-wrapped this is +lines_inserted; for
            // already-wrapped it can be negative (call now spans fewer
            // lines than before).
            int line_delta = new_close_line - old_close_line;
            if (line_delta != 0) {
                for (int k = 0; k < n; k++) {
                    if (tokens[k].out_line > old_close_line &&
                        all_call_idx_set.find(k) == all_call_idx_set.end()) {
                        tokens[k].out_line += line_delta;
                    }
                }
            }

            // Place args and commas
            for (size_t ai = 0; ai < arg_groups.size(); ai++) {
                int target = call_line + arg_line_offset[ai];
                for (int k : arg_groups[ai]) tokens[k].out_line = target;
                if (ai < comma_indices.size())
                    tokens[comma_indices[ai]].out_line = target;
            }

            // Place ) on same line as last arg
            int last_offset = arg_line_offset.back();
            tokens[close_idx].out_line = call_line + last_offset;

            // Move trailing tokens on the call's first line (stuff
            // after `)` on call_line, for never-wrapped) and on the
            // old close line (for already-wrapped) to the new close
            // line.
            if (last_offset > 0) {
                for (int k : line_idx) {
                    if (tokens[k].out_order > tokens[close_idx].out_order &&
                        all_call_idx_set.find(k) == all_call_idx_set.end()) {
                        tokens[k].out_line = call_line + last_offset;
                    }
                }
            }
            for (int k : old_close_trailing) {
                tokens[k].out_line = new_close_line;
            }

            reorder_tokens(tokens);
            changed = true;
            break;
        }
    }
}
