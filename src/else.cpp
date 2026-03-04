#include "token.h"

void join_else_transform(std::vector<Token>& tokens,
                         const FormatOptions& opts) {
    reorder_tokens(tokens);
    int n = static_cast<int>(tokens.size());
    if (n < 2) return;

    LineIndex lidx;
    lidx.build(tokens);

    for (int ei = 0; ei < n; ei++) {
        if (tokens[ei].token != "ELSE") continue;

        int else_line = tokens[ei].out_line;

        // Walk backwards to find preceding non-comment token
        int rbrace_idx = -1;
        bool has_comment = false;
        for (int j = ei - 1; j >= 0; j--) {
            if (tokens[j].token == "COMMENT") {
                has_comment = true;
                continue;
            }
            if (tokens[j].token == "'}'") {
                rbrace_idx = j;
            }
            break;
        }

        // Skip if no preceding }, or comment between } and else
        if (rbrace_idx < 0 || has_comment) continue;

        int rbrace_line = tokens[rbrace_idx].out_line;
        if (rbrace_line == else_line) continue;  // already on same line

        // Check if joining would exceed line_limit
        int rbrace_width = lidx.width(tokens, rbrace_line,
                                       opts.indent_str, opts.function_space);
        int else_width = lidx.width(tokens, else_line,
                                     opts.indent_str, opts.function_space);
        if (rbrace_width + 1 + else_width > opts.line_limit) continue;

        // Move ELSE and all following tokens on the same line to the } line
        for (int k = ei; k < n && tokens[k].out_line == else_line; k++) {
            tokens[k].out_line = rbrace_line;
        }

        // Rebuild line index since lines changed
        lidx.build(tokens);
    }
}
