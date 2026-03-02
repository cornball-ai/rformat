#ifndef RFORMAT_TOKEN_H
#define RFORMAT_TOKEN_H

#include <string>
#include <vector>
#include <algorithm>
#include <initializer_list>

struct Token {
    std::string token;      // token type: "IF", "'('", "SYMBOL", etc.
    std::string text;       // original text
    std::string out_text;   // output text (e.g. "<-" for "=")
    int line1, col1, line2, col2;  // source positions
    int out_line;           // output line number
    double out_order;       // sort order within line (double for insertions)
    int parent;
    int id;
    bool terminal;
    int brace_depth, paren_depth, pab, nesting_level;
};

struct FormatOptions {
    std::string indent_str;
    std::string wrap;           // "paren" or "fixed"
    bool expand_if;
    std::string brace_style;   // "kr" or "allman"
    int line_limit;
    bool function_space;
    std::string control_braces; // "" for FALSE, "single", "multi", "next_line", "same_line"
};

// Sort tokens by (out_line, out_order)
inline void sort_tokens(std::vector<Token>& tokens) {
    std::sort(tokens.begin(), tokens.end(),
              [](const Token& a, const Token& b) {
                  if (a.out_line != b.out_line) return a.out_line < b.out_line;
                  return a.out_order < b.out_order;
              });
}

// Sort and reassign out_order sequentially
inline void reorder_tokens(std::vector<Token>& tokens) {
    sort_tokens(tokens);
    for (size_t i = 0; i < tokens.size(); i++) {
        tokens[i].out_order = static_cast<double>(i + 1);
    }
}

// Check if token type is in a set
inline bool tok_in(const std::string& tok,
                   std::initializer_list<const char*> set) {
    for (const char* s : set) {
        if (tok == s) return true;
    }
    return false;
}

// Find matching close paren ')' starting from open_idx+1
// Returns index or -1 if not found
inline int find_matching_paren(const std::vector<Token>& tokens, int open_idx) {
    int depth = 1;
    int i = open_idx + 1;
    int n = static_cast<int>(tokens.size());
    while (i < n && depth > 0) {
        if (tokens[i].token == "'('") depth++;
        else if (tokens[i].token == "')'") depth--;
        if (depth > 0) i++;
    }
    return (i < n) ? i : -1;
}

// Find matching close brace '}' starting from open_idx+1
inline int find_matching_brace(const std::vector<Token>& tokens,
                               int open_idx) {
    int depth = 1;
    int i = open_idx + 1;
    int n = static_cast<int>(tokens.size());
    while (i < n && depth > 0) {
        if (tokens[i].token == "'{'") depth++;
        else if (tokens[i].token == "'}'") depth--;
        if (depth > 0) i++;
    }
    return (i < n) ? i : -1;
}

// Line index: maps line number -> sorted vector of token indices
// Avoids O(n) scans for "find all tokens on line X"
struct LineIndex {
    std::vector<std::vector<int>> lines; // lines[line_num] = {tok_idx...}

    void build(const std::vector<Token>& tokens) {
        int max_line = 0;
        for (const auto& t : tokens) {
            if (t.out_line > max_line) max_line = t.out_line;
        }
        lines.assign(max_line + 2, std::vector<int>());
        int n = static_cast<int>(tokens.size());
        for (int i = 0; i < n; i++) {
            int ln = tokens[i].out_line;
            if (ln >= 0 && ln < static_cast<int>(lines.size())) {
                lines[ln].push_back(i);
            }
        }
    }

    const std::vector<int>& get(int line_num) const {
        static const std::vector<int> empty;
        if (line_num < 0 || line_num >= static_cast<int>(lines.size()))
            return empty;
        return lines[line_num];
    }

    int width(const std::vector<Token>& tokens, int line_num,
              const std::string& indent_str, bool function_space) const;
};

// Forward declarations - helpers.cpp
bool needs_space(const Token& prev, const Token& tok,
                 const Token* prev_prev, bool function_space);
int token_indent_level(const std::vector<Token>& tokens, int idx);
int ast_line_width(const std::vector<Token>& tokens, int line_num,
                   const std::string& indent_str, bool function_space);
Token make_token(const std::string& token_type, const std::string& text,
                 int out_line, double out_order, int parent = 0);
int col_to_charpos(const std::string& line, int col);

// Forward declarations - enrich.cpp
void enrich_terminals(std::vector<Token>& tokens);
void recompute_nesting(std::vector<Token>& tokens);
void renumber_lines(std::vector<Token>& tokens);
void restore_truncated_str_const(std::vector<Token>& tokens,
                                 const std::vector<std::string>& orig_lines);

// Forward declarations - serialize.cpp
std::string serialize_tokens(std::vector<Token>& tokens,
                             const FormatOptions& opts);

// Forward declarations - transforms
void collapse_calls(std::vector<Token>& tokens, const FormatOptions& opts);
void wrap_long_operators(std::vector<Token>& tokens, const FormatOptions& opts);
void wrap_long_calls(std::vector<Token>& tokens, const FormatOptions& opts);
void reformat_function_defs(std::vector<Token>& tokens,
                            const FormatOptions& opts);
void add_control_braces(std::vector<Token>& tokens, const FormatOptions& opts);
void expand_call_if_args(std::vector<Token>& tokens,
                         const FormatOptions& opts);
void reformat_inline_if(std::vector<Token>& tokens, const FormatOptions& opts);

#endif
