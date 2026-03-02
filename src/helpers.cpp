#include "token.h"

bool needs_space(const Token& prev, const Token& tok,
                 const Token* prev_prev, bool function_space) {
    const std::string& p = prev.token;
    const std::string& t = tok.token;

    // Always space before comments
    if (t == "COMMENT") return true;

    if (tok_in(p, {"'('", "'['", "LBB"})) return false;
    if (tok_in(t, {"')'", "']'", "']]'", "','"})) return false;

    if (p == "')'") {
        if (tok_in(t, {"'['", "LBB", "'('", "'$'", "'@'", "':'"}))
            return false;
        return true;
    }

    if (t == "'('" && p == "SYMBOL_FUNCTION_CALL") return false;
    if (p == "'!'") return false;

    if (p == "'$'" || p == "'@'" || p == "':'" ||
        t == "'$'" || t == "'@'" || t == "':'")
        return false;

    if (p == "NS_GET" || t == "NS_GET" ||
        p == "NS_GET_INT" || t == "NS_GET_INT")
        return false;

    if (tok_in(p, {"IF", "ELSE", "FOR", "WHILE", "REPEAT", "IN"}))
        return true;

    if (t == "IN" || t == "ELSE") return true;

    if (t == "'('" && p == "FUNCTION") return function_space;
    if (t == "'{'" || p == "'{'" || t == "'}'" || p == "'}'") return true;

    // Unary minus/plus detection
    if (p == "'-'" || p == "'+'") {
        if (prev_prev == nullptr) return false; // first real token
        const std::string& pp = prev_prev->token;
        if (tok_in(pp, {"'('", "'['", "LBB", "','", "'{'",
                        "LEFT_ASSIGN", "EQ_ASSIGN", "RIGHT_ASSIGN",
                        "EQ_SUB", "EQ_FORMALS", "AND", "OR", "AND2",
                        "OR2", "GT", "LT", "GE", "LE", "EQ", "NE",
                        "'+'", "'-'", "'*'", "'/'", "'^'", "SPECIAL",
                        "'~'", "'!'", "IF", "ELSE", "FOR", "WHILE",
                        "REPEAT", "IN", "RETURN", "NEXT", "BREAK"}))
            return false;
    }

    // Binary operators
    if (tok_in(p, {"LEFT_ASSIGN", "EQ_ASSIGN", "RIGHT_ASSIGN", "EQ_SUB",
                    "EQ_FORMALS", "AND", "OR", "AND2", "OR2", "GT", "LT",
                    "GE", "LE", "EQ", "NE", "'+'", "'-'", "'*'", "'/'",
                    "'^'", "SPECIAL", "'~'"}) ||
        tok_in(t, {"LEFT_ASSIGN", "EQ_ASSIGN", "RIGHT_ASSIGN", "EQ_SUB",
                    "EQ_FORMALS", "AND", "OR", "AND2", "OR2", "GT", "LT",
                    "GE", "LE", "EQ", "NE", "'+'", "'-'", "'*'", "'/'",
                    "'^'", "SPECIAL", "'~'"}))
        return true;

    if (p == "','" || p == "';'") return true;
    if (tok_in(p, {"NEXT", "BREAK", "RETURN"})) return true;

    // Adjacent symbols/literals
    if (tok_in(p, {"SYMBOL", "SYMBOL_FUNCTION_CALL", "SYMBOL_FORMALS",
                    "SYMBOL_SUB", "SYMBOL_PACKAGE", "NUM_CONST",
                    "STR_CONST", "NULL_CONST", "SPECIAL"}) &&
        tok_in(t, {"SYMBOL", "SYMBOL_FUNCTION_CALL", "SYMBOL_FORMALS",
                    "SYMBOL_SUB", "SYMBOL_PACKAGE", "NUM_CONST",
                    "STR_CONST", "NULL_CONST", "SPECIAL"}))
        return true;

    return false;
}

int token_indent_level(const std::vector<Token>& tokens, int idx) {
    const std::string& tok = tokens[idx].token;
    int level = tokens[idx].nesting_level;
    if (tok == "'}'") level = std::max(0, level - 1);
    else if (tok_in(tok, {"')'", "']'"})) level = std::max(0, level - 1);
    else if (tok == "']]'") level = std::max(0, level - 2);
    return level;
}

int ast_line_width(const std::vector<Token>& tokens, int line_num,
                   const std::string& indent_str, bool function_space) {
    // Find tokens on this line
    std::vector<int> idx;
    for (int i = 0; i < static_cast<int>(tokens.size()); i++) {
        if (tokens[i].out_line == line_num) idx.push_back(i);
    }
    if (idx.empty()) return 0;

    int first_level = token_indent_level(tokens, idx[0]);
    int prefix_width = static_cast<int>(indent_str.size()) * first_level;

    int width = prefix_width;
    const Token* prev = nullptr;
    const Token* prev_prev = nullptr;
    for (int i : idx) {
        const Token& tok = tokens[i];
        if (prev != nullptr && needs_space(*prev, tok, prev_prev,
                                           function_space)) {
            width += 1;
        }
        width += static_cast<int>(tok.out_text.size());
        prev_prev = prev;
        prev = &tok;
    }
    return width;
}

Token make_token(const std::string& token_type, const std::string& text,
                 int out_line, double out_order, int parent) {
    Token t;
    t.token = token_type;
    t.text = text;
    t.out_text = text;
    t.line1 = 0; t.col1 = 0; t.line2 = 0; t.col2 = 0;
    t.out_line = out_line;
    t.out_order = out_order;
    t.parent = parent;
    t.id = 0;
    t.terminal = true;
    t.brace_depth = 0;
    t.paren_depth = 0;
    t.pab = 0;
    t.nesting_level = 0;
    return t;
}

int col_to_charpos(const std::string& line, int col) {
    if (line.find('\t') == std::string::npos) return col;
    int display_col = 0;
    for (size_t i = 0; i < line.size(); i++) {
        if (line[i] == '\t') {
            display_col += (8 - (display_col % 8));
        } else {
            display_col += 1;
        }
        if (display_col >= col) return static_cast<int>(i + 1);
    }
    return static_cast<int>(line.size());
}
