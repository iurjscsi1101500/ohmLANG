#pragma once
#include <iostream>
#include <vector>
#include <string>
#include <utility>
#include <variant>
#include <memory>

enum Tokens : int {
    tok_eof = 0,

    //keyword
    tok_int,
    tok_char,
    tok_float,
    tok_for_loop,
    tok_while_loop,
    tok_return,
    tok_if,
    tok_else,
    tok_const,
    tok_continue,
    tok_break,
    tok_void,
    tok_def,
    tok_let,
    tok_class,
    tok_enum,
    //punctuation
    tok_column,
    tok_semicolon,
    tok_comma,
    //literal
    tok_identifier,
    //operators
    tok_equal,
    tok_num,
    tok_plus,
    tok_minus,
    tok_multiple,
    tok_divide,
    tok_percent,
    tok_float_num,
    tok_open_bracket,
    tok_close_bracket,
    tok_open_para,
    tok_close_para,
    tok_is_equal_to,
    tok_dosent_equal_to,
    tok_smaller_than,
    tok_greater_than,
    tok_is_small_or_equal_to,
    tok_is_greater_or_equal_to,
    tok_big_para_open,
    tok_big_para_close,
    tok_and,
    tok_or,
    tok_minus_equal,
    tok_plus_equal,
    tok_not,
    tok_string_literal,
    tok_multiply_equal,
    tok_divide_equal,
    tok_percent_equal,
    tok_string_type,
    tok_char_var,
    tok_bool,
    tok_true,
    tok_false,
    tok_dot_dot,
    tok_import,
    tok_arrow,
    tok_in,
    tok_new
};
using Token = std::pair<Tokens, std::string>;

std::vector<Token> lexer(const std::string& string)
{
    std::vector<Token> lol;

    for (int i = 0; i < string.length(); ++i)
    {
        if (std::isspace(string[i]))
            continue;

        if (string[i] == '/' && i + 1 < string.size() && string[i + 1] == '/') {
            while (i < string.size() && string[i] != '\n') ++i;
            continue;
        }
        auto match_kw = [&](const std::string& kw, Tokens tok) {
            bool prev_ok = (i == 0 || !(std::isalnum(string[i - 1]) || string[i - 1] == '_'));
            bool next_ok = (i + kw.length() >= string.size() || !(std::isalnum(string[i + kw.length()]) || string[i + kw.length()] == '_'));

            if (prev_ok && string.substr(i, kw.length()) == kw && next_ok) {
                lol.push_back({tok, kw});
                i += kw.length() - 1;
                return true;
            }
            return false;
        };

        auto match_op = [&](const std::string& op, Tokens tok) {
            if (string.substr(i, op.length()) == op) {
                lol.push_back({tok, op});
                i += op.length() - 1;
                return true;
            }
            return false;
        };
        //done
        if (match_op("==", tok_is_equal_to)) continue;
        if (match_op("!=", tok_dosent_equal_to)) continue;
        if (match_op(">=", tok_is_greater_or_equal_to)) continue;
        if (match_op("<=", tok_is_small_or_equal_to)) continue;
        if (match_op("&&", tok_and)) continue;
        if (match_op("||", tok_or)) continue;
        if (match_op("+=", tok_plus_equal)) continue;
        if (match_op("-=", tok_minus_equal)) continue;
        if (match_op("/=", tok_divide_equal)) continue;
        if (match_op("*=", tok_multiply_equal)) continue;
        if (match_op("%=", tok_percent_equal)) continue;
        if (match_op("->", tok_arrow)) continue; // not done
        //
        if (match_kw("..", tok_dot_dot)) continue; //done
        if (match_kw("def", tok_def)) continue; //done
        if (match_kw("new", tok_new)) continue; //done
        if (match_kw("char", tok_char)) continue; //done
        if (match_kw("int", tok_int)) continue; //done
        if (match_kw("float", tok_float)) continue; //done
        if (match_kw("for", tok_for_loop)) continue; //done
        if (match_kw("in", tok_in)) continue; //done
        if (match_kw("while", tok_while_loop)) continue; //done
        if (match_kw("return", tok_return)) continue; //done
        if (match_kw("if", tok_if)) continue; //done
        if (match_kw("else", tok_else)) continue; //done
        if (match_kw("const", tok_const)) continue; //done
        if (match_kw("continue", tok_continue)) continue; //done
        if (match_kw("break", tok_break)) continue; //done
        if (match_kw("void", tok_void)) continue; //done
        if (match_kw("let", tok_let)) continue; //done
        if (match_kw("class", tok_class)) continue; //done
        if (match_kw("enum", tok_enum)) continue; //done
        if (match_kw("string", tok_string_type)) continue; //done
        if (match_kw("bool", tok_bool)) continue; //done
        if (match_kw("true", tok_true)) continue; //done
        if (match_kw("false", tok_false)) continue; //done
        if (match_kw("import", tok_import)) continue; //done

        switch (string[i]) {
            case '=': lol.push_back({tok_equal, "="}); continue; //done
            case '+': lol.push_back({tok_plus, "+"}); continue; //done
            case '*': lol.push_back({tok_multiple, "*"}); continue; //done
            case '/': lol.push_back({tok_divide, "/"}); continue; //done
            case '-': //done
                if (i + 1 < string.size() && std::isdigit(string[i + 1])) {
                    int j = i + 1;
                    while (j < string.size() && std::isdigit(string[j])) ++j;
                    if (j < string.size() && string[j] == '.' && j + 1 < string.size() && std::isdigit(string[j + 1])) {
                        ++j;
                        while (j < string.size() && std::isdigit(string[j])) ++j;
                        lol.push_back({tok_float_num, string.substr(i, j - i)});
                    } else {
                        lol.push_back({tok_num, string.substr(i, j - i)});
                    }
                    i = j - 1;
                } else {
                    lol.push_back({tok_minus, "-"});
                }
                continue;
            case '%': lol.push_back({tok_percent, "%"}); continue; //done
            case ';': lol.push_back({tok_semicolon, ";"}); continue; //done
            case ',': lol.push_back({tok_comma, ","}); continue; //done
            case '(': lol.push_back({tok_open_bracket, "("}); continue; //done
            case ')': lol.push_back({tok_close_bracket, ")"}); continue; //done
            case '{': lol.push_back({tok_open_para, "{"}); continue; //done
            case '}': lol.push_back({tok_close_para, "}"}); continue; //done
            case '>': lol.push_back({tok_greater_than, ">"}); continue; //done
            case '<': lol.push_back({tok_smaller_than, "<"}); continue; //done
            case ':': lol.push_back({tok_column, ":"}); continue; //done
            case '[': lol.push_back({tok_big_para_open, "["}); continue; //done
            case ']': lol.push_back({tok_big_para_close, "]"}); continue; //done
            case '!': lol.push_back({tok_not, "!"}); continue; //done
        }

        // String literals
        if (string[i] == '"') {
            int start = ++i;
            std::string strVal;
            while (i < string.size() && string[i] != '"') {
                if (string[i] == '\\' && i + 1 < string.size()) {
                    if (string[i + 1] == 'n') strVal += '\n';
                    else if (string[i + 1] == 't') strVal += '\t';
                    else strVal += string[i + 1];
                    i += 2;
                } else {
                    strVal += string[i++];
                }
            }
            lol.push_back({tok_string_literal, strVal});
            continue;
        }
        if (string[i] == '\'') {
            int start = i;
            i++;
            if (i >= string.size()) continue;

            char c = string[i++];
            if (i >= string.size() || string[i] != '\'') {
                throw std::runtime_error("Invalid char literal at position " + std::to_string(start));
            }
            std::string s(1, c);
            lol.push_back({tok_char_var, s});
            continue;
        }
        // Float or integer literals
        if (std::isdigit(string[i])) {
            int j = i;
            while (j < string.size() && std::isdigit(string[j])) j++;
            if (j < string.size() && string[j] == '.' && j+1 < string.size() && std::isdigit(string[j+1])) {
                j++;
                while (j < string.size() && std::isdigit(string[j])) j++;
                lol.push_back({tok_float_num, string.substr(i, j - i)});
                i = j - 1;
                continue;
            } else {
                lol.push_back({tok_num, string.substr(i, j - i)});
                i = j - 1;
                continue;
            }
        }

        // Variable names
        if (std::isalpha(string[i]) || string[i] == '_') {
            int start = i;
            while (i < string.size() && (std::isalnum(string[i]) || string[i] == '_')) {
                ++i;
            }
            std::string varName = string.substr(start, i - start);
            lol.push_back({tok_identifier, varName});
            --i;
            continue;
        }
    }

    lol.push_back({tok_eof, ""});
    return lol;
}
const char* token_to_string(Tokens tok) {
    switch (tok) {
        case tok_eof:                      return "tok_eof";

        // Keywords
        case tok_int:                      return "tok_int";
        case tok_char:                     return "tok_char";
        case tok_float:                    return "tok_float";
        case tok_for_loop:                 return "tok_for_loop";
        case tok_while_loop:               return "tok_while_loop";
        case tok_return:                   return "tok_return";
        case tok_if:                       return "tok_if";
        case tok_else:                     return "tok_else";
        case tok_const:                    return "tok_const";
        case tok_continue:                 return "tok_continue";
        case tok_break:                    return "tok_break";
        case tok_void:                     return "tok_void";
        case tok_def:                      return "tok_def";
        case tok_let:                      return "tok_let";

        // Punctuation
        case tok_column:                   return "tok_column";
        case tok_semicolon:                return "tok_semicolon";
        case tok_comma:                    return "tok_comma";

        // Literals
        case tok_identifier:               return "tok_identifier";
        case tok_num:                      return "tok_num";
        case tok_float_num:                return "tok_float_num";
        case tok_string_literal:           return "tok_string_literal";

        // Operators
        case tok_equal:                    return "tok_equal";
        case tok_plus:                     return "tok_plus";
        case tok_minus:                    return "tok_minus";
        case tok_multiple:                 return "tok_multiple";
        case tok_divide:                   return "tok_divide";
        case tok_percent:                  return "tok_percent";

        // Brackets and Parentheses
        case tok_open_bracket:             return "tok_open_bracket";
        case tok_close_bracket:            return "tok_close_bracket";
        case tok_open_para:                return "tok_open_para";
        case tok_close_para:               return "tok_close_para";
        case tok_big_para_open:            return "tok_big_para_open";
        case tok_big_para_close:           return "tok_big_para_close";

        // Comparison
        case tok_is_equal_to:              return "tok_is_equal_to";
        case tok_dosent_equal_to:          return "tok_dosent_equal_to";
        case tok_smaller_than:             return "tok_smaller_than";
        case tok_greater_than:             return "tok_greater_than";
        case tok_is_small_or_equal_to:     return "tok_is_small_or_equal_to";
        case tok_is_greater_or_equal_to:   return "tok_is_greater_or_equal_to";

        // Logical and Compound Assignments
        case tok_and:                      return "tok_and";
        case tok_or:                       return "tok_or";
        case tok_minus_equal:              return "tok_minus_equal";
        case tok_plus_equal:               return "tok_plus_equal";
        case tok_not:                      return "tok_not";
        case tok_dot_dot:                  return "tok_dot_dot";
        default:                           return "unknown_token";
    }
}
void print_tokens(const std::vector<Token>& vec) {
    for (const auto& p : vec) {
        std::cout << token_to_string(p.first) << " : '" << p.second << "'\n";
    }
}