#pragma once
#include <string>
#include <iostream>
#include <vector>
#include "lexer.hpp"
#include "ast.hpp"
#include <map>
#include <functional>
#include <unordered_map>
#include <stdexcept>
//AssignementExpr
using SymbolTable = std::unordered_map<std::string, std::shared_ptr<ast::types>>;
SymbolTable symtab;
bool is_type_compatible(const std::shared_ptr<ast::types>& declared, const std::shared_ptr<ast::types>& inferred) {
    if (!declared || !inferred)
        return false;

    return declared->type_name() == inferred->type_name();
}
enum binding_power : int {
    default_bp = 0,
    comma,
    assignment,
    logical,
    equality,
    relational,
    additive,
    multiplicative,
    unary,
    call,
    member,
    primary
};
class parser
{
public:
    std::vector<std::string> errors;
    std::vector<Token> tokens;
    int position;
    bool is_in_scope_class = false;
    bool is_in_scope_enum = false;
    bool hastokens(parser *p);
    Token AdvanceToken(parser *p);
    Token currentToken(parser *p);
    Token expect(Tokens expected, parser *p);
    Token expect_or(Tokens expected_1, Tokens expected_2, parser *p);
    parser(std::vector<Token> tokens) : tokens(tokens), position(0) {}
    ~parser();
};
std::vector<std::string> names;
std::vector<std::string> varNamess;
using stmt_handler = std::shared_ptr<ast::stmt> (*)(parser*);
using nud_handler  = std::shared_ptr<ast::Expr> (*)(parser*);
using led_handler  = std::shared_ptr<ast::Expr> (*)(parser*, std::shared_ptr<ast::Expr>, binding_power);

using stmt_lookup = std::map<Tokens, stmt_handler>;
using nud_lookup  = std::map<Tokens, nud_handler>;
using led_lookup  = std::map<Tokens, led_handler>;
using bp_lookup   = std::map<Tokens, binding_power>;
stmt_lookup stmt_lu;
nud_lookup nud_lu;
led_lookup led_lu;
bp_lookup bp_lu;

using type_nud_handler  = std::shared_ptr<ast::types> (*)(parser*);
using type_led_handler  = std::shared_ptr<ast::types> (*)(parser*, std::shared_ptr<ast::types>, binding_power);

using type_nud_lookup  = std::map<Tokens, type_nud_handler>;
using type_led_lookup  = std::map<Tokens, type_led_handler>;
using type_bp_lookup   = std::map<Tokens, binding_power>;
type_nud_lookup type_nud_lu;
type_led_lookup type_led_lu;
type_bp_lookup type_bp_lu;
Token parser::currentToken(parser *p) {
    return p->tokens[p->position];
}
Token parser::AdvanceToken(parser *p) {
    Token crtkn = currentToken(p);
    p->position++;
    return crtkn;
}
Token parser::expect(Tokens expected, parser *p)
{
    Token currenttoken = currentToken(p);

    //std::cout << token_to_string(currenttoken.first) << " " << token_to_string(expected) << std::endl;
    if (currenttoken.first != expected) {
        std::cout << token_to_string(currenttoken.first) << " " << token_to_string(expected) << std::endl;
        throw std::runtime_error("unexpected token\n");
    }
    return p->AdvanceToken(p);
}
Token parser::expect_or(Tokens expected_1, Tokens expected_2, parser *p) {
    Token currenttoken = currentToken(p);

    if (currenttoken.first != expected_1 && currenttoken.first != expected_2) {
        std::cout << token_to_string(currenttoken.first) << " " << token_to_string(expected_1) << std::endl;
        throw std::runtime_error("unexpected token\n");
    }
    return p->AdvanceToken(p);
}
bool parser::hastokens(parser *p) {
    return p->position < p->tokens.size() &&
           p->tokens[p->position].first != Tokens::tok_eof;
}
void led(Tokens kind, binding_power bp, led_handler led_fn)
{
    bp_lu[kind] = bp;
    led_lu[kind] = led_fn;
}
void nud(Tokens kind, binding_power bp, nud_handler nud_fn)
{
    bp_lu[kind] = bp;
    nud_lu[kind] = nud_fn;
}

void type_led(Tokens kind, binding_power bp, type_led_handler led_fn)
{
    type_bp_lu[kind] = bp;
    type_led_lu[kind] = led_fn;
}
void type_nud(Tokens kind, type_nud_handler nud_fn)
{
    type_nud_lu[kind] = nud_fn;
}
void stmt(Tokens kind, binding_power bp, stmt_handler stmt_fn)
{
    bp_lu[kind] = default_bp;
    stmt_lu[kind] = stmt_fn;
}
std::shared_ptr<ast::Expr> parse_expr(parser *p, binding_power bp)
{
    auto tokenKind = p->currentToken(p).first;
    auto nud_fn = nud_lu[tokenKind];
    if (!nud_fn) {
        std::cout << token_to_string(tokenKind) << "\n";
        throw std::runtime_error("Error nud_fn does not exist\n");
    }
    auto left = nud_fn(p);

    while (bp_lu[p->currentToken(p).first] > bp) {
        tokenKind = p->currentToken(p).first;
        auto led_fn = led_lu[tokenKind];
        std::cout << token_to_string(tokenKind) << "\n";
        if (!led_fn) {
            throw std::runtime_error("Error led_fn does not exist\n");
        }
        left = led_fn(p, left, bp_lu[p->currentToken(p).first]);
    }
    return left;
}
std::shared_ptr<ast::types> parse_types(parser *p, binding_power bp)
{
    auto tokenKind = p->currentToken(p).first;
    auto nud_fn = type_nud_lu[tokenKind];
    if (!nud_fn) {
        std::cout << token_to_string(tokenKind) << "\n";
        throw std::runtime_error("Error type_nud_fn does not exist\n");
    }
    auto type_left = nud_fn(p);

    while (type_bp_lu[p->currentToken(p).first] > bp) {
        tokenKind = p->currentToken(p).first;
        auto type_led_fn = type_led_lu[tokenKind];
        if (!type_led_fn) {
            throw std::runtime_error("Error type_led_fn does not exist\n");
        }
        type_left = type_led_fn(p, type_left, type_bp_lu[p->currentToken(p).first]);
    }
    return type_left;
}
bool is_comparison_operator(const std::string& op) {
    return op == "<" || op == "<=" || op == ">" || op == ">=";
}

bool is_equality_operator(const std::string& op) {
    return op == "==" || op == "!=";
}
std::shared_ptr<ast::types> infer_type_from_expr(std::shared_ptr<ast::Expr> expr) {
    using namespace ast;

    if (!expr) return nullptr;
    if (auto intlit = std::dynamic_pointer_cast<NumberExpr>(expr)) {
        return std::make_shared<SymbolType>("int");
    } else if (auto charlit = std::dynamic_pointer_cast<CharExpr>(expr)) {
        return std::make_shared<SymbolType>("char");
    } else if (auto strlit = std::dynamic_pointer_cast<StringExpr>(expr)) {
        return std::make_shared<SymbolType>("string");
    } else if (auto floatlit = std::dynamic_pointer_cast<FloatExpr>(expr)) {
        return std::make_shared<SymbolType>("float");
    } else if (auto boollit = std::dynamic_pointer_cast<BoolExpr>(expr)) {
        return std::make_shared<SymbolType>("bool");
    } else if (auto sym = std::dynamic_pointer_cast<SymbolExpr>(expr)) {
        std::cout << sym->symbol << "sigma.com ";
        auto it = symtab.find(sym->symbol);
        if (it == symtab.end()) {
            throw std::runtime_error("Undefined variable used in expression: " + sym->symbol);
        }
        return it->second;
    } else if (auto arr = std::dynamic_pointer_cast<ArrayLiteral>(expr)) {
        if (arr->expr.empty()) {
            throw std::runtime_error("Cannot infer type of empty array literal.");
        }
        auto element_type = infer_type_from_expr(arr->expr[0]);
        return std::make_shared<ArrayType>(element_type, static_cast<int>(arr->expr.size()));
    }  else if (auto binop = std::dynamic_pointer_cast<ast::BinaryExpr>(expr)) {
        const std::string& op = binop->opreator;
        auto ltype = infer_type_from_expr(binop->left);
        auto rtype = infer_type_from_expr(binop->right);

        if (is_equality_operator(op) || is_comparison_operator(op) || op == "&&" || op == "||") {
            return std::make_shared<ast::SymbolType>("bool");
        } else if (ltype->type_name() == "float" || rtype->type_name() == "float") {
            return std::make_shared<ast::SymbolType>("float");
        } else if (ltype->type_name() == "int" && rtype->type_name() == "int") {
            return std::make_shared<ast::SymbolType>("int");
        } else {
            throw std::runtime_error("Cannot infer type of binary expr with '" +
                                     ltype->type_name() + "' and '" + rtype->type_name() + "'");
        }


        throw std::runtime_error("Cannot infer type from this expression.");
    }
    throw std::runtime_error("Cannot infer type from this expression.");
}
std::shared_ptr<ast::Expr> parse_primary_expr(parser *p) {
    switch (p->currentToken(p).first)
    {
    case tok_num:
        return std::make_shared<ast::NumberExpr>(atoi(p->AdvanceToken(p).second.c_str()));
    case tok_identifier:
        return std::make_shared<ast::SymbolExpr>(p->AdvanceToken(p).second);
    case tok_string_literal:
        return std::make_shared<ast::StringExpr>(p->AdvanceToken(p).second);
    case tok_float_num:
        return std::make_shared<ast::FloatExpr>(atof(p->AdvanceToken(p).second.c_str()));
    case tok_char_var:
        return std::make_shared<ast::CharExpr>(p->AdvanceToken(p).second[0]);
    case tok_true:
        return std::make_shared<ast::BoolExpr>(true);
    case tok_false:
        return std::make_shared<ast::BoolExpr>(false);
    default:
        throw std::runtime_error("Cannot create primary expression\n");
        break;
    }
}
std::shared_ptr<ast::Expr> parse_binary_expr(parser *p, std::shared_ptr<ast::Expr> left, binding_power bp) {
    auto _operator = p->AdvanceToken(p).second;
    std::shared_ptr<ast::Expr> right = parse_expr(p, bp);

    auto left_type_ptr = infer_type_from_expr(left);
    auto right_type_ptr = infer_type_from_expr(right);
    std::string l_type = left_type_ptr->type_name();
    std::string r_type = right_type_ptr->type_name();

    std::cout << "Left Type: " << l_type << ", Right Type: " << r_type << "\n";

    if (_operator == "&&" || _operator == "||") {
        if (l_type != "bool" || r_type != "bool") {
            throw std::runtime_error("Error: Logical operators require bool operands, got '" + l_type + "' and '" + r_type + "'");
        }
    } else if (is_comparison_operator(_operator)) {
        bool valid = (l_type == "int" && r_type == "int") ||
                     (l_type == "float" && r_type == "float") ||
                     (l_type == "int" && r_type == "float") ||
                     (l_type == "float" && r_type == "int") ||
                     (l_type == "char" && r_type == "char");
        if (!valid)
            throw std::runtime_error("Error: Invalid comparison types: '" + l_type + "' and '" + r_type + "'");
    } else if (is_equality_operator(_operator)) {
        bool compatible = (l_type == r_type) ||
                          ((l_type == "int" && r_type == "float") || (l_type == "float" && r_type == "int")) ||
                          (l_type == "string" && r_type == "string") ||
                          (l_type == "char" && r_type == "char") ||
                          (l_type == "bool" && r_type == "bool");
        if (!compatible)
            throw std::runtime_error("Error: Incompatible equality types: '" + l_type + "' and '" + r_type + "'");
    } else {
        // Assume arithmetic or bitwise
        bool valid = (l_type == "int" || l_type == "float") && (r_type == "int" || r_type == "float");
        if (!valid)
            throw std::runtime_error("Error: Arithmetic operations require numeric types, got '" + l_type + "' and '" + r_type + "'");
    }

    return std::make_shared<ast::BinaryExpr>(left, _operator, right);
}

std::shared_ptr<ast::Expr> parse_paren_expr(parser* p) {
    p->expect(Tokens::tok_open_bracket, p);
    auto expr = parse_expr(p, default_bp);
    p->expect(Tokens::tok_close_bracket, p);
    return expr;
}

std::shared_ptr<ast::Expr> parse_range_expr(parser *p, std::shared_ptr<ast::Expr> left, binding_power bp) {
    p->AdvanceToken(p);
    auto right = parse_expr(p, bp);

    if (auto sym = std::dynamic_pointer_cast<ast::SymbolExpr>(left)) {
        if (symtab.find(sym->symbol) == symtab.end())
            throw std::runtime_error("Undefined variable in range start: " + sym->symbol);
    } else if (!std::dynamic_pointer_cast<ast::NumberExpr>(left)) {
        throw std::runtime_error("Invalid range start: must be int or defined variable");
    }

    if (auto sym = std::dynamic_pointer_cast<ast::SymbolExpr>(right)) {
        if (symtab.find(sym->symbol) == symtab.end())
            throw std::runtime_error("Undefined variable in range end: " + sym->symbol);
    } else if (!std::dynamic_pointer_cast<ast::NumberExpr>(right)) {
        throw std::runtime_error("Invalid range end: must be int or defined variable");
    }

    return std::make_shared<ast::RangeExpr>(left, right);
}

std::shared_ptr<ast::stmt> parse_stmt(parser *p, bool is_in_loop = false) {
    if (p->currentToken(p).first == Tokens::tok_eof)
        return nullptr;
    auto tokenKind = p->currentToken(p).first;

    if (tokenKind == Tokens::tok_break) {
        if (!is_in_loop)
            throw std::runtime_error("'break' used outside of a loop");
        p->AdvanceToken(p);
        p->expect(Tokens::tok_semicolon, p);
        return std::make_shared<ast::BreakStmt>();
    }

    if (tokenKind == Tokens::tok_continue) {
        if (!is_in_loop)
            throw std::runtime_error("'continue' used outside of a loop");
        p->AdvanceToken(p);
        p->expect(Tokens::tok_semicolon, p);
        return std::make_shared<ast::ContinueStmt>();
    }

    auto stmt_func = stmt_lu[tokenKind];

    if (stmt_func) {
        return stmt_func(p);
    }

    auto expression = parse_expr(p, default_bp);
    p->expect(Tokens::tok_semicolon, p);

    return std::make_shared<ast::ExprStmt>(expression);
}
std::shared_ptr<ast::Expr> parse_assignment(parser *p, std::shared_ptr<ast::Expr> left, binding_power bp) {
    auto operatorToken = p->AdvanceToken(p);
    auto rhs = parse_expr(p, assignment);

    if (auto sym = std::dynamic_pointer_cast<ast::SymbolExpr>(left)) {
        auto it = symtab.find(sym->symbol);
        if (it == symtab.end()) {
            throw std::runtime_error("Undefined variable on left-hand side of assignment: " + sym->symbol);
        }
        auto lhs_type = it->second;
        auto rhs_type = infer_type_from_expr(rhs);
            if (operatorToken.first != tok_equal) {
                if (lhs_type->type_name() != "int" || rhs_type->type_name() != "int") {
                    throw std::runtime_error(
                        "Compound assignment (like +=, -=, etc.) is only allowed for variables of type 'int', "
                        "but variable '" + sym->symbol + "' is of type '" + lhs_type->type_name() + "'."
                    );
                }
            } else {
                if (lhs_type->type_name() != rhs_type->type_name())
                    throw std::runtime_error("types are not same");
            }
    } else {
        throw std::runtime_error("Left-hand side of assignment must be a variable.");
    }

    return std::make_shared<ast::AssignementExpr>(operatorToken.second, rhs, left);
}

std::shared_ptr<ast::stmt> parse_var(parser *p)
{
    std::shared_ptr<ast::types> extypes = NULL;
    std::shared_ptr<ast::Expr> assignment_ = NULL;
    auto isconst = p->currentToken(p).first == Tokens::tok_const;
    if (isconst) p->AdvanceToken(p);
    p->expect(Tokens::tok_let, p);
    auto varName = p->expect(Tokens::tok_identifier, p).second;
    for (auto s: varNamess) {
        if (varName == s) {
            throw std::runtime_error("Name already taken");
        }
    }
    for (auto s: names) {
        if (varName == s) {
            throw std::runtime_error("Name already taken");
        }
    }
    if (p->is_in_scope_class || p->is_in_scope_enum)
        varNamess.push_back(varName + "aloo.com.bruh.brr_brr_patabin_idk_lol_hi_noob_pro_max_ultra__" + names.back());
    else
        varNamess.push_back(varName);

    if (p->currentToken(p).first == Tokens::tok_column) {
        p->expect(Tokens::tok_column, p);
        extypes = parse_types(p, default_bp);
        p->AdvanceToken(p);
    }
    if (p->currentToken(p).first != Tokens::tok_semicolon) {
        p->expect(Tokens::tok_equal, p);
        assignment_= parse_expr(p, assignment);
        if (extypes == nullptr) {
            extypes = infer_type_from_expr(assignment_);
            if (extypes->type_name() == "void")
                p->AdvanceToken(p);
        } else {
            auto inferred = infer_type_from_expr(assignment_);
            if (extypes->type_name() == "void")
                p->AdvanceToken(p);
            if (!is_type_compatible(extypes, inferred)) {
                throw std::runtime_error("Type mismatch: expected '" + extypes->type_name() +
                                        "', but got '" + inferred->type_name() + "'");
            }
        }

    }
    p->expect(Tokens::tok_semicolon, p);
    if ((assignment_ == NULL && isconst == true) || (assignment_ == NULL && extypes == NULL))
        throw std::runtime_error("Need a value to declare const OR need a value to declare var with types\n");

    symtab[varName] = extypes;
    names.push_back(varName);

    return std::make_shared<ast::VarDecStmt>(assignment_, varName, isconst, extypes);
}
std::shared_ptr<ast::types> parse_symbol_type(parser *p) {
    return std::make_shared<ast::SymbolType>(p->currentToken(p).second);
}
std::shared_ptr<ast::types> parse_array_type(parser *p) {
    p->expect(Tokens::tok_big_para_open, p);
    auto expr = parse_expr(p, default_bp);
    auto numExpr = std::dynamic_pointer_cast<ast::NumberExpr>(expr);
    if (!numExpr)
        throw std::runtime_error("size only can be integer");
    if (numExpr->is_negitive_or_zero)
        throw std::runtime_error("size only can be positive integer");
    
    p->expect(Tokens::tok_big_para_close, p);
    auto _p = parse_types(p, primary);
    return std::make_shared<ast::ArrayType>(_p, numExpr->num);
}

std::shared_ptr<ast::stmt> parse_block(parser *p, bool is_in_loop = false, bool are_return_allowed = true) {
    p->expect(Tokens::tok_open_para, p);
    std::vector<std::shared_ptr<ast::stmt>> body;

    while (p->hastokens(p) && p->currentToken(p).first != tok_close_para) {
        auto x = parse_stmt(p, is_in_loop);
        if (std::dynamic_pointer_cast<ast::ReturnStmt>(x) && !are_return_allowed) {
            throw std::runtime_error("Return cant be used");
        }
        body.push_back(x);
    }

    p->expect(Tokens::tok_close_para, p);
    return std::make_shared<ast::BlockStmt>(body);
}

std::shared_ptr<ast::stmt> parse_class_dec_stmt(parser *p) {
    p->AdvanceToken(p);
    auto className = p->expect(Tokens::tok_identifier, p).second;
    for (auto s: varNamess) {
        if (className == s) {
            throw std::runtime_error("Name already taken");
        }
    }
    for (auto s: names) {
        if (className == s) {
            throw std::runtime_error("Name already taken");
        }
    }
    names.push_back(className);
    p->is_in_scope_class = true;
    auto classBody = parse_block(p, false, false);
    p->is_in_scope_class = false;
    
    auto blockStmt = std::dynamic_pointer_cast<ast::BlockStmt>(classBody);


    return std::make_shared<ast::ClassDecStmt>(className, blockStmt->body);
}
std::shared_ptr<ast::stmt> parse_enum_dec_stmt(parser *p) {
    p->AdvanceToken(p);
    auto EnumName = p->expect(Tokens::tok_identifier, p).second;
    for (auto s: varNamess) {
        if (EnumName == s) {
            throw std::runtime_error("Name already taken");
        }
    }
    for (auto s: names) {
        if (EnumName == s) {
            throw std::runtime_error("Name already taken");
        }
    }
    names.push_back(EnumName);
    p->is_in_scope_enum = true;
    auto EnumBody = parse_block(p);
    p->is_in_scope_enum = false;
    auto blockStmt = std::dynamic_pointer_cast<ast::BlockStmt>(EnumBody);
    std::vector<std::string> varNames;
    std::vector<std::shared_ptr<ast::VarDecStmt>> vars;
    for (const auto& stmtptr : blockStmt->body) {
        auto varDecl = std::dynamic_pointer_cast<ast::VarDecStmt>(stmtptr);
        if (!varDecl) {
            throw std::runtime_error("Expected only variable declarations in enum block");
        }

        if (varDecl->ExplicitType->type_name() != "int") {
            throw std::runtime_error("Expected only int variables in enum block");
        }
        if (varDecl->expr) {
            throw std::runtime_error("Cannot assign values to enum block variables");
        }
        if (varDecl->is_const) {
            throw std::runtime_error("Enum block cannot contain const values");
        }
        for (auto s: varNames) {
            if (s == varDecl->var_name) {
                throw std::runtime_error("Variable (" + varDecl->var_name + ") Already taken");
            }
        }
        varNames.push_back(varDecl->var_name);
        vars.push_back(varDecl);
    }
    return std::make_shared<ast::EnumDecStmt>(EnumName, vars);
}

std::shared_ptr<ast::stmt> parse_import(parser *p) {
    p->AdvanceToken(p);
    auto filename = p->expect(Tokens::tok_identifier, p).second;
    p->expect(Tokens::tok_semicolon, p);
    return std::make_shared<ast::ImportStmt>(filename);
}
std::shared_ptr<ast::Expr> parse_prefix(parser *p) {
    auto opToken = p->AdvanceToken(p).second;  // e.g. "!"
    auto expr_ = parse_expr(p, unary);
    if (!expr_) {
        throw std::runtime_error("parse_expr returned nullptr in parse_prefix");
    }
    auto type = infer_type_from_expr(expr_);
    if (!type) {
        throw std::runtime_error("infer_type_from_expr returned nullptr in parse_prefix");
    }
    if (type->type_name() != "bool") {
        throw std::runtime_error("Expected bool expression after unary operator");
    }
    return std::make_shared<ast::PrefixExpr>(opToken, expr_);
}


std::shared_ptr<ast::Expr> parse_array_literal(parser *p) {
    p->expect(tok_big_para_open, p);
    std::vector<std::shared_ptr<ast::Expr>> array;
    std::shared_ptr<ast::Expr> first = parse_expr(p, default_bp);
    std::string first_type;

    if (auto sym = std::dynamic_pointer_cast<ast::SymbolExpr>(first)) {
        auto it = symtab.find(sym->symbol);
        if (it == symtab.end())
            throw std::runtime_error("Undefined variable in array literal: " + sym->symbol);
        first_type = it->second->type_name();
    } else {
        first_type = infer_type_from_expr(first)->type_name();
    }

    array.push_back(first);

    while (p->hastokens(p) && p->currentToken(p).first != tok_big_para_close) {
        if (p->currentToken(p).first != tok_eof && p->currentToken(p).first != tok_big_para_close)
            p->expect(tok_comma, p);

        std::shared_ptr<ast::Expr> next = parse_expr(p, default_bp);
        std::string next_type;

        if (auto sym = std::dynamic_pointer_cast<ast::SymbolExpr>(next)) {
            auto it = symtab.find(sym->symbol);
            if (it == symtab.end())
                throw std::runtime_error("Undefined variable in array literal: " + sym->symbol);
            next_type = it->second->type_name();
        } else {
            next_type = infer_type_from_expr(next)->type_name();
        }

        if (next_type != first_type) {
            throw std::runtime_error("Array element type mismatch: expected " + first_type + " but got " + next_type);
        }

        array.push_back(next);
    }

    p->expect(tok_big_para_close, p);
    return std::make_shared<ast::ArrayLiteral>(array);
}
std::shared_ptr<ast::stmt> parse_if_else(parser *p) {
    p->AdvanceToken(p);
    std::shared_ptr<ast::stmt> alternate;

    auto condition = parse_expr(p, assignment);
    auto condition_type = infer_type_from_expr(condition);
    if (condition_type->type_name() != "bool") {
        throw std::runtime_error("Condition in 'if' must be bool: " + condition_type->type_name());
    }
    // Check if 'condition' is a variable reference
    if (auto var_expr = std::dynamic_pointer_cast<ast::VarDecStmt>(condition)) {
        std::string var_name = var_expr->var_name;

        if (symtab.find(var_name) != symtab.end()) {
            auto declared_type = symtab[var_name];

            if (declared_type != condition_type) {
                throw std::runtime_error("Type mismatch in if condition: variable type does not match expression type.");
            }
        }
    }

    auto consequent = parse_block(p);

    if (p->currentToken(p).first == tok_else) {
        p->AdvanceToken(p);
        if (p->currentToken(p).first == tok_if) {
            alternate = parse_if_else(p);
        } else {
            alternate = parse_block(p);
        }
    }

    return std::make_shared<ast::IfStmt>(alternate, consequent, condition);
}
std::shared_ptr<ast::stmt> parse_while(parser *p) {
    p->AdvanceToken(p);
    auto condition = parse_expr(p, assignment);
    auto condition_type = infer_type_from_expr(condition);
    if (condition_type->type_name() != "bool") {
        throw std::runtime_error("Condition in 'while' must be bool: " + condition_type->type_name());
    }
    // Check if 'condition' is a variable reference
    if (auto var_expr = std::dynamic_pointer_cast<ast::VarDecStmt>(condition)) {
        std::string var_name = var_expr->var_name;

        if (symtab.find(var_name) != symtab.end()) {
            auto declared_type = symtab[var_name];

            if (declared_type != condition_type) {
                throw std::runtime_error("Type mismatch in while condition: variable type does not match expression type.");
            }
        }
    }
    auto consequent = parse_block(p, true);

    return std::make_shared<ast::WhileStmt>(condition, consequent);
}
std::shared_ptr<ast::stmt> parse_for(parser *p) {
    p->AdvanceToken(p);
    auto var_token = p->expect(tok_identifier, p).second;
    p->expect(tok_in, p);
    auto expr = parse_expr(p, default_bp);
    auto check_type = infer_type_from_expr(expr);
    if (!std::dynamic_pointer_cast<ast::ArrayType>(check_type))
        throw std::runtime_error("Only Arrays are iterable");
    auto body = parse_block(p, true);

    return  std::make_shared<ast::ForStmt>(var_token, expr, body);
}
std::shared_ptr<ast::stmt> parse_return(parser* p) {
    p->AdvanceToken(p);
    std::shared_ptr<ast::Expr> return_expr = nullptr;
    std::shared_ptr<ast::types> return_type;
    if (p->currentToken(p).first != tok_semicolon)
    {
        return_expr = parse_expr(p, default_bp);
        return_type = infer_type_from_expr(return_expr);
    } else {
        return_type = std::make_shared<ast::SymbolType>("void");
    }

    p->expect(tok_semicolon, p);

    return std::make_shared<ast::ReturnStmt>(return_expr, return_type);
}

std::shared_ptr<ast::stmt> parse_function(parser *p) {
    p->expect(tok_def, p);

    auto function_name = p->expect(tok_identifier, p).second;

    std::vector<std::shared_ptr<ast::ParameterStmt>> parameters;

    p->expect(tok_open_bracket, p);

    while (p->currentToken(p).first != tok_close_bracket) {
        auto name = p->expect(tok_identifier, p).second;
        p->expect(tok_column, p);
        auto type = parse_types(p, default_bp);
        p->AdvanceToken(p);
        if (p->currentToken(p).first != tok_close_bracket) {
            p->expect(tok_comma, p);
        }
        parameters.push_back(std::make_shared<ast::ParameterStmt>(name, type));
    }
    std::cout << "worked " << parameters.front()->var_name << " " << parameters.front()->type->type_name() << std::endl;
    p->expect(tok_close_bracket, p);

    p->expect(tok_column, p);
    auto return_type = parse_types(p, default_bp);
    p->AdvanceToken(p);

    bool has_return = false;
    auto body = parse_block(p, true);
    auto block = std::dynamic_pointer_cast<ast::BlockStmt>(body);
    for (auto c: block->body) {
        if (auto r = std::dynamic_pointer_cast<ast::ReturnStmt>(c)) {
            has_return = true;
            if (r->type->type_name() != return_type->type_name()) {
                throw std::runtime_error("Return type mismatch in 'function' statement");
            }
        }
    }
    if (!has_return) {
        throw std::runtime_error("No Return in function");
    }
    return std::make_shared<ast::FunctionStmt>(parameters, function_name, body, return_type);

}
std::shared_ptr<ast::Expr> parse_function_expr(parser *p) {
    p->expect(tok_def, p);
    std::vector<std::shared_ptr<ast::ParameterStmt>> parameters;

    p->expect(tok_open_bracket, p);

    while (p->currentToken(p).first != tok_close_bracket) {
        auto name = p->expect(tok_identifier, p).second;
        p->expect(tok_column, p);
        auto type = parse_types(p, default_bp);
        p->AdvanceToken(p);
        if (p->currentToken(p).first != tok_close_bracket) {
            p->expect(tok_comma, p);
        }
        parameters.push_back(std::make_shared<ast::ParameterStmt>(name, type));
    }
    std::cout << "worked " << parameters.front()->var_name << " " << parameters.front()->type->type_name() << std::endl;
    p->expect(tok_close_bracket, p);

    p->expect(tok_column, p);
    auto return_type = parse_types(p, default_bp);
    p->AdvanceToken(p);

    bool has_return = false;
    auto body = parse_block(p, true);
    auto block = std::dynamic_pointer_cast<ast::BlockStmt>(body);
    for (auto c: block->body) {
        if (auto r = std::dynamic_pointer_cast<ast::ReturnStmt>(c)) {
            has_return = true;
            if (r->type->type_name() != return_type->type_name()) {
                throw std::runtime_error("Return type mismatch in 'function' statement");
            }
        }
    }
    if (!has_return) {
        throw std::runtime_error("No Return in function");
    }
    return std::make_shared<ast::FunctionExpr>(parameters, body, return_type);
}
std::shared_ptr<ast::Expr> parse_call_expr(parser *p, std::shared_ptr<ast::Expr> left, binding_power bp) {
    p->AdvanceToken(p);
    std::vector<std::shared_ptr<ast::Expr>> arguments;

    while (p->currentToken(p).first != tok_close_bracket) {
        arguments.push_back(parse_expr(p, assignment));
        p->AdvanceToken(p);
        if (p->currentToken(p).first != tok_close_bracket) {
            p->expect(tok_comma, p);
        }
    }
    p->expect(tok_close_bracket, p);
    return std::make_shared<ast::CallExpr>(arguments, left);
}
std::shared_ptr<ast::Expr> parse_member_expr(parser *p, std::shared_ptr<ast::Expr> left, binding_power bp) {

    if (p->AdvanceToken(p).first == tok_big_para_open) {
        auto rhs = parse_expr(p, bp);
        p->expect(tok_big_para_open, p);
        return std::make_shared<ast::CompoundExpr>(left, rhs);
    }
    return std::make_shared<ast::MemberExpr>(left, p->expect(tok_identifier, p).second);
}
std::shared_ptr<ast::Expr> parse_new(parser *p) {
    p->AdvanceToken(p);
    auto expr = parse_expr(p, default_bp);

    auto brr = std::dynamic_pointer_cast<ast::CallExpr>(expr);
    if (!brr)
        throw std::runtime_error("parse_new() failed");

    return std::make_shared<ast::NewExpr>(brr);
}
std::shared_ptr<ast::Expr> parse_grouping_expr(parser *p) {
    p->expect(tok_open_bracket, p);
    auto expr = parse_expr(p, default_bp);
    p->expect(tok_close_bracket, p);
    return expr;
}
void createTokenTypeLookups(void) {
    type_nud(Tokens::tok_string_type, parse_symbol_type);
    type_nud(Tokens::tok_int, parse_symbol_type);
    type_nud(Tokens::tok_char, parse_symbol_type);
    type_nud(Tokens::tok_float, parse_symbol_type);
    type_nud(Tokens::tok_bool, parse_symbol_type);
    type_nud(Tokens::tok_void, parse_symbol_type);
    type_nud(Tokens::tok_big_para_open, parse_array_type);
}
void createTokenLookups(void) {
    led(Tokens::tok_equal, assignment, parse_assignment);
    led(Tokens::tok_plus_equal, assignment, parse_assignment);
    led(Tokens::tok_minus_equal, assignment, parse_assignment);
    led(Tokens::tok_divide_equal, assignment, parse_assignment);
    led(Tokens::tok_multiply_equal, assignment, parse_assignment);
    led(Tokens::tok_percent_equal, assignment, parse_assignment);

    led(Tokens::tok_and, logical, parse_binary_expr);
    led(Tokens::tok_or, logical, parse_binary_expr);
    led(Tokens::tok_dot_dot, logical, parse_range_expr);

    led(Tokens::tok_smaller_than, relational, parse_binary_expr);
    led(Tokens::tok_greater_than, relational, parse_binary_expr);
    led(Tokens::tok_is_greater_or_equal_to, relational, parse_binary_expr);
    led(Tokens::tok_is_small_or_equal_to, relational, parse_binary_expr);
    led(Tokens::tok_is_equal_to, equality, parse_binary_expr);
    led(Tokens::tok_dosent_equal_to, equality, parse_binary_expr);

    led(Tokens::tok_plus, additive, parse_binary_expr);
    led(Tokens::tok_minus, additive, parse_binary_expr);
    led(Tokens::tok_multiple, multiplicative, parse_binary_expr);
    led(Tokens::tok_divide, multiplicative, parse_binary_expr);
    led(Tokens::tok_percent, multiplicative, parse_binary_expr);
    led(Tokens::tok_arrow, member, parse_member_expr);
    led(Tokens::tok_big_para_open, member, parse_member_expr);
    led(Tokens::tok_open_bracket, call, parse_call_expr);

    nud(Tokens::tok_open_bracket, default_bp, parse_grouping_expr);
    nud(Tokens::tok_def, default_bp, parse_function_expr);
    nud(Tokens::tok_new, default_bp, parse_new);
    nud(Tokens::tok_num, primary, parse_primary_expr);
    nud(Tokens::tok_float_num, primary, parse_primary_expr);
    nud(Tokens::tok_string_literal, primary, parse_primary_expr);
    nud(Tokens::tok_false, default_bp, parse_primary_expr);
    nud(Tokens::tok_true, default_bp, parse_primary_expr);
    nud(Tokens::tok_char_var, primary, parse_primary_expr);
    nud(Tokens::tok_identifier, primary, parse_primary_expr);
    nud(Tokens::tok_open_bracket, primary, parse_paren_expr);
    nud(Tokens::tok_not, unary, parse_prefix);
    nud(Tokens::tok_big_para_open, unary, parse_array_literal);

    stmt(Tokens::tok_const, default_bp, parse_var);
    stmt(Tokens::tok_let, default_bp, parse_var);
    stmt(Tokens::tok_class, default_bp, parse_class_dec_stmt);
    stmt(Tokens::tok_enum, default_bp, parse_enum_dec_stmt);
    stmt(Tokens::tok_import, default_bp, parse_import);
    stmt(Tokens::tok_if, default_bp, parse_if_else);
    stmt(Tokens::tok_while_loop, default_bp, parse_while);
    stmt(Tokens::tok_for_loop, default_bp, parse_for);
    stmt(Tokens::tok_def, default_bp, parse_function);
    stmt(Tokens::tok_return, default_bp, parse_return);
}  
parser* CreateParser(std::vector<Token> tokens)
{
    createTokenTypeLookups();
    createTokenLookups();
    return new parser(tokens);
}
ast::BlockStmt Parse (std::vector<Token> tokens) {
    std::vector<std::shared_ptr<ast::stmt>> Body;
    auto p = CreateParser(tokens);

    while (p->hastokens(p)) {
        Body.push_back(parse_stmt(p));
    }
    ast::BlockStmt bstmt(Body);
    return bstmt;
}
