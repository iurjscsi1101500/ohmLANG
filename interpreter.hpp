#pragma once
#include <iostream>
#include <memory>
#include <string>
#include <vector>
#include <variant>
#include "ast.hpp"
#include "parser.hpp"
namespace Interpreter {

    num eval_expr(std::shared_ptr<ast::Expr> expr) {
        if (expr->get_type() == ast::ExprType::Number)
            return std::static_pointer_cast<ast::NumberExpr>(expr)->num;

        if (expr->get_type() == ast::ExprType::Float)
            return std::static_pointer_cast<ast::FloatExpr>(expr)->num;

	if (expr->get_type() == ast::ExprType::Bool)
	    return std::static_pointer_cast<ast::BoolExpr>(expr)->num;

	if (expr->get_type() == ast::ExprType::String)
		return std::static_pointer_cast<ast::StringExpr>(expr)->string;

	if (expr->get_type() == ast::ExprType::Char)
		return std::static_pointer_cast<ast::CharExpr>(expr)->value;

	if (expr->get_type() == ast::ExprType::Symbol) {
		auto sym = std::static_pointer_cast<ast::SymbolExpr>(expr);
		auto it = std::find_if(symtab.begin(), symtab.end(), [&](Variable& var) { return var.name == sym->symbol; });
		return it->value;
	}
        if (expr->get_type() == ast::ExprType::Binary) {
            auto bin_expr = std::static_pointer_cast<ast::BinaryExpr>(expr);
            num lhs = eval_expr(bin_expr->left);
            num rhs = eval_expr(bin_expr->right);
            if (std::holds_alternative<float>(lhs) || std::holds_alternative<float>(rhs)) {
                float a = std::holds_alternative<float>(lhs) ? std::get<float>(lhs) : (float)std::get<int64_t>(lhs);
                float b = std::holds_alternative<float>(rhs) ? std::get<float>(rhs) : (float)std::get<int64_t>(rhs);

                if (strcmp(bin_expr->opreator.c_str(), "+") == 0) return a + b;
                if (strcmp(bin_expr->opreator.c_str(), "-") == 0) return a - b;
                if (strcmp(bin_expr->opreator.c_str(), "*") == 0) return a * b;
                if (strcmp(bin_expr->opreator.c_str(), "/") == 0) return a / b;
                if (strcmp(bin_expr->opreator.c_str(), "%") == 0) return std::fmod(a, b);
                if (strcmp(bin_expr->opreator.c_str(), "^") == 0) return std::pow(a, b);
		if (strcmp(bin_expr->opreator.c_str(), "<") == 0) return (bool)(a < b);
                if (strcmp(bin_expr->opreator.c_str(), "<=") == 0) return (bool)(a <= b);
                if (strcmp(bin_expr->opreator.c_str(), ">=") == 0) return (bool)(a >= b);
                if (strcmp(bin_expr->opreator.c_str(), ">") == 0) return (bool)(a > b);
		if (strcmp(bin_expr->opreator.c_str(), "==") == 0) return (bool)(a == b);
		if (strcmp(bin_expr->opreator.c_str(), "!=") == 0) return (bool)(a != b);
  
            } else if (std::holds_alternative<bool>(lhs) && std::holds_alternative<bool>(rhs)) {
                bool a = std::get<bool>(lhs);
                bool b = std::get<bool>(rhs);

                if (strcmp(bin_expr->opreator.c_str(), "&&") == 0) return (bool)(a && b);
                if (strcmp(bin_expr->opreator.c_str(), "||") == 0) return (bool)(a || b);
		if (strcmp(bin_expr->opreator.c_str(), "==") == 0) return (bool)(a == b);
		if (strcmp(bin_expr->opreator.c_str(), "!=") == 0) return (bool)(a != b);
            } else if (std::holds_alternative<std::string>(lhs) && std::holds_alternative<std::string>(rhs)) {
		std::string a = std::get<std::string>(lhs);
		std::string b = std::get<std::string>(rhs);

		if (strcmp(bin_expr->opreator.c_str(), "==") == 0) return (bool)(a == b);
		if (strcmp(bin_expr->opreator.c_str(), "!=") == 0) return (bool)(a != b);
	    } else if (std::holds_alternative<char>(lhs) && std::holds_alternative<char>(rhs) {
		char a = std::get<char>(lhs);
		char b = std::get<char>(rhs);

		if (strcmp(bin_expr->opreator.c_str(), "==") == 0) return (bool)(a == b);
		if (strcmp(bin_expr->opreator.c_str(), "!=") == 0) return (bool)(a != b);
	    } else {
                int64_t a = std::get<int64_t>(lhs);
                int64_t b = std::get<int64_t>(rhs);

                if (strcmp(bin_expr->opreator.c_str(), "+") == 0) return a + b;
                if (strcmp(bin_expr->opreator.c_str(), "-") == 0) return a - b;
                if (strcmp(bin_expr->opreator.c_str(), "*") == 0) return a * b;
                if (strcmp(bin_expr->opreator.c_str(), "/") == 0) return a / b;
                if (strcmp(bin_expr->opreator.c_str(), "%") == 0) return a % b;
                if (strcmp(bin_expr->opreator.c_str(), "^") == 0) return (int64_t)std::pow(a, b);
                if (strcmp(bin_expr->opreator.c_str(), "<") == 0) return (bool)(a < b);
                if (strcmp(bin_expr->opreator.c_str(), "<=") == 0) return (bool)(a <= b);
                if (strcmp(bin_expr->opreator.c_str(), ">=") == 0) return (bool)(a >= b);
                if (strcmp(bin_expr->opreator.c_str(), ">") == 0) return (bool)(a > b);
		if (strcmp(bin_expr->opreator.c_str(), "<") == 0) return a < b;
		if (strcmp(bin_expr->opreator.c_str(), "==") == 0) return (bool)(a == b);
		if (strcmp(bin_expr->opreator.c_str(), "!=") == 0) return (bool)(a != b);
            }

            throw std::runtime_error("Unknown operator");
        }

        return std::monostate{};
    }
    void eval_stmt(std::shared_ptr<ast::stmt> stmt) {
        if (auto assign_stmt = std::dynamic_pointer_cast<ast::VarDecStmt>(stmt)) {
            auto name = assign_stmt->var_name;
            auto it = std::find_if(symtab.begin(), symtab.end(), [&](Variable& var) { return var.name == name; });
            num new_value = eval_expr(assign_stmt->expr);
            it->value = new_value;
            return;
        }
    }


}

