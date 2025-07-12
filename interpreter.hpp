#pragma once
#include <iostream>
#include <memory>
#include <string>
#include <vector>
#include <variant>
#include "ast.hpp"
#include "parser.hpp"
std::vector<std::string> names__;
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
	    } else if (std::holds_alternative<char>(lhs) && std::holds_alternative<char>(rhs)) {
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
else if (expr->get_type() == ast::ExprType::Assignement) {
    auto assign = std::static_pointer_cast<ast::AssignementExpr>(expr);
    auto sym = std::dynamic_pointer_cast<ast::SymbolExpr>(assign->assigne);
    auto it = std::find_if(symtab.begin(), symtab.end(), [&](Variable& var) { return var.name == sym->symbol; });
    num rhs_val = eval_expr(assign->value);
    const std::string& op = assign->opreator;
    if (op == "=") {
        it->value = rhs_val;
        return rhs_val;
    }
    num& lhs_val = it->value;
    if (std::holds_alternative<long long>(lhs_val) && std::holds_alternative<long long>(rhs_val)) {
        long long& lhs = std::get<long long>(lhs_val);
        long long rhs = std::get<long long>(rhs_val);

        if (op == "+=") lhs += rhs;
        else if (op == "-=") lhs -= rhs;
        else if (op == "*=") lhs *= rhs;
        else if (op == "/=") lhs /= rhs;
        else if (op == "%=") lhs %= rhs;
        else throw std::runtime_error("Unsupported compound assignment op: " + op);

        return lhs;
    }
    if ((std::holds_alternative<float>(lhs_val) || std::holds_alternative<long long>(lhs_val)) &&
        (std::holds_alternative<float>(rhs_val) || std::holds_alternative<long long>(rhs_val))) {

        float lhs = std::holds_alternative<float>(lhs_val) ? std::get<float>(lhs_val)
                                                           : static_cast<float>(std::get<long long>(lhs_val));
        float rhs = std::holds_alternative<float>(rhs_val) ? std::get<float>(rhs_val)
                                                           : static_cast<float>(std::get<long long>(rhs_val));

        if (op == "+=") lhs += rhs;
        else if (op == "-=") lhs -= rhs;
        else if (op == "*=") lhs *= rhs;
        else if (op == "/=") lhs /= rhs;
        else throw std::runtime_error("Unsupported compound assignment op: " + op);

        lhs_val = lhs;
        return lhs;
    }

    throw std::runtime_error("Unsupported types in assignment: " + op);
}

        return std::monostate{};
    }
    void eval_stmt(std::shared_ptr<ast::stmt> stmt) {
	if (auto block_stmt = std::dynamic_pointer_cast<ast::BlockStmt>(stmt)) {
		for (auto& s : block_stmt->body) {
			eval_stmt(s);
		}
		return;
	}
	else if (auto expr_stmt = std::dynamic_pointer_cast<ast::ExprStmt>(stmt)) {
		eval_expr(expr_stmt->expr);
		return;
	}
        else if (auto assign_stmt = std::dynamic_pointer_cast<ast::VarDecStmt>(stmt)) {
            auto it = std::find_if(symtab.begin(), symtab.end(), [&](Variable& var) { return var.name == assign_stmt->var_name; });
            num new_value = eval_expr(assign_stmt->expr);
	    it->value = new_value;
	    names__.push_back(assign_stmt->var_name);
            return;
        } else if (auto if_stmt = std::dynamic_pointer_cast<ast::IfStmt>(stmt)) {
	    bool cond = std::get<bool>(eval_expr(if_stmt->condition));
	    if (cond) {
		eval_stmt(if_stmt->consequent);
	    } else if (if_stmt->alternate) {
		eval_stmt(if_stmt->alternate);
	    }
	    return;
	}
    }


}

