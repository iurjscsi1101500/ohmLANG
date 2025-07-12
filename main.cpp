#include "lexer.hpp"
#include "ast.hpp"
#include "parser.hpp"
#include "interpreter.hpp"
#include <fstream>
#include <sstream>
#include <string>

std::vector<Variable> evaled_symtab;
void print_symtab(const std::vector<Variable>& symtab) {
    for (const auto& var : symtab) {
        std::cout << "Name: " << var.name << "\n";

        // Print type name (if available)
        if (var.type)
            std::cout << "Type: " << var.type->type_name() << "\n";
        else
            std::cout << "Type: <null>\n";

        // Print value
        std::cout << "Value: ";
        std::visit([](auto&& val) {
            using T = std::decay_t<decltype(val)>;
            if constexpr (std::is_same_v<T, std::monostate>) {
                std::cout << "<unset>";
            } else if constexpr (std::is_same_v<T, std::string>) {
                std::cout << '"' << val << '"';
            } else if constexpr (std::is_same_v<T, char>) {
                std::cout << '\'' << val << '\'';
            } else if constexpr (std::is_same_v<T, bool>) {
                std::cout << (val ? "true" : "false");
            } else {
                std::cout << val;
            }
        }, var.value);

        std::cout << "\n---\n";
    }
}


std::string readFile(const std::string& path) {
    std::ifstream file(path);
    std::stringstream buffer;
    buffer << file.rdbuf();
    return buffer.str();
}
int main() {
    std::string lol = readFile("example.ohm");
    std::vector<Token> l = lexer(lol);
    ast::BlockStmt ast_ = Parse(l);

    std::vector<num> l_;
    ast_.dump(std::cout);
    for (auto s: ast_.body) {
	if (auto i = std::dynamic_pointer_cast<ast::VarDecStmt>(s))
		Interpreter::eval_stmt(i);
	else if (auto f = std::dynamic_pointer_cast<ast::IfStmt>(s))
		Interpreter::eval_stmt(f);
    }
    for (auto s: names__) {
        auto it = std::find_if(symtab.begin(), symtab.end(), [&](const Variable& var) { return var.name == s; });
        evaled_symtab.push_back(*it);
    }
    print_symtab(evaled_symtab); 
    return 0;
}

