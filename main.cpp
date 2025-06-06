#include "lexer.hpp"
#include "ast.hpp"
#include "parser.hpp"
#include <fstream>
#include <sstream>
#include <string>

std::string readFile(const std::string& path) {
    std::ifstream file(path);
    std::stringstream buffer;
    buffer << file.rdbuf();
    return buffer.str();
}
int main() {
    std::string lol = readFile("example.ohm");
    std::vector<Token> l = lexer(lol);
    //print_tokens(l);
    ast::BlockStmt ast = Parse(l);
    //ast.dump(std::cout);
}