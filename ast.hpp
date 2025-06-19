#pragma once
#include <iostream>
#include <memory>
#include <string>
#include <vector>
#include <typeinfo>
#include <stdexcept>
//NODE
namespace ast {
//TYPES
struct types {
public:
    virtual ~types() = default;
    virtual std::string type_name() const = 0;
};

struct SymbolType : public types {
    std::string name;
    SymbolType(std::string name) : name(name) {}
    std::string type_name() const override {
        return name;
    }
};

struct ArrayType : public types {
    std::shared_ptr<types> type;
    ArrayType(std::shared_ptr<types> type) : type(type) {}
    std::string type_name() const override {
        return "[] " + type->type_name();
    }
};

//EXPR
enum class ExprType {
    Number,
    Bool,
    Float,
    Char,
    String,
    Symbol,
    Binary,
    Assignement,
    Prefix,
    Array,
    Range,
    Member,
    Call,
    Compound,
    NewE,
    Function,
    Unknown
};

struct Expr {
    virtual ~Expr() = default;
    virtual void dump(std::ostream& os, int indent = 0) const = 0;
    virtual ExprType get_type() const { return ExprType::Unknown; }
};

inline std::string indent_str(int indent) {
    return std::string(indent, ' ');
}

struct NumberExpr : public Expr {
    int64_t num;
    bool is_negitive_or_zero = false;
    NumberExpr(int v) : num(v) {
        if (num <= 0)
            is_negitive_or_zero = true;
    }
    ExprType get_type() const override { return ExprType::Number; }

    void dump(std::ostream& os, int indent = 0) const override {
        os << indent_str(indent) << "NumberExpr(" << num << ")\n";
    }
};

struct BoolExpr : public Expr {
    bool num;
    BoolExpr(bool v) : num(v) {}
    ExprType get_type() const override { return ExprType::Bool; }

    void dump(std::ostream& os, int indent = 0) const override {
        os << indent_str(indent) << "BoolExpr(" << num << ")\n";
    }
};

struct FloatExpr : public Expr {
    float num;
    FloatExpr(float v) : num(v) {}
    ExprType get_type() const override { return ExprType::Float; }

    void dump(std::ostream& os, int indent = 0) const override {
        os << indent_str(indent) << "FloatExpr(" << num << ")\n";
    }
};

struct CharExpr : public Expr {
    char value;
    CharExpr(char v) : value(v) {}
    ExprType get_type() const override { return ExprType::Char; }

    void dump(std::ostream& os, int indent = 0) const override {
        os << indent_str(indent) << "CharExpr('" << value << "')\n";
    }
};

struct StringExpr : public Expr {
    std::string string;
    StringExpr(const std::string& s) : string(s) {}
    ExprType get_type() const override { return ExprType::String; }

    void dump(std::ostream& os, int indent = 0) const override {
        os << indent_str(indent) << "\"" << string << "\"\n";
    }
};

struct SymbolExpr : public Expr {
    std::string symbol;
    SymbolExpr(const std::string& s) : symbol(s) {}
    ExprType get_type() const override { return ExprType::Symbol; }

    void dump(std::ostream& os, int indent = 0) const override {
        os << indent_str(indent) << "SymbolExpr(" << symbol << ")\n";
    }
};

struct BinaryExpr : public Expr {
    std::shared_ptr<Expr> left;
    std::string opreator;
    std::shared_ptr<Expr> right;

    BinaryExpr(std::shared_ptr<Expr> l, std::string op, std::shared_ptr<Expr> r)
        : left(l), opreator(op), right(r) {}
    ExprType get_type() const override { return ExprType::Binary; }

    void dump(std::ostream& os, int indent = 0) const override {
        os << indent_str(indent) << "BinaryExpr(\n";
        left->dump(os, indent + 2);
        os << indent_str(indent + 2) << "Operator: \"" << opreator << "\"\n";
        right->dump(os, indent + 2);
        os << indent_str(indent) << ")\n";
    }
};

struct AssignementExpr : public Expr {
    std::string opreator;
    std::shared_ptr<Expr> value;
    std::shared_ptr<Expr> assigne;

    AssignementExpr(std::string opreator, std::shared_ptr<Expr> value, std::shared_ptr<Expr> assigne)
        : opreator(opreator), value(value), assigne(assigne) {}
    ExprType get_type() const override { return ExprType::Assignement; }

    void dump(std::ostream& os, int indent = 0) const override {
        os << indent_str(indent) << "AssignementExpr(\n";
        value->dump(os, 6);
        os << "      Operator: \"" << opreator << "\",\n";
        assigne->dump(os, 6);
        os << ")\n";
    }
};

struct PrefixExpr : public Expr {
    std::string op;
    std::shared_ptr<Expr> expr;

    PrefixExpr(const std::string& op_, std::shared_ptr<ast::Expr> expr_)
        : op(op_), expr(std::move(expr_)) {}
    ExprType get_type() const override { return ExprType::Prefix; }

    void dump(std::ostream& os, int indent = 0) const override {
        os << indent_str(indent) << "PrefixExpr(\n";
        os << indent_str(indent + 2) << "Operator: \"" << op << "\"\n";
        expr->dump(os, indent + 2);
        os << indent_str(indent) << ")\n";
    }
};
inline std::string to_string(ExprType type) {
    switch (type) {
        case ExprType::Number:      return "Number";
        case ExprType::Bool:        return "Bool";
        case ExprType::Float:       return "Float";
        case ExprType::Char:        return "Char";
        case ExprType::String:      return "String";
        case ExprType::Symbol:      return "Symbol";
        case ExprType::Binary:      return "Binary";
        case ExprType::Assignement: return "Assignement";
        case ExprType::Prefix:      return "Prefix";
        case ExprType::Array:       return "Array";
        case ExprType::Range:       return "Range";
        case ExprType::Call:        return "Call";
        case ExprType::NewE:        return "New";
        case ExprType::Member:      return "Member";
        case ExprType::Function:    return "Function";
        default:                    return "Unknown";
    }
}

struct ArrayLiteral : public Expr {
    std::vector<std::shared_ptr<Expr>> expr;
    std::string type;
    ArrayLiteral(std::vector<std::shared_ptr<Expr>> expr) : expr(expr) {
        type = to_string(expr.back()->get_type());
    }
    ExprType get_type() const override { return ExprType::Array; }

    void dump(std::ostream& os, int indent = 0) const override {
        os << indent_str(indent) << "ArrayLiteral(\n";
        for (const auto& e : expr)
            e->dump(os, indent + 2);
        os << indent_str(indent) << ")\n";
    }
};
struct RangeExpr : Expr {
    std::shared_ptr<Expr> start;
    std::shared_ptr<Expr> end;

    RangeExpr(std::shared_ptr<Expr> start, std::shared_ptr<Expr> end)
        : start(start), end(end) {}
    ExprType get_type() const override { return ExprType::Range; }
    void dump(std::ostream& os, int indent = 0) const override {
        os << indent_str(indent) << "RangeExpr(\n";
        start->dump(os, indent + 2);
        end->dump(os, indent + 2);
        os << indent_str(indent) << ")\n";
    }

};
struct MemberExpr : public Expr {
    std::shared_ptr<Expr> member;
    std::string property;

    MemberExpr(std::shared_ptr<Expr> member, std::string property)
        : member(std::move(member)), property(std::move(property)) {}

    ExprType get_type() const override { return ExprType::Member; }

    void dump(std::ostream& os, int indent = 0) const override {
        std::string ind = indent_str(indent);
        os << ind << "MemberExpr:\n";
        if (member) member->dump(os, indent + 2);
        os << indent_str(indent + 2) << "property: " << property << "\n";
    }
};

struct CallExpr : public Expr {
    std::vector<std::shared_ptr<Expr>> arguments;
    std::shared_ptr<Expr> meathod;

    CallExpr(std::vector<std::shared_ptr<Expr>> arguments, std::shared_ptr<Expr> meathod)
        : arguments(std::move(arguments)), meathod(std::move(meathod)) {}

    ExprType get_type() const override { return ExprType::Call; }

    void dump(std::ostream& os, int indent = 0) const override {
        std::string ind = indent_str(indent);
        os << ind << "CallExpr:\n";
        os << indent_str(indent + 2) << "method:\n";
        if (meathod) meathod->dump(os, indent + 4);
        os << indent_str(indent + 2) << "arguments:\n";
        for (const auto& arg : arguments) {
            if (arg) arg->dump(os, indent + 4);
        }
    }
};

struct CompoundExpr : public Expr {
    std::shared_ptr<Expr> member;
    std::shared_ptr<Expr> Property;

    CompoundExpr(std::shared_ptr<Expr> member, std::shared_ptr<Expr> Property)
        : member(std::move(member)), Property(std::move(Property)) {}

    ExprType get_type() const override { return ExprType::Compound; }

    void dump(std::ostream& os, int indent = 0) const override {
        std::string ind = indent_str(indent);
        os << ind << "CompoundExpr:\n";
        os << indent_str(indent + 2) << "member:\n";
        if (member) member->dump(os, indent + 4);
        os << indent_str(indent + 2) << "property:\n";
        if (Property) Property->dump(os, indent + 4);
    }
};

struct NewExpr : public Expr {
    std::shared_ptr<CallExpr> instantiation;

    NewExpr(std::shared_ptr<CallExpr> instantiation)
        : instantiation(std::move(instantiation)) {}

    ExprType get_type() const override { return ExprType::NewE; }

    void dump(std::ostream& os, int indent = 0) const override {
        std::string ind = indent_str(indent);
        os << ind << "NewExpr:\n";
        if (instantiation) instantiation->dump(os, indent + 2);
    }
};
//STATEMENTS
struct stmt
{
public:
    virtual void dump(std::ostream& os, int indent = 0) const = 0;
    virtual ~stmt() = default;
};

struct BlockStmt : public stmt {
    std::vector<std::shared_ptr<stmt>> body;
    BlockStmt(std::vector<std::shared_ptr<stmt>> Body) : body(Body) {}
    void dump(std::ostream& os, int indent = 0) const override {
        std::string pad(indent, ' ');
        os << pad << "BlockStmt(\n";
        for (const auto& stmt : body) {
            if (stmt)
                stmt->dump(os, indent + 2);
            else
                os << pad << "  nullptr\n";
        }
        os << pad << ")\n";
    }
};
struct ExprStmt : public stmt {
    std::shared_ptr<Expr> expr;

    ExprStmt(std::shared_ptr<Expr> expr) : expr(expr) {}
    void dump(std::ostream& os, int indent) const override {
        os << std::string(indent, ' ') << "ExprStmt\n";
        if (expr) {
            expr->dump(os, indent + 2);
        }
    }
};
struct VarDecStmt : public stmt {
    std::shared_ptr<types> ExplicitType;
    std::shared_ptr<Expr> expr; //assigned value
    std::string var_name;
    bool is_const;
    void dump(std::ostream& os, int indent) const override {
        os << "     VarDecStmt(\n";
        os << "          " << (is_const ? "const " : "") << "let " << var_name;

        if (ExplicitType)
            os << ": " << ExplicitType->type_name();
        else
            os << ": <inferred?>";

        os << " = ";

        if (expr)
            expr->dump(os, indent);
        else
            os << "NaN";

        os << "          \n)";
    }
    VarDecStmt(std::shared_ptr<Expr> expr, std::string var_name, bool is_const, std::shared_ptr<types> ExplicitType) : expr(expr), is_const(is_const), var_name(var_name), ExplicitType(ExplicitType) {}
};
struct ImportStmt : public stmt {
    std::string fileName;

    ImportStmt(std::string fileName) : fileName(std::move(fileName)) {}

    void dump(std::ostream& os, int indent = 0) const override {
        std::string ind(indent, ' ');
        os << ind << "import \"" << fileName << "\";\n";
    }
};
struct ClassDecStmt : public stmt {
    std::string ClassName;
    std::vector<std::shared_ptr<stmt>> body;

    ClassDecStmt(std::string name, std::vector<std::shared_ptr<stmt>> body)
        : ClassName(std::move(name)), body(std::move(body)) {}

    void dump(std::ostream& os, int indent = 0) const override {
        std::string ind(indent, ' ');
        os << ind << "class " << ClassName << " {\n";
        for (const auto& s : body) {
            if (s) s->dump(os, indent + 2);
        }
        os << ind << "};\n";
    }
};
struct EnumDecStmt : public stmt {
    std::string EnumName;
    std::vector<std::shared_ptr<VarDecStmt>> numbers;
    EnumDecStmt(std::string EnumName, std::vector<std::shared_ptr<VarDecStmt>> numbers) : EnumName(EnumName), numbers(numbers) {}
    void dump(std::ostream& os, int indent = 0) const override {
        std::string ind(indent, ' ');
        os << ind << "EnumDecStmt: " << EnumName << "\n";
        for (const auto& var : numbers) {
            var->dump(os, indent + 2);
        }
    }

};
struct IfStmt : public stmt {
    std::shared_ptr<stmt> alternate;
    std::shared_ptr<stmt> consequent;
    std::shared_ptr<Expr> condition;
    IfStmt(std::shared_ptr<stmt> alternate, std::shared_ptr<stmt> consequent, std::shared_ptr<Expr> condition) : condition(condition), alternate(alternate), consequent(consequent
    ) {}
    void dump(std::ostream& os, int indent = 0) const override {
        std::string ind(indent, ' ');
        os << ind << "IfStmt:\n";

        os << ind << "  Condition:\n";
        if (condition) condition->dump(os, indent + 4);
        else os << ind << "    (null)\n";

        os << ind << "  Consequent:\n";
        if (consequent) consequent->dump(os, indent + 4);
        else os << ind << "    (null)\n";

        os << ind << "  Alternate:\n";
        if (alternate) alternate->dump(os, indent + 4);
        else os << ind << "    (null)\n";
    }

};


struct WhileStmt : public stmt {
    std::shared_ptr<Expr> condition;
    std::shared_ptr<stmt> body;

    WhileStmt(std::shared_ptr<Expr> condition, std::shared_ptr<stmt> body)
        : condition(condition), body(body) {}

    void dump(std::ostream& os, int indent = 0) const override {
        std::string ind(indent, ' ');
        os << ind << "WhileStmt:\n";

        os << ind << "  Condition:\n";
        if (condition)
            condition->dump(os, indent + 4);
        else
            os << ind << "    (null)\n";

        os << ind << "  Body:\n";
        if (body)
            body->dump(os, indent + 4);
        else
            os << ind << "    (null)\n";
    }
};
struct BreakStmt : public stmt {
    void dump(std::ostream& os, int indent = 0) const override {
        std::string ind(indent, ' ');
        os << ind << "BreakStmt\n";
    }
};

struct ContinueStmt : public stmt {
    void dump(std::ostream& os, int indent = 0) const override {
        std::string ind(indent, ' ');
        os << ind << "ContinueStmt\n";
    }
};
struct ForStmt : public stmt {
    std::string var_name;
    std::shared_ptr<Expr> iterable;
    std::shared_ptr<stmt> body;

    ForStmt(const std::string& var, std::shared_ptr<Expr> iter, std::shared_ptr<stmt> body)
        : var_name(var), iterable(iter), body(body) {}

    void dump(std::ostream& os, int indent = 0) const override {
        std::string ind(indent, ' ');
        os << ind << "ForEachStmt:\n";
        os << ind << "  Var: " << var_name << "\n";
        os << ind << "  Iterable:\n";
        iterable->dump(os, indent + 4);
        os << ind << "  Body:\n";
        body->dump(os, indent + 4);
    }
};
struct ParameterStmt : public stmt {
    std::string var_name;
    std::shared_ptr<types> type;

    ParameterStmt(const std::string& var, std::shared_ptr<types> type) : var_name(var), type(type) {}
    void dump(std::ostream& os, int indent = 0) const override {
        std::string ind(indent, ' ');
        os << ind << "ParameterStmt:\n";
        os << ind << "  Name: " << var_name << "\n";
        os << ind << "  Type: ";
        if (type) {
            os << type->type_name() << "\n";
        } else {
            os << "(unknown)\n";
        }
    }

};
struct FunctionStmt : public stmt {
    std::vector<std::shared_ptr<ParameterStmt>> parameters;
    std::string name;
    std::shared_ptr<stmt> body;
    std::shared_ptr<types> return_type;
    FunctionStmt(std::vector<std::shared_ptr<ParameterStmt>> parameters,std::string name, std::shared_ptr<stmt> body, std::shared_ptr<types> return_type) : name(name), parameters(parameters), body(body), return_type(return_type) {}
    void dump(std::ostream& os, int indent = 0) const override {
        std::string ind(indent, ' ');
        os << ind << "FunctionStmt:\n";
        os << ind << "  Name: " << name << "\n";

        os << ind << "  Parameters:\n";
        if (parameters.empty()) {
            os << ind << "    (none)\n";
        } else {
            for (const auto& param : parameters) {
                if (param) param->dump(os, indent + 4);
                else os << ind << "    (null)\n";
            }
        }

        os << ind << "  Return Type:\n";
        if (return_type) {
            os << ind << "    " << return_type->type_name() << "\n";
        } else {
            os << ind << "    (void or unknown)\n";
        }

        os << ind << "  Body:\n";
        if (body) {
            body->dump(os, indent + 4);
        } else {
            os << ind << "    (null)\n";
        }
    }

};
struct ReturnStmt : public stmt {
    std::shared_ptr<Expr> value;  // can be null for 'return;'
    std::shared_ptr<types> type;

    ReturnStmt(std::shared_ptr<Expr> value, std::shared_ptr<types> type)  : value(value), type(type) {}

    void dump(std::ostream& os, int indent = 0) const override {
        std::string ind(indent, ' ');
        os << ind << "ReturnStmt:\n";
        if (value) {
            os << ind << "  Value:\n";
            value->dump(os, indent + 4);
        } else {
            os << ind << "  (void)\n";
        }
        os << ind << "  Type: " << type->type_name();
    }
};
struct FunctionExpr : public Expr {
    std::vector<std::shared_ptr<ParameterStmt>> parameters;
    std::shared_ptr<stmt> body;
    std::shared_ptr<types> return_type;

    FunctionExpr(std::vector<std::shared_ptr<ParameterStmt>> parameters, std::shared_ptr<stmt> body, std::shared_ptr<types> return_type)
        : parameters(std::move(parameters)), body(std::move(body)), return_type(std::move(return_type)) {}

    ExprType get_type() const override { return ExprType::Function; }

    void dump(std::ostream& os, int indent = 0) const override {
        std::string ind = indent_str(indent);
        os << ind << "FunctionExpr:\n";

        os << indent_str(indent + 2) << "parameters:\n";
        for (const auto& param : parameters) {
            if (param) param->dump(os, indent + 4);
        }

        os << indent_str(indent + 2) << "return_type: ";
        if (return_type) {
            os << return_type->type_name() << "\n";
        } else {
            os << "void\n";
        }

        os << indent_str(indent + 2) << "body:\n";
        if (body) body->dump(os, indent + 4);
    }
};
}
