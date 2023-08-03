/*
 * Copyright (c) 2021, Itamar S. <itamar8910@gmail.com>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#include <optional>
#include <string_view>
#include "util.hh"
#include "lexer.hh"

namespace Cpp {

class ASTNode;
class TranslationUnit;
class Declaration;
class FunctionDefinition;
class Type;
class Parameter;
class Statement;
class Name;

class ASTNode : public intrusive_ref_counter<ASTNode> {
    public:
        virtual ~ASTNode() = default;
        virtual std::string_view class_name() const = 0;
        virtual void dump(FILE* = stdout, size_t indent = 0) const;

        template<typename T>
        bool fast_is() const = delete;

        ASTNode const* parent() const { return m_parent; }
        Position start() const
        {
            assert(m_start.has_value());
            return m_start.value();
        }
        Position end() const
        {
            assert(m_end.has_value());
            return m_end.value();
        }
        std::string const& filename() const
        {
            return m_filename;
        }
        void set_end(Position const& end) { m_end = end; }
        void set_parent(ASTNode const& parent) { m_parent = &parent; }

        virtual std::vector<intrusive_ptr<Declaration const>> declarations() const { return {}; }

        virtual bool is_identifier() const { return false; }
        virtual bool is_member_expression() const { return false; }
        virtual bool is_variable_or_parameter_declaration() const { return false; }
        virtual bool is_function_call() const { return false; }
        virtual bool is_type() const { return false; }
        virtual bool is_declaration() const { return false; }
        virtual bool is_name() const { return false; }
        virtual bool is_dummy_node() const { return false; }

    protected:
        ASTNode(ASTNode const* parent, std::optional<Position> start, std::optional<Position> end, std::string const& filename)
                : m_parent(parent)
                , m_start(start)
                , m_end(end)
                , m_filename(filename)
        {
        }

    private:
        ASTNode const* m_parent { nullptr };
        std::optional<Position> m_start;
        std::optional<Position> m_end;
        std::string m_filename;
    };

class TranslationUnit : public ASTNode {

public:
    virtual ~TranslationUnit() override = default;
    virtual std::string_view class_name() const override { return "TranslationUnit"; }
    virtual void dump(FILE* = stdout, size_t indent = 0) const override;
    virtual std::vector<intrusive_ptr<Declaration const>> declarations() const override { return m_declarations; }

    TranslationUnit(ASTNode const* parent, std::optional<Position> start, std::optional<Position> end, std::string const& filename)
            : ASTNode(parent, start, end, filename)
    {
    }

    void set_declarations(std::vector<intrusive_ptr<Declaration const>>&& declarations) { m_declarations = move(declarations); }

private:
    std::vector<intrusive_ptr<Declaration const>> m_declarations;
};

class Statement : public ASTNode {
public:
    virtual ~Statement() override = default;
    virtual std::string_view class_name() const override { return "Statement"; }

    virtual std::vector<intrusive_ptr<Declaration const>> declarations() const override;

protected:
    Statement(ASTNode const* parent, std::optional<Position> start, std::optional<Position> end, std::string const& filename)
            : ASTNode(parent, start, end, filename)
    {
    }
};

class Declaration : public Statement {

public:
    virtual bool is_declaration() const override { return true; }
    virtual bool is_variable_declaration() const { return false; }
    virtual bool is_parameter() const { return false; }
    virtual bool is_struct_or_class() const { return false; }
    virtual bool is_struct() const { return false; }
    virtual bool is_class() const { return false; }
    virtual bool is_function() const { return false; }
    virtual bool is_namespace() const { return false; }
    virtual bool is_enum() const { return false; }
    bool is_member() const { return parent() != nullptr && parent()->is_declaration() && assert_cast<Declaration>(parent())->is_struct_or_class(); }
    Name const* name() const { return m_name.get(); }
    std::string_view full_name() const;
    void set_name(intrusive_ptr<Name const> name) { m_name = std::move(name); }

protected:
    Declaration(ASTNode const* parent, std::optional<Position> start, std::optional<Position> end, std::string const& filename)
            : Statement(parent, start, end, filename)
    {
    }

    intrusive_ptr<Name const> m_name;
    mutable std::optional<std::string> m_full_name;
};

class InvalidDeclaration : public Declaration {

public:
    virtual ~InvalidDeclaration() override = default;
    virtual std::string_view class_name() const override { return "InvalidDeclaration"; }
    InvalidDeclaration(ASTNode const* parent, std::optional<Position> start, std::optional<Position> end, std::string const& filename)
            : Declaration(parent, start, end, filename)
    {
    }
};

class FunctionDeclaration : public Declaration {
public:
    virtual ~FunctionDeclaration() override = default;
    virtual std::string_view class_name() const override { return "FunctionDeclaration"; }
    virtual void dump(FILE* = stdout, size_t indent = 0) const override;
    virtual bool is_function() const override { return true; }
    virtual bool is_constructor() const { return false; }
    virtual bool is_destructor() const { return false; }
    intrusive_ptr<FunctionDefinition const> definition() { return m_definition; }

    FunctionDeclaration(ASTNode const* parent, std::optional<Position> start, std::optional<Position> end, std::string const& filename)
            : Declaration(parent, start, end, filename)
    {
    }

    virtual std::vector<intrusive_ptr<Declaration const>> declarations() const override;
    std::vector<std::string_view> const& qualifiers() const { return m_qualifiers; }
    void set_qualifiers(std::vector<std::string_view> const& qualifiers) { m_qualifiers = qualifiers; }
    Type const* return_type() const { return m_return_type.get(); }
    void set_return_type(intrusive_ptr<Type const> const& return_type) { m_return_type = return_type; }
    std::vector<intrusive_ptr<Parameter const>> const& parameters() const { return m_parameters; }
    void set_parameters(std::vector<intrusive_ptr<Parameter const>> const& parameters) { m_parameters = parameters; }
    FunctionDefinition const* definition() const { return m_definition.get(); }
    void set_definition(intrusive_ptr<FunctionDefinition const>&& definition) { m_definition = std::move(definition); }

private:
    std::vector<std::string_view> m_qualifiers;
    intrusive_ptr<Type const> m_return_type;
    std::vector<intrusive_ptr<Parameter const>> m_parameters;
    intrusive_ptr<FunctionDefinition const> m_definition;
};

class VariableOrParameterDeclaration : public Declaration {
public:
    virtual ~VariableOrParameterDeclaration() override = default;
    virtual bool is_variable_or_parameter_declaration() const override { return true; }

    void set_type(intrusive_ptr<Type const>&& type) { m_type =std::move(type); }
    Type const* type() const { return m_type.get(); }

protected:
    VariableOrParameterDeclaration(ASTNode const* parent, std::optional<Position> start, std::optional<Position> end, std::string const& filename)
            : Declaration(parent, start, end, filename)
    {
    }

    intrusive_ptr<Type const> m_type;
};

class Parameter : public VariableOrParameterDeclaration {
public:
    virtual ~Parameter() override = default;
    virtual std::string_view class_name() const override { return "Parameter"; }
    virtual void dump(FILE* = stdout, size_t indent = 0) const override;
    virtual bool is_parameter() const override { return true; }

    Parameter(ASTNode const* parent, std::optional<Position> start, std::optional<Position> end, std::string const& filename, intrusive_ptr<Name const> name)
            : VariableOrParameterDeclaration(parent, start, end, filename)
    {
        m_name = name;
    }

    bool is_ellipsis() const { return m_is_ellipsis; }
    void set_ellipsis(bool is_ellipsis) { m_is_ellipsis = is_ellipsis; }

private:
    bool m_is_ellipsis { false };
};

class Type : public ASTNode {
public:
    virtual ~Type() override = default;
    virtual std::string_view class_name() const override { return "Type"; }
    virtual bool is_type() const override { return true; }
    virtual bool is_templatized() const { return false; }
    virtual bool is_named_type() const { return false; }
    virtual std::string to_string() const = 0;
    virtual void dump(FILE* = stdout, size_t indent = 0) const override;

    bool is_auto() const { return m_is_auto; }
    void set_auto(bool is_auto) { m_is_auto = is_auto; }
    std::vector<std::string_view> const& qualifiers() const { return m_qualifiers; }
    void set_qualifiers(std::vector<std::string_view>&& qualifiers) { m_qualifiers =std::move(qualifiers); }

protected:
    Type(ASTNode const* parent, std::optional<Position> start, std::optional<Position> end, std::string const& filename)
            : ASTNode(parent, start, end, filename)
    {
    }

private:
    bool m_is_auto { false };
    std::vector<std::string_view> m_qualifiers;
};

class NamedType : public Type {
public:
    virtual ~NamedType() override = default;
    virtual std::string_view class_name() const override { return "NamedType"; }
    virtual std::string to_string() const override;
    virtual bool is_named_type() const override { return true; }

    NamedType(ASTNode const* parent, std::optional<Position> start, std::optional<Position> end, std::string const& filename)
            : Type(parent, start, end, filename)
    {
    }

    Name const* name() const { return m_name.get(); }
    void set_name(intrusive_ptr<Name const>&& name) { m_name =std::move(name); }

private:
    intrusive_ptr<Name const> m_name;
};

class Pointer : public Type {
public:
    virtual ~Pointer() override = default;
    virtual std::string_view class_name() const override { return "Pointer"; }
    virtual void dump(FILE* = stdout, size_t indent = 0) const override;
    virtual std::string to_string() const override;

    Pointer(ASTNode const* parent, std::optional<Position> start, std::optional<Position> end, std::string const& filename)
            : Type(parent, start, end, filename)
    {
    }

    Type const* pointee() const { return m_pointee.get(); }
    void set_pointee(intrusive_ptr<Type const>&& pointee) { m_pointee =std::move(pointee); }

private:
    intrusive_ptr<Type const> m_pointee;
};

class Reference : public Type {
public:
    virtual ~Reference() override = default;
    virtual std::string_view class_name() const override { return "Reference"; }
    virtual void dump(FILE* = stdout, size_t indent = 0) const override;
    virtual std::string to_string() const override;

    enum class Kind {
        Lvalue,
        Rvalue,
    };

    Reference(ASTNode const* parent, std::optional<Position> start, std::optional<Position> end, std::string const& filename, Kind kind)
            : Type(parent, start, end, filename)
            , m_kind(kind)
    {
    }

    Type const* referenced_type() const { return m_referenced_type.get(); }
    void set_referenced_type(intrusive_ptr<Type const>&& pointee) { m_referenced_type =std::move(pointee); }
    Kind kind() const { return m_kind; }

private:
    intrusive_ptr<Type const> m_referenced_type;
    Kind m_kind;
};

class FunctionType : public Type {
public:
    virtual ~FunctionType() override = default;
    virtual std::string_view class_name() const override { return "FunctionType"; }
    virtual void dump(FILE* = stdout, size_t indent = 0) const override;
    virtual std::string to_string() const override;

    FunctionType(ASTNode const* parent, std::optional<Position> start, std::optional<Position> end, std::string const& filename)
            : Type(parent, start, end, filename)
    {
    }

    void set_return_type(intrusive_ptr<Type const>&& type) { m_return_type = type; }
    void set_parameters(std::vector<intrusive_ptr<Parameter const>> parameters) { m_parameters =std::move(parameters); }

private:
    intrusive_ptr<Type const> m_return_type;
    std::vector<intrusive_ptr<Parameter const>> m_parameters;
};

class FunctionDefinition : public ASTNode {
public:
    virtual ~FunctionDefinition() override = default;
    virtual std::string_view class_name() const override { return "FunctionDefinition"; }
    virtual void dump(FILE* = stdout, size_t indent = 0) const override;

    FunctionDefinition(ASTNode const* parent, std::optional<Position> start, std::optional<Position> end, std::string const& filename)
            : ASTNode(parent, start, end, filename)
    {
    }

    virtual std::vector<intrusive_ptr<Declaration const>> declarations() const override;
    std::vector<intrusive_ptr<Statement const>> const& statements() { return m_statements; }
    void add_statement(intrusive_ptr<Statement const>&& statement) { m_statements.emplace_back(std::move(statement)); }

private:
    std::vector<intrusive_ptr<Statement const>> m_statements;
};

class InvalidStatement : public Statement {
public:
    virtual ~InvalidStatement() override = default;
    virtual std::string_view class_name() const override { return "InvalidStatement"; }
    InvalidStatement(ASTNode const* parent, std::optional<Position> start, std::optional<Position> end, std::string const& filename)
            : Statement(parent, start, end, filename)
    {
    }
};

class Expression : public Statement {
public:
    virtual ~Expression() override = default;
    virtual std::string_view class_name() const override { return "Expression"; }

protected:
    Expression(ASTNode const* parent, std::optional<Position> start, std::optional<Position> end, std::string const& filename)
            : Statement(parent, start, end, filename)
    {
    }
};

class InvalidExpression : public Expression {
public:
    virtual ~InvalidExpression() override = default;
    virtual std::string_view class_name() const override { return "InvalidExpression"; }
    InvalidExpression(ASTNode const* parent, std::optional<Position> start, std::optional<Position> end, std::string const& filename)
            : Expression(parent, start, end, filename)
    {
    }
};

class VariableDeclaration : public VariableOrParameterDeclaration {
public:
    virtual ~VariableDeclaration() override = default;
    virtual std::string_view class_name() const override { return "VariableDeclaration"; }
    virtual void dump(FILE* = stdout, size_t indent = 0) const override;

    VariableDeclaration(ASTNode const* parent, std::optional<Position> start, std::optional<Position> end, std::string const& filename)
            : VariableOrParameterDeclaration(parent, start, end, filename)
    {
    }

    virtual bool is_variable_declaration() const override { return true; }

    Expression const* initial_value() const { return m_initial_value.get(); }
    void set_initial_value(intrusive_ptr<Expression const>&& initial_value) { m_initial_value =std::move(initial_value); }

private:
    intrusive_ptr<Expression const> m_initial_value;
};

class Identifier : public Expression {
public:
    virtual ~Identifier() override = default;
    virtual std::string_view class_name() const override { return "Identifier"; }
    virtual void dump(FILE* = stdout, size_t indent = 0) const override;

    Identifier(ASTNode const* parent, std::optional<Position> start, std::optional<Position> end, std::string const& filename, std::string_view name)
            : Expression(parent, start, end, filename)
            , m_name(name)
    {
    }
    Identifier(ASTNode const* parent, std::optional<Position> start, std::optional<Position> end, std::string const& filename)
            : Identifier(parent, start, end, filename, {})
    {
    }

    virtual bool is_identifier() const override { return true; }

    std::string_view name() const { return m_name; }
    void set_name(std::string_view&& name) { m_name =std::move(name); }

private:
    std::string_view m_name;
};

class Name : public Expression {
public:
    virtual ~Name() override = default;
    virtual std::string_view class_name() const override { return "Name"; }
    virtual void dump(FILE* = stdout, size_t indent = 0) const override;
    virtual bool is_name() const override { return true; }
    virtual bool is_templatized() const { return false; }
    virtual bool is_sized() const { return false; }

    Name(ASTNode const* parent, std::optional<Position> start, std::optional<Position> end, std::string const& filename)
            : Expression(parent, start, end, filename)
    {
    }
    virtual std::string_view full_name() const;

    Identifier const* name() const { return m_name.get(); }
    void set_name(intrusive_ptr<Identifier const>&& name) { m_name =std::move(name); }
    std::vector<intrusive_ptr<Identifier const>> const& scope() const { return m_scope; }
    void set_scope(std::vector<intrusive_ptr<Identifier const>> scope) { m_scope =std::move(scope); }
    void add_to_scope(intrusive_ptr<Identifier const>&& part) { m_scope.emplace_back(std::move(part)); }

private:
    intrusive_ptr<Identifier const> m_name;
    std::vector<intrusive_ptr<Identifier const>> m_scope;
    mutable std::optional<std::string> m_full_name;
};

class SizedName : public Name {
public:
    virtual ~SizedName() override = default;
    virtual std::string_view class_name() const override { return "SizedName"; }
    virtual bool is_sized() const override { return true; }
    void dump(FILE* output, size_t indent) const override;

    SizedName(ASTNode const* parent, std::optional<Position> start, std::optional<Position> end, std::string const& filename)
            : Name(parent, start, end, filename)
    {
    }

    void append_dimension(std::string_view dim) { m_dimensions.emplace_back(dim); }

private:
    std::vector<std::string_view> m_dimensions;
    mutable std::optional<std::string> m_full_name;
};

class TemplatizedName : public Name {
public:
    virtual ~TemplatizedName() override = default;
    virtual std::string_view class_name() const override { return "TemplatizedName"; }
    virtual bool is_templatized() const override { return true; }
    virtual std::string_view full_name() const override;

    TemplatizedName(ASTNode const* parent, std::optional<Position> start, std::optional<Position> end, std::string const& filename)
            : Name(parent, start, end, filename)
    {
    }

    void add_template_argument(intrusive_ptr<Type const>&& type) { m_template_arguments.emplace_back(std::move(type)); }

private:
    std::vector<intrusive_ptr<Type const>> m_template_arguments;
    mutable std::optional<std::string> m_full_name;
};

class NumericLiteral : public Expression {
public:
    virtual ~NumericLiteral() override = default;
    virtual std::string_view class_name() const override { return "NumericLiteral"; }
    virtual void dump(FILE* = stdout, size_t indent = 0) const override;

    NumericLiteral(ASTNode const* parent, std::optional<Position> start, std::optional<Position> end, std::string const& filename, std::string_view value)
            : Expression(parent, start, end, filename)
            , m_value(value)
    {
    }

private:
    std::string_view m_value;
};

class NullPointerLiteral : public Expression {
public:
    virtual ~NullPointerLiteral() override = default;
    virtual std::string_view class_name() const override { return "NullPointerLiteral"; }
    virtual void dump(FILE* = stdout, size_t indent = 0) const override;

    NullPointerLiteral(ASTNode const* parent, std::optional<Position> start, std::optional<Position> end, std::string const& filename)
            : Expression(parent, start, end, filename)
    {
    }
};

class BooleanLiteral : public Expression {
public:
    virtual ~BooleanLiteral() override = default;
    virtual std::string_view class_name() const override { return "BooleanLiteral"; }
    virtual void dump(FILE* = stdout, size_t indent = 0) const override;

    BooleanLiteral(ASTNode const* parent, std::optional<Position> start, std::optional<Position> end, std::string const& filename, bool value)
            : Expression(parent, start, end, filename)
            , m_value(value)
    {
    }

private:
    bool m_value;
};

enum class BinaryOp {
    Addition,
    Subtraction,
    Multiplication,
    Division,
    Modulo,
    GreaterThan,
    GreaterThanEquals,
    LessThan,
    LessThanEquals,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    LeftShift,
    RightShift,
    EqualsEquals,
    NotEqual,
    LogicalOr,
    LogicalAnd,
    Arrow,
};

class BinaryExpression : public Expression {
public:
    BinaryExpression(ASTNode const* parent, std::optional<Position> start, std::optional<Position> end, std::string const& filename)
            : Expression(parent, start, end, filename)
    {
    }

    virtual ~BinaryExpression() override = default;
    virtual std::string_view class_name() const override { return "BinaryExpression"; }
    virtual void dump(FILE* = stdout, size_t indent = 0) const override;

    BinaryOp op() const { return m_op; }
    void set_op(BinaryOp op) { m_op = op; }
    Expression const* lhs() const { return m_lhs.get(); }
    void set_lhs(intrusive_ptr<Expression const>&& e) { m_lhs = std::move(e); }
    Expression const* rhs() const { return m_rhs.get(); }
    void set_rhs(intrusive_ptr<Expression const>&& e) { m_rhs = std::move(e); }

private:
    BinaryOp m_op;
    intrusive_ptr<Expression const> m_lhs;
    intrusive_ptr<Expression const> m_rhs;
};

enum class AssignmentOp {
    Assignment,
    AdditionAssignment,
    SubtractionAssignment,
};

class AssignmentExpression : public Expression {
public:
    AssignmentExpression(ASTNode const* parent, std::optional<Position> start, std::optional<Position> end, std::string const& filename)
            : Expression(parent, start, end, filename)
    {
    }

    virtual ~AssignmentExpression() override = default;
    virtual std::string_view class_name() const override { return "AssignmentExpression"; }
    virtual void dump(FILE* = stdout, size_t indent = 0) const override;

    AssignmentOp op() const { return m_op; }
    void set_op(AssignmentOp op) { m_op = op; }
    Expression const* lhs() const { return m_lhs.get(); }
    void set_lhs(intrusive_ptr<Expression const>&& e) { m_lhs = std::move(e); }
    Expression const* rhs() const { return m_rhs.get(); }
    void set_rhs(intrusive_ptr<Expression const>&& e) { m_rhs = std::move(e); }

private:
    AssignmentOp m_op {};
    intrusive_ptr<Expression const> m_lhs;
    intrusive_ptr<Expression const> m_rhs;
};

class FunctionCall : public Expression {
public:
    FunctionCall(ASTNode const* parent, std::optional<Position> start, std::optional<Position> end, std::string const& filename)
            : Expression(parent, start, end, filename)
    {
    }

    virtual ~FunctionCall() override = default;
    virtual std::string_view class_name() const override { return "FunctionCall"; }
    virtual void dump(FILE* = stdout, size_t indent = 0) const override;
    virtual bool is_function_call() const override { return true; }

    Expression const* callee() const { return m_callee.get(); }
    void set_callee(intrusive_ptr<Expression const>&& callee) { m_callee = std::move(callee); }

    void add_argument(intrusive_ptr<Expression const>&& arg) { m_arguments.emplace_back(std::move(arg)); }
    std::vector<intrusive_ptr<Expression const>> const& arguments() const { return m_arguments; }

private:
    intrusive_ptr<Expression const> m_callee;
    std::vector<intrusive_ptr<Expression const>> m_arguments;
};

class StringLiteral final : public Expression {
public:
    StringLiteral(ASTNode const* parent, std::optional<Position> start, std::optional<Position> end, std::string const& filename)
            : Expression(parent, start, end, filename)
    {
    }

    ~StringLiteral() override = default;
    virtual std::string_view class_name() const override { return "StringLiteral"; }
    virtual void dump(FILE* = stdout, size_t indent = 0) const override;

    std::string const& value() const { return m_value; }
    void set_value(std::string value) { m_value = std::move(value); }

private:
    std::string m_value;
};

class ReturnStatement : public Statement {
public:
    virtual ~ReturnStatement() override = default;
    virtual std::string_view class_name() const override { return "ReturnStatement"; }

    ReturnStatement(ASTNode const* parent, std::optional<Position> start, std::optional<Position> end, std::string const& filename)
            : Statement(parent, start, end, filename)
    {
    }
    virtual void dump(FILE* = stdout, size_t indent = 0) const override;

    Expression const* value() const { return m_value.get(); }
    void set_value(intrusive_ptr<Expression const>&& value) { m_value = std::move(value); }

private:
    intrusive_ptr<Expression const> m_value;
};

class EnumDeclaration : public Declaration {
public:
    virtual ~EnumDeclaration() override = default;
    virtual std::string_view class_name() const override { return "EnumDeclaration"; }
    virtual void dump(FILE* = stdout, size_t indent = 0) const override;
    virtual bool is_enum() const override { return true; }

    EnumDeclaration(ASTNode const* parent, std::optional<Position> start, std::optional<Position> end, std::string const& filename)
            : Declaration(parent, start, end, filename)
    {
    }

    enum class Type {
        RegularEnum,
        EnumClass
    };

    void set_type(Type type) { m_type = type; }
    void add_entry(std::string_view entry, intrusive_ptr<Expression const> value = nullptr) { m_entries.emplace_back(EnumerationEntry{ entry, std::move(value) }); }

private:
    Type m_type { Type::RegularEnum };
    struct EnumerationEntry {
        std::string_view name;
        intrusive_ptr<Expression const> value;
    };
    std::vector<EnumerationEntry> m_entries;
};

class StructOrClassDeclaration : public Declaration {
public:
    virtual ~StructOrClassDeclaration() override = default;
    virtual std::string_view class_name() const override { return "StructOrClassDeclaration"; }
    virtual void dump(FILE* = stdout, size_t indent = 0) const override;
    virtual bool is_struct_or_class() const override { return true; }
    virtual bool is_struct() const override { return m_type == Type::Struct; }
    virtual bool is_class() const override { return m_type == Type::Class; }
    virtual std::vector<intrusive_ptr<Declaration const>> declarations() const override;

    enum class Type {
        Struct,
        Class
    };

    StructOrClassDeclaration(ASTNode const* parent, std::optional<Position> start, std::optional<Position> end, std::string const& filename, StructOrClassDeclaration::Type type)
            : Declaration(parent, start, end, filename)
            , m_type(type)
    {
    }

    std::vector<intrusive_ptr<Declaration const>> const& members() const { return m_members; }
    void set_members(std::vector<intrusive_ptr<Declaration const>>&& members) { m_members = std::move(members); }

    std::vector<intrusive_ptr<Name const>> const& baseclasses() const { return m_baseclasses; }
    void set_baseclasses(std::vector<intrusive_ptr<Name const>>&& baseclasses) { m_baseclasses = std::move(baseclasses); }

private:
    StructOrClassDeclaration::Type m_type;
    std::vector<intrusive_ptr<Declaration const>> m_members;
    std::vector<intrusive_ptr<Name const>> m_baseclasses;
};

enum class UnaryOp {
    Invalid,
    BitwiseNot,
    Not,
    Plus,
    Minus,
    PlusPlus,
    Address,
};

class UnaryExpression : public Expression {
public:
    UnaryExpression(ASTNode const* parent, std::optional<Position> start, std::optional<Position> end, std::string const& filename)
            : Expression(parent, start, end, filename)
    {
    }

    virtual ~UnaryExpression() override = default;
    virtual std::string_view class_name() const override { return "UnaryExpression"; }
    virtual void dump(FILE* = stdout, size_t indent = 0) const override;

    void set_op(UnaryOp op) { m_op = op; }
    void set_lhs(intrusive_ptr<Expression const>&& e) { m_lhs = std::move(e); }

private:
    UnaryOp m_op;
    intrusive_ptr<Expression const> m_lhs;
};

class MemberExpression : public Expression {
public:
    MemberExpression(ASTNode const* parent, std::optional<Position> start, std::optional<Position> end, std::string const& filename)
            : Expression(parent, start, end, filename)
    {
    }

    virtual ~MemberExpression() override = default;
    virtual std::string_view class_name() const override { return "MemberExpression"; }
    virtual void dump(FILE* = stdout, size_t indent = 0) const override;
    virtual bool is_member_expression() const override { return true; }

    Expression const* object() const { return m_object.get(); }
    void set_object(intrusive_ptr<Expression const>&& object) { m_object = std::move(object); }
    Expression const* property() const { return m_property.get(); }
    void set_property(intrusive_ptr<Expression const>&& property) { m_property = std::move(property); }

private:
    intrusive_ptr<Expression const> m_object;
    intrusive_ptr<Expression const> m_property;
};

class ForStatement : public Statement {
public:
    ForStatement(ASTNode const* parent, std::optional<Position> start, std::optional<Position> end, std::string const& filename)
            : Statement(parent, start, end, filename)
    {
    }

    virtual ~ForStatement() override = default;
    virtual std::string_view class_name() const override { return "ForStatement"; }
    virtual void dump(FILE* = stdout, size_t indent = 0) const override;

    virtual std::vector<intrusive_ptr<Declaration const>> declarations() const override;

    void set_init(intrusive_ptr<VariableDeclaration const>&& init) { m_init = std::move(init); }
    void set_test(intrusive_ptr<Expression const>&& test) { m_test = std::move(test); }
    void set_update(intrusive_ptr<Expression const>&& update) { m_update = std::move(update); }
    void set_body(intrusive_ptr<Statement const>&& body) { m_body = std::move(body); }
    Statement const* body() const { return m_body.get(); }

private:
    intrusive_ptr<VariableDeclaration const> m_init;
    intrusive_ptr<Expression const> m_test;
    intrusive_ptr<Expression const> m_update;
    intrusive_ptr<Statement const> m_body;
};

class BlockStatement final : public Statement {
public:
    BlockStatement(ASTNode const* parent, std::optional<Position> start, std::optional<Position> end, std::string const& filename)
            : Statement(parent, start, end, filename)
    {
    }

    virtual ~BlockStatement() override = default;
    virtual std::string_view class_name() const override { return "BlockStatement"; }
    virtual void dump(FILE* = stdout, size_t indent = 0) const override;

    virtual std::vector<intrusive_ptr<Declaration const>> declarations() const override;

    void add_statement(intrusive_ptr<Statement const>&& statement) { m_statements.emplace_back(std::move(statement)); }

private:
    std::vector<intrusive_ptr<Statement const>> m_statements;
};

class Comment final : public Statement {
public:
    Comment(ASTNode const* parent, std::optional<Position> start, std::optional<Position> end, std::string const& filename)
            : Statement(parent, start, end, filename)
    {
    }

    virtual ~Comment() override = default;
    virtual std::string_view class_name() const override { return "Comment"; }
};

class IfStatement : public Statement {
public:
    IfStatement(ASTNode const* parent, std::optional<Position> start, std::optional<Position> end, std::string const& filename)
            : Statement(parent, start, end, filename)
    {
    }

    virtual ~IfStatement() override = default;
    virtual std::string_view class_name() const override { return "IfStatement"; }
    virtual void dump(FILE* = stdout, size_t indent = 0) const override;
    virtual std::vector<intrusive_ptr<Declaration const>> declarations() const override;

    void set_predicate(intrusive_ptr<Expression const>&& predicate) { m_predicate = std::move(predicate); }
    void set_then_statement(intrusive_ptr<Statement const>&& then) { m_then = std::move(then); }
    void set_else_statement(intrusive_ptr<Statement const>&& _else) { m_else = std::move(_else); }

    Statement const* then_statement() const { return m_then.get(); }
    Statement const* else_statement() const { return m_else.get(); }

private:
    intrusive_ptr<Expression const> m_predicate;
    intrusive_ptr<Statement const> m_then;
    intrusive_ptr<Statement const> m_else;
};

class NamespaceDeclaration : public Declaration {
public:
    virtual ~NamespaceDeclaration() override = default;
    virtual std::string_view class_name() const override { return "NamespaceDeclaration"; }
    virtual void dump(FILE* = stdout, size_t indent = 0) const override;
    virtual bool is_namespace() const override { return true; }

    NamespaceDeclaration(ASTNode const* parent, std::optional<Position> start, std::optional<Position> end, std::string const& filename)
            : Declaration(parent, start, end, filename)
    {
    }

    virtual std::vector<intrusive_ptr<Declaration const>> declarations() const override { return m_declarations; }
    void add_declaration(intrusive_ptr<Declaration const>&& declaration) { m_declarations.emplace_back(std::move(declaration)); }

private:
    std::vector<intrusive_ptr<Declaration const>> m_declarations;
};

class CppCastExpression : public Expression {
public:
    CppCastExpression(ASTNode const* parent, std::optional<Position> start, std::optional<Position> end, std::string const& filename)
            : Expression(parent, start, end, filename)
    {
    }

    virtual ~CppCastExpression() override = default;
    virtual std::string_view class_name() const override { return "CppCastExpression"; }
    virtual void dump(FILE* = stdout, size_t indent = 0) const override;

    void set_cast_type(std::string_view cast_type) { m_cast_type = std::move(cast_type); }
    void set_type(intrusive_ptr<Type const>&& type) { m_type = std::move(type); }
    void set_expression(intrusive_ptr<Expression const>&& e) { m_expression = std::move(e); }

private:
    std::string_view m_cast_type;
    intrusive_ptr<Type const> m_type;
    intrusive_ptr<Expression const> m_expression;
};

class CStyleCastExpression : public Expression {
public:
    CStyleCastExpression(ASTNode const* parent, std::optional<Position> start, std::optional<Position> end, std::string const& filename)
            : Expression(parent, start, end, filename)
    {
    }

    virtual ~CStyleCastExpression() override = default;
    virtual std::string_view class_name() const override { return "CStyleCastExpression"; }
    virtual void dump(FILE* = stdout, size_t indent = 0) const override;

    void set_type(intrusive_ptr<Type const>&& type) { m_type = std::move(type); }
    void set_expression(intrusive_ptr<Expression const>&& e) { m_expression = std::move(e); }

private:
    intrusive_ptr<Type const> m_type;
    intrusive_ptr<Expression const> m_expression;
};

class SizeofExpression : public Expression {
public:
    SizeofExpression(ASTNode const* parent, std::optional<Position> start, std::optional<Position> end, std::string const& filename)
            : Expression(parent, start, end, filename)
    {
    }

    virtual ~SizeofExpression() override = default;
    virtual std::string_view class_name() const override { return "SizeofExpression"; }
    virtual void dump(FILE* = stdout, size_t indent = 0) const override;

    void set_type(intrusive_ptr<Type const>&& type) { m_type = std::move(type); }

private:
    intrusive_ptr<Type const> m_type;
};

class BracedInitList : public Expression {
public:
    BracedInitList(ASTNode const* parent, std::optional<Position> start, std::optional<Position> end, std::string const& filename)
            : Expression(parent, start, end, filename)
    {
    }

    virtual ~BracedInitList() override = default;
    virtual std::string_view class_name() const override { return "BracedInitList"; }
    virtual void dump(FILE* = stdout, size_t indent = 0) const override;

    void add_expression(intrusive_ptr<Expression const>&& exp) { m_expressions.emplace_back(std::move(exp)); }

private:
    std::vector<intrusive_ptr<Expression const>> m_expressions;
};

class DummyAstNode : public ASTNode {
public:
    DummyAstNode(ASTNode const* parent, std::optional<Position> start, std::optional<Position> end, std::string const& filename)
            : ASTNode(parent, start, end, filename)
    {
    }
    virtual bool is_dummy_node() const override { return true; }
    virtual std::string_view class_name() const override { return "DummyAstNode"; }
    virtual void dump(FILE* = stdout, size_t = 0) const override { }
};

class Constructor : public FunctionDeclaration {
public:
    virtual ~Constructor() override = default;
    virtual std::string_view class_name() const override { return "Constructor"; }
    virtual void dump(FILE* = stdout, size_t indent = 0) const override;
    virtual bool is_constructor() const override { return true; }

    Constructor(ASTNode const* parent, std::optional<Position> start, std::optional<Position> end, std::string const& filename)
            : FunctionDeclaration(parent, start, end, filename)
    {
    }
};

class Destructor : public FunctionDeclaration {
public:
    virtual ~Destructor() override = default;
    virtual std::string_view class_name() const override { return "Destructor"; }
    virtual void dump(FILE* = stdout, size_t indent = 0) const override;
    virtual bool is_destructor() const override { return true; }

    Destructor(ASTNode const* parent, std::optional<Position> start, std::optional<Position> end, std::string const& filename)
            : FunctionDeclaration(parent, start, end, filename)
    {
    }
};

class UsingNamespaceDeclaration : public Declaration {
public:
    virtual ~UsingNamespaceDeclaration() override = default;
    virtual std::string_view class_name() const override { return "UsingNamespaceDeclaration"; }
    virtual void dump(FILE* = stdout, size_t indent = 0) const override;

    UsingNamespaceDeclaration(ASTNode const* parent, std::optional<Position> start, std::optional<Position> end, std::string const& filename)
            : Declaration(parent, start, end, filename)
    {
    }
};

class TypedefDeclaration : public Declaration {
public:
    virtual ~TypedefDeclaration() override = default;
    virtual std::string_view class_name() const override { return "TypedefDeclaration"; }
    virtual void dump(FILE* = stdout, size_t indent = 0) const override;

    TypedefDeclaration(ASTNode const* parent, std::optional<Position> start, std::optional<Position> end, std::string const& filename)
            : Declaration(parent, start, end, filename)
    {
    }

    void set_alias(Type const& alias) { m_alias.reset(&alias, true); }
    Type const* alias() const { return m_alias.get(); }

private:
    intrusive_ptr<Type const> m_alias;
};
template<>
inline bool ASTNode::fast_is<Identifier>() const { return is_identifier(); }
template<>
inline bool ASTNode::fast_is<MemberExpression>() const { return is_member_expression(); }
template<>
inline bool ASTNode::fast_is<VariableOrParameterDeclaration>() const { return is_variable_or_parameter_declaration(); }
template<>
inline bool ASTNode::fast_is<FunctionCall>() const { return is_function_call(); }
template<>
inline bool ASTNode::fast_is<Type>() const { return is_type(); }
template<>
inline bool ASTNode::fast_is<Declaration>() const { return is_declaration(); }
template<>
inline bool ASTNode::fast_is<Name>() const { return is_name(); }
template<>
inline bool ASTNode::fast_is<DummyAstNode>() const { return is_dummy_node(); }

template<>
inline bool ASTNode::fast_is<VariableDeclaration>() const { return is_declaration() && assert_cast<Declaration>(*this).is_variable_declaration(); }
template<>
inline bool ASTNode::fast_is<StructOrClassDeclaration>() const { return is_declaration() && assert_cast<Declaration>(*this).is_struct_or_class(); }
template<>
inline bool ASTNode::fast_is<FunctionDeclaration>() const { return is_declaration() && assert_cast<Declaration>(*this).is_function(); }
template<>
inline bool ASTNode::fast_is<NamespaceDeclaration>() const { return is_declaration() && assert_cast<Declaration>(*this).is_namespace(); }
template<>
inline bool ASTNode::fast_is<Constructor>() const { return is_declaration() && assert_cast<Declaration>(*this).is_function() && assert_cast<FunctionDeclaration>(*this).is_constructor(); }
template<>
inline bool ASTNode::fast_is<Destructor>() const { return is_declaration() && assert_cast<Declaration>(*this).is_function() && assert_cast<FunctionDeclaration>(*this).is_destructor(); }
template<>
inline bool ASTNode::fast_is<NamedType>() const { return is_type() && assert_cast<Type>(*this).is_named_type(); }
template<>
inline bool ASTNode::fast_is<TemplatizedName>() const { return is_name() && assert_cast<Name>(*this).is_templatized(); }
template<>
inline bool ASTNode::fast_is<SizedName>() const { return is_name() && assert_cast<Name>(*this).is_sized(); }
}
