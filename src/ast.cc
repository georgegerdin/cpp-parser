/*
 * Copyright (c) 2021, Itamar S. <itamar8910@gmail.com>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#include "cpp_parser/ast.hh"

namespace Cpp {

    static void print_indent(FILE* output, int indent)
    {
        for (int i = 0; i < indent * 2; ++i)
            out(output, " ");
    }

    void ASTNode::dump(FILE* output, size_t indent) const
    {
        print_indent(output, indent);
        outln(output, "{}[{}:{}->{}:{}]", class_name(), start().line, start().column, end().line, end().column);
    }

    void TranslationUnit::dump(FILE* output, size_t indent) const
    {
        ASTNode::dump(output, indent);
        for (auto const& child : m_declarations) {
            child->dump(output, indent + 1);
        }
    }

    void FunctionDeclaration::dump(FILE* output, size_t indent) const
    {
        ASTNode::dump(output, indent);

        std::string qualifiers_string;
        if (!m_qualifiers.empty()) {
            print_indent(output, indent + 1);
            outln(output, "[{}]", join_strings(' ', m_qualifiers));
        }

        m_return_type->dump(output, indent + 1);
        if (m_name) {
            print_indent(output, indent + 1);
            outln(output, "{}", m_name->full_name());
        }
        print_indent(output, indent + 1);
        outln(output, "(");
        for (auto const& arg : m_parameters) {
            arg->dump(output, indent + 1);
        }
        print_indent(output, indent + 1);
        outln(output, ")");
        if (!m_definition.is_null()) {
            m_definition->dump(output, indent + 1);
        }
    }

    std::vector<intrusive_ptr<Declaration const>> FunctionDeclaration::declarations() const
    {
        std::vector<intrusive_ptr<Declaration const>> declarations;
        for (auto& arg : m_parameters) {
            declarations.push_back(arg);
        }

        if (m_definition) {
            auto definition_declarations = m_definition->declarations();
            declarations.insert(declarations.end(), definition_declarations.begin(), definition_declarations.end());
        }
        return declarations;
    }

    void Type::dump(FILE* output, size_t indent) const
    {
        ASTNode::dump(output, indent);
        print_indent(output, indent + 1);
        outln(output, "{}", to_string());
    }

    std::string NamedType::to_string() const
    {
        std::string qualifiers_string;
        if (!qualifiers().empty())
            qualifiers_string = fmt::format("[{}] ", join_strings(' ', qualifiers()));

        std::string name;
        if (is_auto())
            name = "auto";
        else
            name = !m_name ? "" : m_name->full_name();

        return fmt::format("{}{}", qualifiers_string, name);
    }

    std::string Pointer::to_string() const
    {
        if (!m_pointee)
            return {};
        std::string builder;
        builder.append(m_pointee->to_string());
        builder+= '*';
        return builder;
    }

    std::string Reference::to_string() const
    {
        if (!m_referenced_type)
            return {};
        std::string builder;
        builder.append(m_referenced_type->to_string());
        if (m_kind == Kind::Lvalue)
            builder+= '&';
        else
            builder.append("&&");
        return builder;
    }

    std::string FunctionType::to_string() const
    {
        std::string builder;
        builder.append(m_return_type->to_string());
        builder+='(';
        bool first = true;
        for (auto& parameter : m_parameters) {
            if (first)
                first = false;
            else
                builder.append(", ");
            if (parameter->type())
                builder.append(parameter->type()->to_string());
            if (parameter->name() && !parameter->full_name().empty()) {
                builder+=' ';
                builder.append(parameter->full_name());
            }
        }
        builder+=(')');
        return builder;
    }

    void Parameter::dump(FILE* output, size_t indent) const
    {
        ASTNode::dump(output, indent);
        if (m_is_ellipsis) {
            print_indent(output, indent + 1);
            outln(output, "...");
        }
        if (m_name) {
            print_indent(output, indent);
            outln(output, "{}", m_name->full_name());
        }
        if (m_type)
            m_type->dump(output, indent + 1);
    }

    void FunctionDefinition::dump(FILE* output, size_t indent) const
    {
        ASTNode::dump(output, indent);
        print_indent(output, indent);
        outln(output, "{{");
        for (auto const& statement : m_statements) {
            statement->dump(output, indent + 1);
        }
        print_indent(output, indent);
        outln(output, "}}");
    }

    std::vector<intrusive_ptr<Declaration const>> FunctionDefinition::declarations() const
    {
        std::vector<intrusive_ptr<Declaration const>> declarations;
        for (auto& statement : m_statements) {
            auto statement_declarations = statement->declarations();
            declarations.insert(declarations.end(), statement_declarations.begin(), statement_declarations.end());
        }
        return declarations;
    }

    void VariableDeclaration::dump(FILE* output, size_t indent) const
    {
        ASTNode::dump(output, indent);
        if (m_type)
            m_type->dump(output, indent + 1);
        print_indent(output, indent + 1);
        outln(output, "{}", full_name());
        if (m_initial_value)
            m_initial_value->dump(output, indent + 1);
    }

    void Identifier::dump(FILE* output, size_t indent) const
    {
        ASTNode::dump(output, indent);
        print_indent(output, indent);
        outln(output, "{}", m_name);
    }

    void NumericLiteral::dump(FILE* output, size_t indent) const
    {
        ASTNode::dump(output, indent);
        print_indent(output, indent);
        outln(output, "{}", m_value);
    }

    void BinaryExpression::dump(FILE* output, size_t indent) const
    {
        ASTNode::dump(output, indent);

        char const* op_string = nullptr;
        switch (m_op) {
            case BinaryOp::Addition:
                op_string = "+";
                break;
            case BinaryOp::Subtraction:
                op_string = "-";
                break;
            case BinaryOp::Multiplication:
                op_string = "*";
                break;
            case BinaryOp::Division:
                op_string = "/";
                break;
            case BinaryOp::Modulo:
                op_string = "%";
                break;
            case BinaryOp::GreaterThan:
                op_string = ">";
                break;
            case BinaryOp::GreaterThanEquals:
                op_string = ">=";
                break;
            case BinaryOp::LessThan:
                op_string = "<";
                break;
            case BinaryOp::LessThanEquals:
                op_string = "<=";
                break;
            case BinaryOp::BitwiseAnd:
                op_string = "&";
                break;
            case BinaryOp::BitwiseOr:
                op_string = "|";
                break;
            case BinaryOp::BitwiseXor:
                op_string = "^";
                break;
            case BinaryOp::LeftShift:
                op_string = "<<";
                break;
            case BinaryOp::RightShift:
                op_string = ">>";
                break;
            case BinaryOp::EqualsEquals:
                op_string = "==";
                break;
            case BinaryOp::NotEqual:
                op_string = "!=";
                break;
            case BinaryOp::LogicalOr:
                op_string = "||";
                break;
            case BinaryOp::LogicalAnd:
                op_string = "&&";
                break;
            case BinaryOp::Arrow:
                op_string = "->";
                break;
        }

        m_lhs->dump(output, indent + 1);
        print_indent(output, indent + 1);
        assert(op_string);
        outln(output, "{}", op_string);
        m_rhs->dump(output, indent + 1);
    }

    void AssignmentExpression::dump(FILE* output, size_t indent) const
    {
        ASTNode::dump(output, indent);

        char const* op_string = nullptr;
        switch (m_op) {
            case AssignmentOp::Assignment:
                op_string = "=";
                break;
            case AssignmentOp::AdditionAssignment:
                op_string = "+=";
                break;
            case AssignmentOp::SubtractionAssignment:
                op_string = "-=";
                break;
        }

        m_lhs->dump(output, indent + 1);
        print_indent(output, indent + 1);
        assert(op_string);
        outln(output, "{}", op_string);
        m_rhs->dump(output, indent + 1);
    }

    void FunctionCall::dump(FILE* output, size_t indent) const
    {
        ASTNode::dump(output, indent);
        m_callee->dump(output, indent + 1);
        for (auto const& arg : m_arguments) {
            arg->dump(output, indent + 1);
        }
    }

    void StringLiteral::dump(FILE* output, size_t indent) const
    {
        ASTNode::dump(output, indent);
        print_indent(output, indent + 1);
        outln(output, "{}", m_value);
    }

    void ReturnStatement::dump(FILE* output, size_t indent) const
    {
        ASTNode::dump(output, indent);
        if (m_value)
            m_value->dump(output, indent + 1);
    }

    void EnumDeclaration::dump(FILE* output, size_t indent) const
    {
        ASTNode::dump(output, indent);
        print_indent(output, indent);
        outln(output, "{}", full_name());
        for (auto& entry : m_entries) {
            print_indent(output, indent + 1);
            outln(output, "{}", entry.name);
            if (entry.value)
                entry.value->dump(output, indent + 2);
        }
    }

    void StructOrClassDeclaration::dump(FILE* output, size_t indent) const
    {
        ASTNode::dump(output, indent);
        print_indent(output, indent);
        outln(output, "{}", full_name());
        if (!m_baseclasses.empty()) {
            print_indent(output, indent + 1);
            outln(output, ":");
            for (size_t i = 0; i < m_baseclasses.size(); ++i) {
                auto& baseclass = m_baseclasses[i];
                baseclass->dump(output, indent + 1);
                if (i < m_baseclasses.size() - 1) {
                    print_indent(output, indent + 1);
                    outln(output, ",");
                }
            }
        }
        outln(output, "");
        for (auto& member : m_members) {
            member->dump(output, indent + 1);
        }
    }
    std::vector<intrusive_ptr<Declaration const>> StructOrClassDeclaration::declarations() const
    {
        std::vector<intrusive_ptr<Declaration const>> declarations;
        for (auto& member : m_members)
            declarations.push_back(member);
        return declarations;
    }

    void UnaryExpression::dump(FILE* output, size_t indent) const
    {
        ASTNode::dump(output, indent);

        char const* op_string = nullptr;
        switch (m_op) {
            case UnaryOp::BitwiseNot:
                op_string = "~";
                break;
            case UnaryOp::Not:
                op_string = "!";
                break;
            case UnaryOp::Plus:
                op_string = "+";
                break;
            case UnaryOp::Minus:
                op_string = "-";
                break;
            case UnaryOp::PlusPlus:
                op_string = "++";
                break;
            case UnaryOp::Address:
                op_string = "&";
                break;
            case UnaryOp::Dereference:
                op_string = "*";
                break;
            default:
                op_string = "<invalid>";
        }

        assert(op_string);
        print_indent(output, indent + 1);
        outln(output, "{}", op_string);
        m_lhs->dump(output, indent + 1);
    }

    void BooleanLiteral::dump(FILE* output, size_t indent) const
    {
        ASTNode::dump(output, indent);
        print_indent(output, indent + 1);
        outln(output, "{}", m_value ? "true" : "false");
    }

    void Pointer::dump(FILE* output, size_t indent) const
    {
        ASTNode::dump(output, indent);
        if (!m_pointee) {
            m_pointee->dump(output, indent + 1);
        }
    }

    void Reference::dump(FILE* output, size_t indent) const
    {
        ASTNode::dump(output, indent);
        print_indent(output, indent + 1);
        outln(output, "{}", m_kind == Kind::Lvalue ? "&" : "&&");
        if (!m_referenced_type) {
            m_referenced_type->dump(output, indent + 1);
        }
    }

    void FunctionType::dump(FILE* output, size_t indent) const
    {
        ASTNode::dump(output, indent);
        if (m_return_type)
            m_return_type->dump(output, indent + 1);
        print_indent(output, indent + 1);
        outln("(");
        for (auto& parameter : m_parameters)
            parameter->dump(output, indent + 2);
        print_indent(output, indent + 1);
        outln(")");
    }

    void MemberExpression::dump(FILE* output, size_t indent) const
    {
        ASTNode::dump(output, indent);
        m_object->dump(output, indent + 1);
        m_property->dump(output, indent + 1);
    }

    void BlockStatement::dump(FILE* output, size_t indent) const
    {
        ASTNode::dump(output, indent);
        for (auto& statement : m_statements) {
            statement->dump(output, indent + 1);
        }
    }

    void ForStatement::dump(FILE* output, size_t indent) const
    {
        ASTNode::dump(output, indent);
        if (m_init)
            m_init->dump(output, indent + 1);
        if (m_test)
            m_test->dump(output, indent + 1);
        if (m_update)
            m_update->dump(output, indent + 1);
        if (m_body)
            m_body->dump(output, indent + 1);
    }

    std::vector<intrusive_ptr<Declaration const>> Statement::declarations() const
    {
        if (is_declaration()) {
            std::vector<intrusive_ptr<Declaration const>> vec;
            auto const& decl = static_cast<Declaration const&>(*this);
            vec.emplace_back(intrusive_ptr<Declaration const>(&const_cast<Declaration&>(decl)));
            return vec;
        }
        return {};
    }

    std::vector<intrusive_ptr<Declaration const>> ForStatement::declarations() const
    {
        std::vector<intrusive_ptr<Declaration const>> declarations;
        if (m_init) {
            auto init_declarations = m_init->declarations();
            declarations.insert(declarations.end(), init_declarations.begin(), init_declarations.end());
        }
        if (m_body) {
            auto body_declarations = m_body->declarations();
            declarations.insert(declarations.end(), body_declarations.begin(), body_declarations.end());
        }
        return declarations;
    }

    std::vector<intrusive_ptr<Declaration const>> BlockStatement::declarations() const
    {
        std::vector<intrusive_ptr<Declaration const>> declarations;
        for (auto& statement : m_statements) {
            auto statement_declarations = statement->declarations();
            declarations.insert(declarations.end(), statement_declarations.begin(), statement_declarations.end());
        }
        return declarations;
    }

    void IfStatement::dump(FILE* output, size_t indent) const
    {
        ASTNode::dump(output, indent);
        if (m_predicate) {
            print_indent(output, indent + 1);
            outln(output, "Predicate:");
            m_predicate->dump(output, indent + 1);
        }
        if (m_then) {
            print_indent(output, indent + 1);
            outln(output, "Then:");
            m_then->dump(output, indent + 1);
        }
        if (m_else) {
            print_indent(output, indent + 1);
            outln(output, "Else:");
            m_else->dump(output, indent + 1);
        }
    }

    std::vector<intrusive_ptr<Declaration const>> IfStatement::declarations() const
    {
        std::vector<intrusive_ptr<Declaration const>> declarations;
        if (m_predicate) {
            auto predicate_declarations = m_predicate->declarations();
            declarations.insert(declarations.end(), predicate_declarations.begin(), predicate_declarations.end());
        }
        if (m_then) {
            auto then_declarations = m_then->declarations();
            declarations.insert(declarations.end(), then_declarations.begin(), then_declarations.end());
        }
        if (m_else) {
            auto else_declarations = m_else->declarations();
            declarations.insert(declarations.end(), else_declarations.begin(), else_declarations.end());
        }

        return declarations;
    }

    void NamespaceDeclaration::dump(FILE* output, size_t indent) const
    {
        ASTNode::dump(output, indent);
        print_indent(output, indent + 1);
        outln(output, "{}", full_name());
        for (auto& decl : m_declarations)
            decl->dump(output, indent + 1);
    }

    void NullPointerLiteral::dump(FILE* output, size_t indent) const
    {
        ASTNode::dump(output, indent);
    }

    void Name::dump(FILE* output, size_t indent) const
    {
        ASTNode::dump(output, indent);
        print_indent(output, indent);
        outln(output, "{}", full_name());
    }

    std::string_view Name::full_name() const
    {
        if (m_full_name.has_value())
            return *m_full_name;

        std::string builder;
        if (!m_scope.empty()) {
            for (auto& scope : m_scope) {
                builder.append(fmt::format("{}::", scope->name()));
            }
        }
        m_full_name = fmt::format("{}{}", builder, !m_name ? "" : m_name->name());
        return *m_full_name;
    }

    std::string_view TemplatizedName::full_name() const
    {
        if (m_full_name.has_value())
            return *m_full_name;

        std::string name;
        name.append(Name::full_name());
        name+=('<');
        for (auto& type : m_template_arguments) {
            name.append(type->to_string());
        }
        name+=('>');
        m_full_name = name;
        return *m_full_name;
    }

    void SizedName::dump(FILE* output, size_t indent) const
    {
        Name::dump(output, indent);
        print_indent(output, indent + 1);

        std::string dimension_info;
        for (auto const& dim : m_dimensions) {
            dimension_info+=('[');
            dimension_info.append(dim);
            dimension_info+=(']');
        }

        if (dimension_info.empty()) {
            dimension_info.append("[]");
        }
        outln(output, "{}", dimension_info);
    }

    void CppCastExpression::dump(FILE* output, size_t indent) const
    {
        ASTNode::dump(output, indent);

        print_indent(output, indent);
        outln(output, "{}", m_cast_type);

        print_indent(output, indent + 1);
        outln(output, "<");
        if (m_type)
            m_type->dump(output, indent + 1);
        print_indent(output, indent + 1);
        outln(output, ">");

        if (m_expression)
            m_expression->dump(output, indent + 1);
    }

    void SizeofExpression::dump(FILE* output, size_t indent) const
    {
        ASTNode::dump(output, indent);
        if (m_type)
            m_type->dump(output, indent + 1);
    }

    void BracedInitList::dump(FILE* output, size_t indent) const
    {
        ASTNode::dump(output, indent);
        for (auto& exp : m_expressions) {
            exp->dump(output, indent + 1);
        }
    }

    void CStyleCastExpression::dump(FILE* output, size_t indent) const
    {
        ASTNode::dump(output, indent);
        if (m_type)
            m_type->dump(output, indent + 1);
        if (m_expression)
            m_expression->dump(output, indent + 1);
    }

    void Constructor::dump(FILE* output, size_t indent) const
    {
        print_indent(output, indent);
        outln(output, "C'tor");
        print_indent(output, indent + 1);
        outln(output, "(");
        for (auto const& arg : parameters()) {
            arg->dump(output, indent + 1);
        }
        print_indent(output, indent + 1);
        outln(output, ")");
        if (definition()) {
            definition()->dump(output, indent + 1);
        }
    }

    void Destructor::dump(FILE* output, size_t indent) const
    {
        print_indent(output, indent);
        outln(output, "D'tor");
        print_indent(output, indent + 1);
        outln(output, "(");
        for (auto const& arg : parameters()) {
            arg->dump(output, indent + 1);
        }
        print_indent(output, indent + 1);
        outln(output, ")");
        if (definition()) {
            definition()->dump(output, indent + 1);
        }
    }

    std::string_view Declaration::full_name() const
    {
        if (!m_full_name.has_value()) {
            if (m_name)
                m_full_name = m_name->full_name();
            else
                m_full_name = "";
        }

        return *m_full_name;
    }

    void UsingNamespaceDeclaration::dump(FILE* output, size_t indent) const
    {
        ASTNode::dump(output, indent);
        print_indent(output, indent + 1);
        outln(output, "{}", full_name());
    }

    void TypedefDeclaration::dump(FILE* output, size_t indent) const
    {
        ASTNode::dump(output, indent);
        print_indent(output, indent + 1);
        outln(output, "{}", full_name());
        if (m_alias)
            m_alias->dump(output, indent + 1);
    }

}
