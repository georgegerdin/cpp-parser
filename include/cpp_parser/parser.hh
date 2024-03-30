/*
 * Copyright (c) 2021, Itamar S. <itamar8910@gmail.com>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#include <vector>
#include <string>
#include <string_view>
#include "ast.hh"
#include "lexer.hh"
#include "preprocessor.hh"

namespace CodeComprehension {
    struct TodoEntry {
        std::string content;
        std::string filename;
        size_t line { 0 };
        size_t column { 0 };
    };
}

namespace Cpp {

    class Parser final {
        Parser(const Parser&) = delete; 
        Parser& operator=(const Parser&) = delete;
        
    public:
        explicit Parser(std::vector<Token> tokens, std::string const& filename);
        ~Parser() = default;

        intrusive_ptr<TranslationUnit> parse();
        bool eof() const;

        intrusive_ptr<ASTNode const> node_at(Position) const;
        std::optional<size_t> index_of_node_at(Position) const;
        std::optional<Token> token_at(Position) const;
        std::optional<size_t> index_of_token_at(Position) const;
        intrusive_ptr<TranslationUnit const> root_node() const { return m_root_node; }
        std::string text_of_node(ASTNode const&) const;
        std::string_view text_of_token(Cpp::Token const& token) const;
        void print_tokens() const;
        std::vector<Token> const& tokens() const { return m_tokens; }
        std::vector<std::string> const& errors() const { return m_errors; }

        std::vector<CodeComprehension::TodoEntry> get_todo_entries() const;

        std::vector<Token> tokens_in_range(Position start, Position end) const;

    private:
        enum class DeclarationType {
            Function,
            Variable,
            Enum,
            Class,
            Namespace,
            Constructor,
            Destructor,
            UsingNamespace,
            Typedef,
            UsingType,
        };

        std::optional<DeclarationType> match_declaration_in_translation_unit();
        std::optional<Parser::DeclarationType> match_class_member(std::string_view class_name);

        bool match_function_declaration();
        bool match_comment();
        bool match_preprocessor();
        bool match_whitespace();
        bool match_variable_declaration();
        bool match_expression();
        bool match_secondary_expression();
        bool match_enum_declaration();
        bool match_class_declaration();
        bool match_literal();
        bool match_unary_expression();
        bool match_boolean_literal();
        bool match_keyword(std::string const&);
        bool match_block_statement();
        bool match_namespace_declaration();
        bool match_template_arguments();
        bool match_name();
        bool match_cpp_cast_expression();
        bool match_c_style_cast_expression();
        bool match_sizeof_expression();
        bool match_new_expression();
        bool match_braced_init_list();
        bool match_type();
        bool match_named_type();
        bool match_access_specifier();
        bool match_constructor(std::string_view class_name);
        bool match_destructor(std::string_view class_name);
        bool match_using_namespace_declaration();
        bool match_typedef_declaration();
        bool match_using_type_declaration();

        std::optional<std::vector<intrusive_ptr<Parameter const>>> parse_parameter_list(ASTNode const& parent);
        std::optional<Token> consume_whitespace();
        void consume_preprocessor();

        intrusive_ptr<Declaration const> parse_declaration(ASTNode const& parent, DeclarationType);
        intrusive_ptr<FunctionDeclaration const> parse_function_declaration(ASTNode const& parent);
        intrusive_ptr<FunctionDefinition const> parse_function_definition(ASTNode const& parent);
        intrusive_ptr<Statement const> parse_statement(ASTNode const& parent);
        intrusive_ptr<VariableDeclaration const> parse_variable_declaration(ASTNode const& parent, bool expect_semicolon = true);
        intrusive_ptr<Expression const> parse_expression(ASTNode const& parent);
        intrusive_ptr<Expression const> parse_primary_expression(ASTNode const& parent);
        intrusive_ptr<Expression const> parse_secondary_expression(ASTNode const& parent, intrusive_ptr<Expression const> lhs);
        intrusive_ptr<StringLiteral const> parse_string_literal(ASTNode const& parent);
        intrusive_ptr<ReturnStatement const> parse_return_statement(ASTNode const& parent);
        intrusive_ptr<EnumDeclaration const> parse_enum_declaration(ASTNode const& parent);
        intrusive_ptr<StructOrClassDeclaration const> parse_class_declaration(ASTNode const& parent);
        intrusive_ptr<Expression const> parse_literal(ASTNode const& parent);
        intrusive_ptr<UnaryExpression const> parse_unary_expression(ASTNode const& parent);
        intrusive_ptr<BooleanLiteral const> parse_boolean_literal(ASTNode const& parent);
        intrusive_ptr<Type const> parse_type(ASTNode const& parent);
        intrusive_ptr<BinaryExpression const> parse_binary_expression(ASTNode const& parent, intrusive_ptr<Expression const> lhs, BinaryOp);
        intrusive_ptr<AssignmentExpression const> parse_assignment_expression(ASTNode const& parent, intrusive_ptr<Expression const> lhs, AssignmentOp);
        intrusive_ptr<ForStatement const> parse_for_statement(ASTNode const& parent);
        intrusive_ptr<BlockStatement const> parse_block_statement(ASTNode const& parent);
        intrusive_ptr<Comment const> parse_comment(ASTNode const& parent);
        intrusive_ptr<IfStatement const> parse_if_statement(ASTNode const& parent);
        intrusive_ptr<NamespaceDeclaration const> parse_namespace_declaration(ASTNode const& parent, bool is_nested_namespace = false);
        std::vector<intrusive_ptr<Declaration const>> parse_declarations_in_translation_unit(ASTNode const& parent);
        intrusive_ptr<Declaration const> parse_single_declaration_in_translation_unit(ASTNode const& parent);
        std::vector<intrusive_ptr<Type const>> parse_template_arguments(ASTNode const& parent);
        intrusive_ptr<Name const> parse_name(ASTNode const& parent);
        intrusive_ptr<CppCastExpression const> parse_cpp_cast_expression(ASTNode const& parent);
        intrusive_ptr<SizeofExpression const> parse_sizeof_expression(ASTNode const& parent);
        intrusive_ptr<NewExpression const> parse_new_expression(ASTNode const& parent);
        intrusive_ptr<BracedInitList const> parse_braced_init_list(ASTNode const& parent);
        intrusive_ptr<CStyleCastExpression const> parse_c_style_cast_expression(ASTNode const& parent);
        std::vector<intrusive_ptr<Declaration const>> parse_class_members(StructOrClassDeclaration& parent);
        intrusive_ptr<Constructor const> parse_constructor(ASTNode const& parent);
        intrusive_ptr<Destructor const> parse_destructor(ASTNode const& parent);
        intrusive_ptr<UsingNamespaceDeclaration const> parse_using_namespace_declaration(ASTNode const& parent);
        intrusive_ptr<TypedefDeclaration const> parse_typedef_declaration(ASTNode const& parent);
        intrusive_ptr<TypedefDeclaration const> parse_using_type_declaration(ASTNode const& parent);

        bool match(Token::Type);
        Token consume(Token::Type);
        Token consume();
        Token consume_keyword(std::string const&);
        Token peek(size_t offset = 0) const;
        std::optional<Token> peek(Token::Type) const;
        Position position() const;
        Position previous_token_end() const;
        std::string text_in_range(Position start, Position end) const;

        void save_state();
        void load_state();

        struct State {
            size_t token_index { 0 };
            std::vector<intrusive_ptr<ASTNode>> state_nodes;
        };

        void error(std::string_view message = {});

        template<class T, class... Args>
        intrusive_ptr<T>
        create_ast_node(ASTNode const& parent, Position const& start, std::optional<Position> end, Args&&... args)
        {
            auto node = intrusive_ptr(new T(&parent, start, end, m_filename, std::forward<Args>(args)...));

            if (m_saved_states.empty()) {
                m_nodes.push_back(node);
            } else {
                m_state.state_nodes.push_back(node);
            }

            return node;
        }

        intrusive_ptr<TranslationUnit>
        create_root_ast_node(Position const& start, Position end)
        {
            auto node = intrusive_ptr<TranslationUnit>(new TranslationUnit(nullptr, start, end, m_filename));
            m_nodes.push_back(node);
            m_root_node = node;
            return node;
        }

        DummyAstNode& get_dummy_node()
        {
            static intrusive_ptr<DummyAstNode> dummy(new DummyAstNode(nullptr, {}, {}, {}));
            return *dummy.get();
        }

        bool match_attribute_specification();
        void consume_attribute_specification();
        void consume_access_specifier();
        bool match_ellipsis();
        std::vector<std::string_view> parse_type_qualifiers();
        std::vector<std::string_view> parse_function_qualifiers();

        enum class CtorOrDtor {
            Ctor,
            Dtor,
        };
        void parse_constructor_or_destructor_impl(FunctionDeclaration&, CtorOrDtor);

        std::string m_filename;
        std::vector<Token> m_tokens;
        State m_state;
        std::vector<State> m_saved_states;
        intrusive_ptr<TranslationUnit> m_root_node;
        std::vector<std::string> m_errors;
        std::vector<intrusive_ptr<ASTNode>> m_nodes;
    };

}
