/*
 * Copyright (c) 2021, Itamar S. <itamar8910@gmail.com>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#include <vector>
#include <string>

#include "cpp_parser/parser.hh"
#include "cpp_parser/ast.hh"
#include "cpp_parser/lexer.hh"
#include "cpp_parser/util.hh"

#define CPP_DEBUG false

//#define LOG_SCOPE() ScopeLogger<CPP_DEBUG> logger(std::string::formatted("'{}' - {} ({})", peek().text(), peek().type_as_deprecated_string(), m_state.token_index))
#define LOG_SCOPE()

namespace Cpp {

Parser::Parser(std::vector<Token> tokens, std::string const& filename)
        : m_filename(filename)
        , m_tokens(move(tokens))
{
    if constexpr (CPP_DEBUG) {
        dbgln("Tokens:");
        for (size_t i = 0; i < m_tokens.size(); ++i) {
            dbgln("{}- {}", i, m_tokens[i].text());
        }
    }
}

intrusive_ptr<TranslationUnit> Parser::parse()
{
    LOG_SCOPE();
    if (m_tokens.empty())
        return create_root_ast_node({}, {});
    auto unit = create_root_ast_node(m_tokens.front().start(), m_tokens.back().end());
    unit->set_declarations(parse_declarations_in_translation_unit(*unit));
    return unit;
}

std::vector<intrusive_ptr<Declaration const>> Parser::parse_declarations_in_translation_unit(ASTNode const& parent)
{
    std::vector<intrusive_ptr<Declaration const>> declarations;
    while (!eof()) {
        auto declaration = parse_single_declaration_in_translation_unit(parent);
        if (declaration) {
            declarations.push_back(declaration);
        } else {
            error("unexpected token"sv);
            consume();
        }
    }
    return declarations;
}

intrusive_ptr<Declaration const> Parser::parse_single_declaration_in_translation_unit(ASTNode const& parent)
{
    while (!eof()) {
        if (match_comment()) {
            consume(Token::Type::Comment);
            continue;
        }

        if (match_preprocessor()) {
            consume_preprocessor();
            continue;
        }

        auto declaration = match_declaration_in_translation_unit();
        if (declaration.has_value()) {
            return parse_declaration(parent, declaration.value());
        }
        return {};
    }
    return {};
}

intrusive_ptr<Declaration const> Parser::parse_declaration(ASTNode const& parent, DeclarationType declaration_type)
{
    switch (declaration_type) {
        case DeclarationType::Function:
            return parse_function_declaration(parent);
        case DeclarationType::Variable:
            return parse_variable_declaration(parent);
        case DeclarationType::Enum:
            return parse_enum_declaration(parent);
        case DeclarationType::Class:
            return parse_class_declaration(parent);
        case DeclarationType::Namespace:
            return parse_namespace_declaration(parent);
        case DeclarationType::Constructor:
            return parse_constructor(parent);
        case DeclarationType::Destructor:
            return parse_destructor(parent);
        case DeclarationType::UsingNamespace:
            return parse_using_namespace_declaration(parent);
        case DeclarationType::UsingType:
            return parse_using_type_declaration(parent);
        case DeclarationType::Typedef:
            return parse_typedef_declaration(parent);
        default:
            error("unexpected declaration type"sv);
            return create_ast_node<InvalidDeclaration>(parent, position(), position());
    }
}

intrusive_ptr<FunctionDeclaration const> Parser::parse_function_declaration(ASTNode const& parent)
{
    auto func = create_ast_node<FunctionDeclaration>(parent, position(), {});

    func->set_qualifiers(parse_function_qualifiers());
    func->set_return_type(parse_type(*func));

    func->set_name(parse_name(*func));

    consume(Token::Type::LeftParen);
    auto parameters = parse_parameter_list(*func);
    if (parameters.has_value())
        func->set_parameters(parameters.value());

    consume(Token::Type::RightParen);

    while (match_keyword("const") || match_keyword("override")) {
        consume();
        // FIXME: Note that this function is supposed to be a class member, and `this` has to be const, somehow.
    }

    intrusive_ptr<FunctionDefinition const> body;
    Position func_end {};
    if (peek(Token::Type::LeftCurly).has_value()) {
        body = parse_function_definition(*func);
        func_end = body->end();
    } else {
        func_end = position();
        if (match_attribute_specification())
            consume_attribute_specification(); // we don't use the value of __attribute__
        consume(Token::Type::Semicolon);
    }

    func->set_definition(std::move(body));
    func->set_end(func_end);
    return func;
}

intrusive_ptr<FunctionDefinition const> Parser::parse_function_definition(ASTNode const& parent)
{
    LOG_SCOPE();
    auto func = create_ast_node<FunctionDefinition>(parent, position(), {});
    consume(Token::Type::LeftCurly);
    while (!eof() && peek().type() != Token::Type::RightCurly) {
        func->add_statement(parse_statement(*func.get()));
    }
    func->set_end(position());
    if (!eof())
        consume(Token::Type::RightCurly);
    return func;
}

intrusive_ptr<Statement const> Parser::parse_statement(ASTNode const& parent)
{
    LOG_SCOPE();
    ArmedScopeGuard consume_semicolon([this]() {
        consume(Token::Type::Semicolon);
    });

    if (match_block_statement()) {
        consume_semicolon.disarm();
        return parse_block_statement(parent);
    }
    if (match_comment()) {
        consume_semicolon.disarm();
        return parse_comment(parent);
    }
    if (match_variable_declaration()) {
        return parse_variable_declaration(parent, false);
    }
    if (match_expression()) {
        return parse_expression(parent);
    }
    if (match_keyword("return")) {
        return parse_return_statement(parent);
    }
    if (match_keyword("for")) {
        consume_semicolon.disarm();
        return parse_for_statement(parent);
    }
    if (match_keyword("if")) {
        consume_semicolon.disarm();
        return parse_if_statement(parent);
    } else {
        error("unexpected statement type"sv);
        consume_semicolon.disarm();
        consume();
        return create_ast_node<InvalidStatement>(parent, position(), position());
    }
}

intrusive_ptr<Comment const> Parser::parse_comment(ASTNode const& parent)
{
    auto comment = create_ast_node<Comment>(parent, position(), {});
    consume(Token::Type::Comment);
    comment->set_end(position());
    return comment;
}

bool Parser::match_block_statement()
{
    return peek().type() == Token::Type::LeftCurly;
}

intrusive_ptr<BlockStatement const> Parser::parse_block_statement(ASTNode const& parent)
{
    LOG_SCOPE();
    auto block_statement = create_ast_node<BlockStatement>(parent, position(), {});
    consume(Token::Type::LeftCurly);
    while (!eof() && peek().type() != Token::Type::RightCurly) {
        block_statement->add_statement(parse_statement(*block_statement));
    }
    consume(Token::Type::RightCurly);
    block_statement->set_end(position());
    return block_statement;
}

bool Parser::match_type()
{
    return match_named_type();
}

bool Parser::match_named_type()
{
    save_state();
    ScopeGuard state_guard = [this] { load_state(); };

    parse_type_qualifiers();
    if (match_keyword("auto")) {
        return true;
    }

    if (match_keyword("struct")) {
        consume(Token::Type::Keyword); // Consume struct prefix
    }

    if (!match_name())
        return false;

    return true;
}

bool Parser::match_template_arguments()
{
    save_state();
    ScopeGuard state_guard = [this] { load_state(); };

    if (!peek(Token::Type::Less).has_value())
        return false;
    consume();

    while (!eof() && peek().type() != Token::Type::Greater) {
        if (!match_named_type())
            return false;
        (void)parse_type(get_dummy_node());
    }

    return peek().type() == Token::Type::Greater;
}

std::vector<intrusive_ptr<Type const>> Parser::parse_template_arguments(ASTNode const& parent)
{
    LOG_SCOPE();

    consume(Token::Type::Less);

    std::vector<intrusive_ptr<Type const>> template_arguments;
    while (!eof() && peek().type() != Token::Type::Greater) {
        template_arguments.push_back(parse_type(parent));
    }

    consume(Token::Type::Greater);

    return template_arguments;
}

bool Parser::match_variable_declaration()
{
    LOG_SCOPE();
    save_state();
    ScopeGuard state_guard = [this] { load_state(); };

    if (!match_type()) {
        return false;
    }

    assert(m_root_node);
    (void)parse_type(get_dummy_node());

    // Identifier
    if (!match_name())
        return false;

    (void)parse_name(get_dummy_node());

    while (!eof() && (peek().type() == Token::Type::LeftBracket)) {
        consume(Token::Type::LeftBracket);

        if (match(Token::Type::Integer)) {
            consume(Token::Type::Integer);
        }
        if (!match(Token::Type::RightBracket)) {
            error("No closing right bracket"sv);
            return false;
        }
        consume(Token::Type::RightBracket);
    }

    if (match(Token::Type::Equals)) {
        consume(Token::Type::Equals);
        if (!match_expression()) {
            error("initial value of variable is not an expression"sv);
            return false;
        }
        return true;
    }

    if (match_braced_init_list())
        (void)parse_braced_init_list(get_dummy_node());

    return match(Token::Type::Semicolon);
}

intrusive_ptr<VariableDeclaration const> Parser::parse_variable_declaration(ASTNode const& parent, bool expect_semicolon)
{
    LOG_SCOPE();
    auto var = create_ast_node<VariableDeclaration>(parent, position(), {});
    if (!match_variable_declaration()) {
        error("unexpected token for variable type"sv);
        var->set_end(position());
        return var;
    }
    var->set_type(parse_type(*var.get()));
    auto name = parse_name(*var);
    intrusive_ptr<Expression const> initial_value;

    if (match(Token::Type::Equals)) {
        consume(Token::Type::Equals);
        initial_value = parse_expression(*var.get());
    }

    if (match_braced_init_list()) {
        initial_value = parse_braced_init_list(*var.get());
    }

    if (expect_semicolon)
        consume(Token::Type::Semicolon);

    var->set_end(position());
    var->set_name(name);
    var->set_initial_value(std::move(initial_value));

    return var;
}

intrusive_ptr<Expression const> Parser::parse_expression(ASTNode const& parent)
{
    LOG_SCOPE();
    auto expression = parse_primary_expression(parent);
    // TODO: remove eof() logic, should still work without it
    if (eof() || match(Token::Type::Semicolon)) {
        return expression;
    }

    std::vector<intrusive_ptr<Expression const>> secondary_expressions;

    while (match_secondary_expression()) {
        // FIXME: Handle operator precedence
        expression = parse_secondary_expression(parent, expression);
        secondary_expressions.push_back(expression);
    }

    for (size_t i = 0; secondary_expressions.size() != 0 && i < secondary_expressions.size() - 1; ++i) {
        const_cast<Expression&>(*secondary_expressions[i].get()).set_parent(*secondary_expressions[i + 1].get());
    }

    return expression;
}

bool Parser::match_secondary_expression()
{
    auto type = peek().type();
    return type == Token::Type::Plus
           || type == Token::Type::PlusEquals
           || type == Token::Type::Minus
           || type == Token::Type::MinusEquals
           || type == Token::Type::Asterisk
           || type == Token::Type::AsteriskEquals
           || type == Token::Type::Percent
           || type == Token::Type::PercentEquals
           || type == Token::Type::Equals
           || type == Token::Type::Greater
           || type == Token::Type::GreaterEquals
           || type == Token::Type::Less
           || type == Token::Type::LessEquals
           || type == Token::Type::Dot
           || type == Token::Type::PlusPlus
           || type == Token::Type::MinusMinus
           || type == Token::Type::And
           || type == Token::Type::AndEquals
           || type == Token::Type::Pipe
           || type == Token::Type::PipeEquals
           || type == Token::Type::Caret
           || type == Token::Type::CaretEquals
           || type == Token::Type::LessLess
           || type == Token::Type::LessLessEquals
           || type == Token::Type::GreaterGreater
           || type == Token::Type::GreaterGreaterEquals
           || type == Token::Type::EqualsEquals
           || type == Token::Type::AndAnd
           || type == Token::Type::PipePipe
           || type == Token::Type::ExclamationMarkEquals
           || type == Token::Type::Arrow
           || type == Token::Type::LeftParen;
}

intrusive_ptr<Expression const> Parser::parse_primary_expression(ASTNode const& parent)
{
    LOG_SCOPE();
    // TODO: remove eof() logic, should still work without it
    if (eof()) {
        auto node = create_ast_node<Identifier>(parent, position(), position());
        return node;
    }

    if (match_unary_expression())
        return parse_unary_expression(parent);

    if (match_literal()) {
        return parse_literal(parent);
    }

    if (match_cpp_cast_expression())
        return parse_cpp_cast_expression(parent);

    if (match_c_style_cast_expression())
        return parse_c_style_cast_expression(parent);

    if (match_sizeof_expression())
        return parse_sizeof_expression(parent);

    if (match_new_expression())
        return parse_new_expression(parent);

    if (match_braced_init_list())
        return parse_braced_init_list(parent);

    if (match_name()) {
        return parse_name(parent);
    }

    error("could not parse primary expression"sv);
    auto token = consume();
    return create_ast_node<InvalidExpression>(parent, token.start(), token.end());
}

bool Parser::match_literal()
{
    switch (peek().type()) {
        case Token::Type::Integer:
            return true;
        case Token::Type::SingleQuotedString:
            return true;
        case Token::Type::DoubleQuotedString:
            return true;
        case Token::Type::Float:
            return true;
        case Token::Type::Keyword: {
            return match_boolean_literal() || peek().text() == "nullptr";
        }
        default:
            return false;
    }
}

bool Parser::match_unary_expression()
{
    auto type = peek().type();
    return type == Token::Type::PlusPlus
           || type == Token::Type::MinusMinus
           || type == Token::Type::ExclamationMark
           || type == Token::Type::Tilde
           || type == Token::Type::Plus
           || type == Token::Type::Minus
           || type == Token::Type::And
           || type == Token::Type::Asterisk;
}

intrusive_ptr<UnaryExpression const> Parser::parse_unary_expression(ASTNode const& parent)
{
    auto unary_exp = create_ast_node<UnaryExpression>(parent, position(), {});
    auto op_token = consume();
    UnaryOp op { UnaryOp::Invalid };
    switch (op_token.type()) {
        case Token::Type::Minus:
            op = UnaryOp::Minus;
            break;
        case Token::Type::Plus:
            op = UnaryOp::Plus;
            break;
        case Token::Type::ExclamationMark:
            op = UnaryOp::Not;
            break;
        case Token::Type::Tilde:
            op = UnaryOp::BitwiseNot;
            break;
        case Token::Type::PlusPlus:
            op = UnaryOp::PlusPlus;
            break;
        case Token::Type::And:
            op = UnaryOp::Address;
            break;
        case Token::Type::Asterisk:
            op = UnaryOp::Dereference;
            break;
        default:
            break;
    }
    unary_exp->set_op(op);
    auto lhs = parse_expression(*unary_exp);
    unary_exp->set_lhs(lhs.get());
    unary_exp->set_end(lhs->end());
    return unary_exp;
}

intrusive_ptr<Expression const> Parser::parse_literal(ASTNode const& parent)
{
    switch (peek().type()) {
        case Token::Type::Integer: {
            auto token = consume();
            return create_ast_node<NumericLiteral>(parent, token.start(), token.end(), text_of_token(token));
        }
        case Token::Type::SingleQuotedString:
            [[fallthrough]];
        case Token::Type::DoubleQuotedString:
            return parse_string_literal(parent);
        case Token::Type::Keyword: {
            if (match_boolean_literal())
                return parse_boolean_literal(parent);
            if (peek().text() == "nullptr") {
                auto token = consume();
                return create_ast_node<NullPointerLiteral>(parent, token.start(), token.end());
            }
            [[fallthrough]];
        }
        default: {
            error("could not parse literal"sv);
            auto token = consume();
            return create_ast_node<InvalidExpression>(parent, token.start(), token.end());
        }
    }
}

intrusive_ptr<Expression const> Parser::parse_secondary_expression(ASTNode const& parent, intrusive_ptr<Expression const> lhs)
{
    LOG_SCOPE();
    switch (peek().type()) {
        case Token::Type::Plus:
            return parse_binary_expression(parent, lhs, BinaryOp::Addition);
        case Token::Type::Less:
            return parse_binary_expression(parent, lhs, BinaryOp::LessThan);
        case Token::Type::EqualsEquals:
            return parse_binary_expression(parent, lhs, BinaryOp::EqualsEquals);
        case Token::Type::ExclamationMarkEquals:
            return parse_binary_expression(parent, lhs, BinaryOp::NotEqual);
        case Token::Type::And:
            return parse_binary_expression(parent, lhs, BinaryOp::BitwiseAnd);
        case Token::Type::AndAnd:
            return parse_binary_expression(parent, lhs, BinaryOp::LogicalAnd);
        case Token::Type::Pipe:
            return parse_binary_expression(parent, lhs, BinaryOp::BitwiseOr);
        case Token::Type::PipePipe:
            return parse_binary_expression(parent, lhs, BinaryOp::LogicalOr);
        case Token::Type::Arrow:
            return parse_binary_expression(parent, lhs, BinaryOp::Arrow);
        case Token::Type::Equals:
            return parse_assignment_expression(parent, lhs, AssignmentOp::Assignment);
        case Token::Type::Dot: {
            consume();
            auto exp = create_ast_node<MemberExpression>(parent, lhs->start(), {});
            const_cast<Expression&>(*lhs).set_parent(*exp);
            exp->set_object(std::move(lhs));
            auto identifier_token = consume(Token::Type::Identifier);
            exp->set_property(create_ast_node<Identifier>(*exp, identifier_token.start(), identifier_token.end(), identifier_token.text()));
            exp->set_end(position());
            return exp;
        }
        case Token::Type::LeftParen: {
            consume();
            auto func = create_ast_node<FunctionCall>(parent, lhs->start(), {});
            const_cast<Expression&>(*lhs).set_parent(*func);
            func->set_callee(std::move(lhs));
            while (peek().type() != Token::Type::RightParen && !eof()) {
                func->add_argument(parse_expression(*func));
                if (peek().type() == Token::Type::Comma)
                    consume(Token::Type::Comma);
            }
            consume(Token::Type::RightParen);
            func->set_end(position());
            return func;
        }
        default: {
            error(fmt::format("unexpected operator for expression. operator: {}", peek().to_string()));
            auto token = consume();
            return create_ast_node<InvalidExpression>(parent, token.start(), token.end());
        }
    }
}

intrusive_ptr<BinaryExpression const> Parser::parse_binary_expression(ASTNode const& parent, intrusive_ptr<Expression const> lhs, BinaryOp op)
{
    consume(); // Operator
    auto exp = create_ast_node<BinaryExpression>(parent, lhs->start(), {});
    const_cast<Expression&>(*lhs).set_parent(*exp);
    exp->set_op(op);
    exp->set_lhs(std::move(lhs));
    auto rhs = parse_expression(*exp.get());
    exp->set_end(rhs->end());
    exp->set_rhs(std::move(rhs));
    return exp;
}

intrusive_ptr<AssignmentExpression const> Parser::parse_assignment_expression(ASTNode const& parent, intrusive_ptr<Expression const> lhs, AssignmentOp op)
{
    consume(); // Operator
    auto exp = create_ast_node<AssignmentExpression>(parent, lhs->start(), {});
    const_cast<Expression&>(*lhs).set_parent(*exp);
    exp->set_op(op);
    exp->set_lhs(std::move(lhs));
    auto rhs = parse_expression(*exp.get());
    exp->set_end(rhs->end());
    exp->set_rhs(std::move(rhs));
    return exp;
}

std::optional<Parser::DeclarationType> Parser::match_declaration_in_translation_unit()
{
    if (match_function_declaration())
        return DeclarationType::Function;
    if (match_enum_declaration())
        return DeclarationType::Enum;
    if (match_class_declaration())
        return DeclarationType::Class;
    if (match_namespace_declaration())
        return DeclarationType::Namespace;
    if (match_variable_declaration())
        return DeclarationType::Variable;
    if (match_constructor())
        return DeclarationType::Constructor;
    if (match_destructor())
        return DeclarationType::Destructor;
    if (match_using_namespace_declaration())
        return DeclarationType::UsingNamespace;
    if (match_using_type_declaration())
        return DeclarationType::UsingType;
    if (match_typedef_declaration())
        return DeclarationType::Typedef;
    return {};
}

std::optional<Parser::DeclarationType> Parser::match_class_member(std::string_view class_name)
{
    if (match_function_declaration())
        return DeclarationType::Function;
    if (match_enum_declaration())
        return DeclarationType::Enum;
    if (match_class_declaration())
        return DeclarationType::Class;
    if (match_variable_declaration())
        return DeclarationType::Variable;
    if (match_constructor(class_name))
        return DeclarationType::Constructor;
    if (match_destructor(class_name))
        return DeclarationType::Destructor;
    return {};
}

bool Parser::match_enum_declaration()
{
    save_state();
    ScopeGuard state_guard = [this] { load_state(); };

    if (!match_keyword("enum"))
        return false;

    consume(Token::Type::Keyword);

    if (match_keyword("class"))
        consume(Token::Type::Keyword);

    if (!match(Token::Type::Identifier))
        return false;

    consume(Token::Type::Identifier);

    return match(Token::Type::LeftCurly);
}

bool Parser::match_class_declaration()
{
    save_state();
    ScopeGuard state_guard = [this] { load_state(); };

    if (!match_keyword("struct") && !match_keyword("class"))
        return false;

    consume(Token::Type::Keyword);

    if (!match(Token::Type::Identifier))
        return false;

    consume(Token::Type::Identifier);

    if(match_keyword("final"))
        consume();

    if (peek().type() == Token::Type::Colon) {
        do {
            consume();

            while (match_keyword("private") || match_keyword("public") || match_keyword("protected") || match_keyword("virtual"))
                consume();

            if (!match_name())
                return false;
            (void)parse_name(get_dummy_node());
        } while (peek().type() == Token::Type::Comma);
    }

    return match(Token::Type::LeftCurly);
}

bool Parser::match_namespace_declaration()
{
    return match_keyword("namespace");
}

bool Parser::match_function_declaration()
{
    save_state();
    ScopeGuard state_guard = [this] { load_state(); };

    parse_function_qualifiers();

    if (!match_type())
        return false;

    assert(m_root_node);
    (void)parse_type(get_dummy_node());

    if (!match_name())
        return false;

    (void)parse_name(get_dummy_node());

    if (!peek(Token::Type::LeftParen).has_value())
        return false;
    consume();

    while (consume().type() != Token::Type::RightParen && !eof()) { };

    while (match_keyword("const") || match_keyword("override"))
        consume();

    if (peek(Token::Type::Semicolon).has_value() || peek(Token::Type::LeftCurly).has_value())
        return true;

    if (match_attribute_specification()) {
        consume_attribute_specification();
        return peek(Token::Type::Semicolon).has_value();
    }

    return false;
}

std::optional<std::vector<intrusive_ptr<Parameter const>>> Parser::parse_parameter_list(ASTNode const& parent)
{
    LOG_SCOPE();
    std::vector<intrusive_ptr<Parameter const>> parameters;
    while (peek().type() != Token::Type::RightParen && !eof()) {
        if (match_ellipsis()) {
            auto param = create_ast_node<Parameter>(parent, position(), {}, intrusive_ptr<Name> {});
            consume(Token::Type::Dot);
            consume(Token::Type::Dot);
            auto last_dot = consume(Token::Type::Dot);
            param->set_ellipsis(true);
            param->set_end(last_dot.end());
            parameters.push_back(std::move(param));
        } else {
            auto type = parse_type(parent);

            intrusive_ptr<Name const> name;
            if (match_name()) {
                name = parse_name(parent);
            }

            auto param = create_ast_node<Parameter>(parent, type->start(), name ? name->end() : type->end(), name);
            const_cast<Type&>(*type).set_parent(*param.get());

            param->set_type(std::move(type));
            parameters.push_back(std::move(param));
        }

        if (peek(Token::Type::Comma).has_value())
            consume(Token::Type::Comma);
    }
    return parameters;
}

bool Parser::match_comment()
{
    return match(Token::Type::Comment);
}

bool Parser::match_whitespace()
{
    return match(Token::Type::Whitespace);
}

bool Parser::match_preprocessor()
{
    return match(Token::Type::PreprocessorStatement) || match(Token::Type::IncludeStatement);
}

void Parser::consume_preprocessor()
{
    LOG_SCOPE();
    switch (peek().type()) {
        case Token::Type::PreprocessorStatement:
            consume();
            break;
        case Token::Type::IncludeStatement:
            consume();
            consume(Token::Type::IncludePath);
            break;
        default:
            error("unexpected token while parsing preprocessor statement"sv);
            consume();
    }
}

std::optional<Token> Parser::consume_whitespace()
{
    LOG_SCOPE();
    return consume(Token::Type::Whitespace);
}

Token Parser::consume(Token::Type type)
{
    auto token = consume();
    if (token.type() != type)
        error(fmt::format("expected {} at {}:{}, found: {}", Token::type_to_string(type), token.start().line, token.start().column, Token::type_to_string(token.type())));
    return token;
}

bool Parser::match(Token::Type type)
{
    return peek().type() == type;
}

Token Parser::consume()
{
    if (eof()) {
        error("C++ Parser: out of tokens"sv);
        return { Token::Type::EOF_TOKEN, position(), position(), {} };
    }
    return m_tokens[m_state.token_index++];
}

Token Parser::peek(size_t offset) const
{
    if (m_state.token_index + offset >= m_tokens.size())
        return { Token::Type::EOF_TOKEN, position(), position(), {} };
    return m_tokens[m_state.token_index + offset];
}

std::optional<Token> Parser::peek(Token::Type type) const
{
    auto token = peek();
    if (token.type() == type)
        return token;
    return {};
}

void Parser::save_state()
{
    m_saved_states.push_back(m_state);
    m_state.state_nodes.clear();
}

void Parser::load_state()
{
    m_state = m_saved_states.back();
    m_saved_states.pop_back();
}

std::string_view Parser::text_of_token(Cpp::Token const& token) const
{
    return token.text();
}

std::string Parser::text_of_node(ASTNode const& node) const
{
    return text_in_range(node.start(), node.end());
}

std::string Parser::text_in_range(Position start, Position end) const
{
    std::string builder;
    for (auto token : tokens_in_range(start, end)) {
        builder.append(token.text());
    }
    return builder;
}

std::vector<Token> Parser::tokens_in_range(Position start, Position end) const
{
    auto start_token_index = index_of_token_at(start);
    auto end_node_index = index_of_token_at(end);
    assert(start_token_index.has_value());
    assert(end_node_index.has_value());

    std::vector<Token> tokens;
    for (size_t i = start_token_index.value(); i <= end_node_index.value(); ++i) {
        tokens.push_back(m_tokens[i]);
    }
    return tokens;
}

void Parser::error(std::string_view message)
{
    LOG_SCOPE();

    if (!m_saved_states.empty())
        return;

    if (message.empty())
        message = "<empty>"sv;
    std::string formatted_message;
    if (m_state.token_index >= m_tokens.size()) {
        formatted_message = fmt::format("C++ Parsed error on EOF.{}", message);
    } else {
        formatted_message = fmt::format("C++ Parser error: {}. token: {} ({}:{})",
                                                        message,
                                                        m_state.token_index < m_tokens.size() ? text_of_token(m_tokens[m_state.token_index]) : "EOF"sv,
                                                        m_tokens[m_state.token_index].start().line,
                                                        m_tokens[m_state.token_index].start().column);
    }

    m_errors.push_back(formatted_message);
}

bool Parser::match_expression()
{
    return match_literal()
           || match_name()
           || match_unary_expression()
           || match_cpp_cast_expression()
           || match_c_style_cast_expression()
           || match_sizeof_expression()
           || match_braced_init_list();
}

bool Parser::eof() const
{
    return m_state.token_index >= m_tokens.size();
}

Position Parser::position() const
{
    if (m_tokens.empty())
        return {};

    if (eof())
        return m_tokens.back().end();

    return peek().start();
}

Position Parser::previous_token_end() const
{
    if (m_state.token_index < 1)
        return {};
    return m_tokens[m_state.token_index - 1].end();
}

intrusive_ptr<ASTNode const> Parser::node_at(Position pos) const
{
    assert(m_saved_states.empty());
    auto index = index_of_node_at(pos);
    if (!index.has_value())
        return nullptr;
    return m_nodes[index.value()];
}

std::optional<size_t> Parser::index_of_node_at(Position pos) const
{
    assert(!m_tokens.empty());
    assert(m_saved_states.empty());
    std::optional<size_t> match_node_index;

    auto node_span = [](ASTNode const& node) {
        assert(node.end().line >= node.start().line);
        assert((node.end().line > node.start().line) || (node.end().column >= node.start().column));
        return Position { node.end().line - node.start().line, node.start().line != node.end().line ? 0 : node.end().column - node.start().column };
    };

    for (size_t node_index = 0; node_index < m_nodes.size(); ++node_index) {
        auto& node = m_nodes[node_index];
        if (node->start() > pos || node->end() < pos)
            continue;

        if (!match_node_index.has_value() || (node_span(*node.get()) <= node_span(*m_nodes[match_node_index.value()].get())))
            match_node_index = node_index;
    }
    return match_node_index;
}

std::optional<Token> Parser::token_at(Position pos) const
{
    auto index = index_of_token_at(pos);
    if (!index.has_value())
        return {};
    return m_tokens[index.value()];
}

std::optional<size_t> Parser::index_of_token_at(Position pos) const
{
    for (size_t token_index = 0; token_index < m_tokens.size(); ++token_index) {
        auto token = m_tokens[token_index];
        if (token.start() > pos || token.end() < pos)
            continue;
        return token_index;
    }
    return {};
}

void Parser::print_tokens() const
{
    for (auto& token : m_tokens) {
        outln("{}", token.to_string());
    }
}

template<typename T, typename U>
bool contains(T target, U const& value) {
    return target.find(value) != target.npos;
}

std::vector<CodeComprehension::TodoEntry> Parser::get_todo_entries() const
{
    std::vector<CodeComprehension::TodoEntry> ret;
    for (auto& token : m_tokens) {
        if (token.type() == Token::Type::Comment) {
            if (contains(token.text(), "TODO"sv) || contains(token.text(), "FIXME"sv)) {
                ret.emplace_back(CodeComprehension::TodoEntry{ std::string(token.text()), m_filename, token.start().line, token.start().column });
            }
        }
    }
    return ret;
}

intrusive_ptr<StringLiteral const> Parser::parse_string_literal(ASTNode const& parent)
{
    LOG_SCOPE();
    std::optional<size_t> start_token_index;
    std::optional<size_t> end_token_index;
    while (!eof()) {
        auto token = peek();
        if (token.type() != Token::Type::DoubleQuotedString && token.type() != Token::Type::SingleQuotedString && token.type() != Token::Type::EscapeSequence) {
            assert(start_token_index.has_value());
            end_token_index = m_state.token_index - 1;
            break;
        }
        if (!start_token_index.has_value())
            start_token_index = m_state.token_index;
        consume();
    }

    // String was not terminated
    if (!end_token_index.has_value()) {
        end_token_index = m_tokens.size() - 1;
    }

    assert(start_token_index.has_value());
    assert(end_token_index.has_value());

    Token start_token = m_tokens[start_token_index.value()];
    Token end_token = m_tokens[end_token_index.value()];

    auto text = text_in_range(start_token.start(), end_token.end());
    auto string_literal = create_ast_node<StringLiteral>(parent, start_token.start(), end_token.end());
    string_literal->set_value(std::move(text));
    return string_literal;
}

intrusive_ptr<ReturnStatement const> Parser::parse_return_statement(ASTNode const& parent)
{
    LOG_SCOPE();
    auto return_statement = create_ast_node<ReturnStatement>(parent, position(), {});
    consume(Token::Type::Keyword);
    if (!peek(Token::Type::Semicolon).has_value()) {
        return_statement->set_value(parse_expression(*return_statement));
    }
    return_statement->set_end(position());
    return return_statement;
}

intrusive_ptr<EnumDeclaration const> Parser::parse_enum_declaration(ASTNode const& parent)
{
    LOG_SCOPE();
    auto enum_decl = create_ast_node<EnumDeclaration>(parent, position(), {});
    consume_keyword("enum");

    if (match_keyword("class")) {
        consume(Token::Type::Keyword);
        enum_decl->set_type(EnumDeclaration::Type::EnumClass);
    } else {
        enum_decl->set_type(EnumDeclaration::Type::RegularEnum);
    }

    auto name = parse_name(*enum_decl);
    enum_decl->set_name(name);
    consume(Token::Type::LeftCurly);
    while (!eof() && peek().type() != Token::Type::RightCurly) {
        auto name = text_of_token(consume(Token::Type::Identifier));
        intrusive_ptr<Expression const> value;
        if (peek().type() == Token::Type::Equals) {
            consume();
            value = parse_expression(*enum_decl.get());
        }
        enum_decl->add_entry(name, std::move(value));
        if (peek().type() != Token::Type::Comma) {
            break;
        }
        consume(Token::Type::Comma);
    }
    consume(Token::Type::RightCurly);
    consume(Token::Type::Semicolon);
    enum_decl->set_end(position());
    return enum_decl;
}

Token Parser::consume_keyword(std::string const& keyword)
{
    auto token = consume();
    if (token.type() != Token::Type::Keyword) {
        error(fmt::format("unexpected token: {}, expected Keyword", token.to_string()));
        return token;
    }
    if (text_of_token(token) != keyword) {
        error(fmt::format("unexpected keyword: {}, expected {}", text_of_token(token), keyword));
        return token;
    }
    return token;
}

bool Parser::match_keyword(std::string const& keyword)
{
    auto token = peek();
    if (token.type() != Token::Type::Keyword) {
        return false;
    }
    if (text_of_token(token) != keyword) {
        return false;
    }
    return true;
}

intrusive_ptr<StructOrClassDeclaration const> Parser::parse_class_declaration(ASTNode const& parent)
{
    LOG_SCOPE();

    auto type_token = consume(Token::Type::Keyword);
    StructOrClassDeclaration::Type type {};

    if (type_token.text() == "struct")
        type = StructOrClassDeclaration::Type::Struct;
    if (type_token.text() == "class")
        type = StructOrClassDeclaration::Type::Class;

    auto decl = create_ast_node<StructOrClassDeclaration>(parent, position(), {}, type);

    auto name = parse_name(*decl);
    decl->set_name(name);

    auto has_final = match_keyword("final");

    std::vector<intrusive_ptr<Name const>> baseclasses;

    // FIXME: Don't ignore this.
    if (peek(has_final ? 1 : 0).type() == Token::Type::Colon) {
        if (has_final)
            consume();

        do {
            consume();

            while (match_keyword("private") || match_keyword("public") || match_keyword("protected") || match_keyword("virtual"))
                consume();

            baseclasses.push_back(parse_name(*decl));
        } while (peek().type() == Token::Type::Comma);
    }

    decl->set_baseclasses(std::move(baseclasses));

    consume(Token::Type::LeftCurly);

    while (!eof() && peek().type() != Token::Type::RightCurly) {
        decl->set_members(parse_class_members(*decl));
    }

    consume(Token::Type::RightCurly);
    consume(Token::Type::Semicolon);
    decl->set_end(position());
    return decl;
}

intrusive_ptr<BooleanLiteral const> Parser::parse_boolean_literal(ASTNode const& parent)
{
    LOG_SCOPE();
    auto token = consume(Token::Type::Keyword);
    auto text = text_of_token(token);
    // text == "true" || text == "false";
    bool value = (text == "true");
    return create_ast_node<BooleanLiteral>(parent, token.start(), token.end(), value);
}

bool Parser::match_boolean_literal()
{
    auto token = peek();
    if (token.type() != Token::Type::Keyword)
        return false;
    auto text = text_of_token(token);
    return text == "true" || text == "false";
}

template<typename T>
void extend(T& target, T const& src) {
    target.insert(target.end(), src.begin(), src.end());
}

intrusive_ptr<Type const> Parser::parse_type(ASTNode const& parent)
{
    LOG_SCOPE();

    if (!match_named_type()) {
        error("expected named named_type"sv);
        auto token = consume();
        return create_ast_node<NamedType>(parent, token.start(), token.end());
    }

    auto named_type = create_ast_node<NamedType>(parent, position(), {});

    auto qualifiers = parse_type_qualifiers();
    named_type->set_qualifiers(std::move(qualifiers));

    if (match_keyword("auto")) {
        consume(Token::Type::Keyword);
        named_type->set_auto(true);
        auto original_qualifiers = named_type->qualifiers();
        extend(original_qualifiers, parse_type_qualifiers());
        named_type->set_qualifiers(std::move(original_qualifiers));
        named_type->set_end(position());
        return named_type;
    }

    if (match_keyword("struct")) {
        consume(Token::Type::Keyword); // Consume struct prefix
    }

    if (!match_name()) {
        named_type->set_end(position());
        error(fmt::format("expected name instead of: {}", peek().text()));
        return named_type;
    }
    named_type->set_name(parse_name(*named_type));

    auto original_qualifiers = named_type->qualifiers();
    extend(original_qualifiers, parse_type_qualifiers());
    named_type->set_qualifiers(std::move(original_qualifiers));

    intrusive_ptr<Type> type = named_type;
    while (!eof() && peek().type() == Token::Type::Asterisk) {
        type->set_end(position());
        auto asterisk = consume();
        auto ptr = create_ast_node<Pointer>(parent, type->start(), asterisk.end());
        type->set_parent(*ptr);
        ptr->set_pointee(type);
        ptr->set_qualifiers(parse_type_qualifiers());
        ptr->set_end(position());
        type = ptr;
    }

    if (!eof() && (peek().type() == Token::Type::And || peek().type() == Token::Type::AndAnd)) {
        type->set_end(position());
        auto ref_token = consume();
        auto ref = create_ast_node<Reference>(parent, type->start(), ref_token.end(), ref_token.type() == Token::Type::And ? Reference::Kind::Lvalue : Reference::Kind::Rvalue);
        type->set_parent(*ref);
        ref->set_referenced_type(type);
        ref->set_end(position());
        type = ref;
    }

    if (peek().type() == Token::Type::LeftParen) {
        type->set_end(previous_token_end());
        consume();
        auto fn_type = create_ast_node<FunctionType>(parent, type->start(), position());
        fn_type->set_return_type(*type);
        type->set_parent(*fn_type);
        if (auto parameters = parse_parameter_list(*type); parameters.has_value())
            fn_type->set_parameters(parameters.value());
        consume(Token::Type::RightParen);
        type = fn_type;
    }

    type->set_end(previous_token_end());

    return type;
}

intrusive_ptr<ForStatement const> Parser::parse_for_statement(ASTNode const& parent)
{
    LOG_SCOPE();
    auto for_statement = create_ast_node<ForStatement>(parent, position(), {});
    consume(Token::Type::Keyword);
    consume(Token::Type::LeftParen);
    if (peek().type() != Token::Type::Semicolon)
        for_statement->set_init(parse_variable_declaration(*for_statement, false));
    consume(Token::Type::Semicolon);

    if (peek().type() != Token::Type::Semicolon)
        for_statement->set_test(parse_expression(*for_statement));
    consume(Token::Type::Semicolon);

    if (peek().type() != Token::Type::RightParen)
        for_statement->set_update(parse_expression(*for_statement));
    consume(Token::Type::RightParen);

    for_statement->set_body(parse_statement(*for_statement));

    for_statement->set_end(for_statement->body()->end());
    return for_statement;
}

intrusive_ptr<IfStatement const> Parser::parse_if_statement(ASTNode const& parent)
{
    LOG_SCOPE();
    auto if_statement = create_ast_node<IfStatement>(parent, position(), {});
    consume(Token::Type::Keyword);
    consume(Token::Type::LeftParen);
    if_statement->set_predicate(parse_expression(*if_statement));
    consume(Token::Type::RightParen);
    if_statement->set_then_statement(parse_statement(*if_statement));
    if (match_keyword("else")) {
        consume(Token::Type::Keyword);
        if_statement->set_else_statement(parse_statement(*if_statement));
        if_statement->set_end(if_statement->else_statement()->end());
    } else {
        if_statement->set_end(if_statement->then_statement()->end());
    }
    return if_statement;
}
std::vector<std::string_view> Parser::parse_type_qualifiers()
{
    LOG_SCOPE();
    std::vector<std::string_view> qualifiers;
    while (!eof()) {
        auto token = peek();
        if (token.type() != Token::Type::Keyword)
            break;
        auto text = text_of_token(token);
        if (text == "static" || text == "const" || text == "extern") {
            qualifiers.push_back(text);
            consume();
        } else {
            break;
        }
    }
    return qualifiers;
}

std::vector<std::string_view> Parser::parse_function_qualifiers()
{
    LOG_SCOPE();
    std::vector<std::string_view> qualifiers;
    while (!eof()) {
        auto token = peek();
        if (token.type() != Token::Type::Keyword)
            break;
        auto text = text_of_token(token);
        if (text == "static" || text == "inline" || text == "extern" || text == "virtual") {
            qualifiers.push_back(text);
            consume();
        } else {
            break;
        }
    }
    return qualifiers;
}

bool Parser::match_attribute_specification()
{
    return text_of_token(peek()) == "__attribute__";
}
void Parser::consume_attribute_specification()
{
    consume(); // __attribute__
    consume(Token::Type::LeftParen);
    size_t left_count = 1;
    while (!eof()) {
        auto token = consume();
        if (token.type() == Token::Type::LeftParen) {
            ++left_count;
        }
        if (token.type() == Token::Type::RightParen) {
            --left_count;
        }
        if (left_count == 0)
            return;
    }
}

bool Parser::match_ellipsis()
{
    if (m_state.token_index > m_tokens.size() - 3)
        return false;
    return peek().type() == Token::Type::Dot && peek(1).type() == Token::Type::Dot && peek(2).type() == Token::Type::Dot;
}

intrusive_ptr<NamespaceDeclaration const> Parser::parse_namespace_declaration(ASTNode const& parent, bool is_nested_namespace)
{
    auto namespace_decl = create_ast_node<NamespaceDeclaration>(parent, position(), {});

    if (!is_nested_namespace)
        consume(Token::Type::Keyword);

    auto name = parse_name(*namespace_decl);
    namespace_decl->set_name(name);

    if (peek().type() == Token::Type::ColonColon) {
        consume(Token::Type::ColonColon);
        namespace_decl->add_declaration(parse_namespace_declaration(*namespace_decl, true));
        namespace_decl->set_end(position());
        return namespace_decl;
    }

    consume(Token::Type::LeftCurly);
    while (!eof() && peek().type() != Token::Type::RightCurly) {
        auto declaration = parse_single_declaration_in_translation_unit(*namespace_decl);
        if (declaration) {
            namespace_decl->add_declaration(std::move(declaration));
        } else {
            error("unexpected token"sv);
            consume();
        }
    }
    consume(Token::Type::RightCurly);
    namespace_decl->set_end(position());
    return namespace_decl;
}

bool Parser::match_name()
{
    auto type = peek().type();
    return type == Token::Type::Identifier || type == Token::Type::KnownType;
}

intrusive_ptr<Name const> Parser::parse_name(ASTNode const& parent)
{
    LOG_SCOPE();
    intrusive_ptr<Name> name_node = create_ast_node<Name>(parent, position(), {});
    while (!eof() && (peek().type() == Token::Type::Identifier || peek().type() == Token::Type::KnownType) && peek(1).type() == Token::Type::ColonColon) {
        auto token = consume();
        name_node->add_to_scope(create_ast_node<Identifier>(*name_node, token.start(), token.end(), token.text()));
        consume(Token::Type::ColonColon);
    }

    if (peek().type() == Token::Type::Identifier || peek().type() == Token::Type::KnownType) {
        auto token = consume();
        name_node->set_name(create_ast_node<Identifier>(*name_node, token.start(), token.end(), token.text()));
    } else {
        name_node->set_end(position());
        return name_node;
    }

    bool is_templatized = false;
    if (match_template_arguments()) {
        is_templatized = true;
        consume(Token::Type::Less);
        intrusive_ptr<TemplatizedName> templatized_name = create_ast_node<TemplatizedName>(parent, name_node->start(), {});
        templatized_name->set_name(name_node->name());
        templatized_name->set_scope(name_node->scope());
        name_node->set_end(position());
        name_node = templatized_name;
        while (peek().type() != Token::Type::Greater && !eof()) {
            templatized_name->add_template_argument(parse_type(*templatized_name));
            if (peek().type() == Token::Type::Comma)
                consume(Token::Type::Comma);
        }
        consume(Token::Type::Greater);
    }

    if (!is_templatized && (peek().type() == Token::Type::LeftBracket)) {
        intrusive_ptr<SizedName> sized_name = create_ast_node<SizedName>(parent, name_node->start(), {});
        sized_name->set_name(name_node->name());
        sized_name->set_scope(name_node->scope());

        while (peek().type() == Token::Type::LeftBracket) {
            consume(Token::Type::LeftBracket);

            std::string_view size = "0"sv;
            if (peek().type() == Token::Type::Integer) {
                auto token = consume(Token::Type::Integer);
                size = token.text();
            }
            sized_name->append_dimension(size);

            consume(Token::Type::RightBracket);
        }
        name_node->set_end(position());
        name_node = sized_name;
    }

    name_node->set_end(previous_token_end());
    return name_node;
}

bool Parser::match_cpp_cast_expression()
{
    save_state();
    ScopeGuard state_guard = [this] { load_state(); };

    auto token = consume();
    if (token.type() != Token::Type::Keyword)
        return false;

    auto text = token.text();
    if (text == "static_cast" || text == "reinterpret_cast" || text == "dynamic_cast" || text == "const_cast")
        return true;
    return false;
}

bool Parser::match_c_style_cast_expression()
{
    save_state();
    ScopeGuard state_guard = [this] { load_state(); };

    if (consume().type() != Token::Type::LeftParen)
        return false;

    if (!match_type())
        return false;
    (void)parse_type(get_dummy_node());

    if (consume().type() != Token::Type::RightParen)
        return false;

    if (!match_expression())
        return false;

    return true;
}

intrusive_ptr<CStyleCastExpression const> Parser::parse_c_style_cast_expression(ASTNode const& parent)
{
    auto parse_exp = create_ast_node<CStyleCastExpression>(parent, position(), {});

    consume(Token::Type::LeftParen);
    parse_exp->set_type(parse_type(*parse_exp));
    consume(Token::Type::RightParen);
    parse_exp->set_expression(parse_expression(*parse_exp));
    parse_exp->set_end(position());

    return parse_exp;
}

intrusive_ptr<CppCastExpression const> Parser::parse_cpp_cast_expression(ASTNode const& parent)
{
    auto cast_expression = create_ast_node<CppCastExpression>(parent, position(), {});

    cast_expression->set_cast_type(consume(Token::Type::Keyword).text());

    consume(Token::Type::Less);
    cast_expression->set_type(parse_type(*cast_expression));
    consume(Token::Type::Greater);

    consume(Token::Type::LeftParen);
    cast_expression->set_expression(parse_expression(*cast_expression));
    consume(Token::Type::RightParen);

    cast_expression->set_end(position());

    return cast_expression;
}

bool Parser::match_sizeof_expression()
{
    return match_keyword("sizeof");
}

bool Parser::match_new_expression()
{
    return match_keyword("new");
}

intrusive_ptr<SizeofExpression const> Parser::parse_sizeof_expression(ASTNode const& parent)
{
    auto exp = create_ast_node<SizeofExpression>(parent, position(), {});
    consume(Token::Type::Keyword);
    consume(Token::Type::LeftParen);
    exp->set_type(parse_type(parent));
    consume(Token::Type::RightParen);
    exp->set_end(position());
    return exp;
}


intrusive_ptr<NewExpression const> Parser::parse_new_expression(ASTNode const& parent)
{
    auto exp = create_ast_node<NewExpression>(parent, position(), {});
    consume(Token::Type::Keyword);
    exp->set_type(parse_type(parent));
    exp->set_end(position());
    return exp;
}

bool Parser::match_braced_init_list()
{
    return match(Token::Type::LeftCurly);
}

intrusive_ptr<BracedInitList const> Parser::parse_braced_init_list(ASTNode const& parent)
{
    auto init_list = create_ast_node<BracedInitList>(parent, position(), {});

    consume(Token::Type::LeftCurly);
    while (!eof() && peek().type() != Token::Type::RightCurly) {
        init_list->add_expression(parse_expression(*init_list));
        if (peek().type() == Token::Type::Comma)
            consume(Token::Type::Comma);
    }
    consume(Token::Type::RightCurly);
    init_list->set_end(position());
    return init_list;
}
std::vector<intrusive_ptr<Declaration const>> Parser::parse_class_members(StructOrClassDeclaration& parent)
{
    auto class_name = parent.full_name();

    std::vector<intrusive_ptr<Declaration const>> members;
    while (!eof() && peek().type() != Token::Type::RightCurly) {
        if (match_access_specifier())
            consume_access_specifier(); // FIXME: Do not ignore access specifiers
        auto member_type = match_class_member(class_name);
        if (member_type.has_value()) {
            members.push_back(parse_declaration(parent, member_type.value()));
        } else {
            error("Expected class member"sv);
            consume();
        }
    }
    return members;
}

bool Parser::match_access_specifier()
{
    if (peek(1).type() != Token::Type::Colon)
        return false;

    return match_keyword("private") || match_keyword("protected") || match_keyword("public");
}

void Parser::consume_access_specifier()
{
    consume(Token::Type::Keyword);
    consume(Token::Type::Colon);
}

bool Parser::match_constructor(std::string_view class_name)
{
    save_state();
    ScopeGuard state_guard = [this] { load_state(); };

    auto token = consume();
    if (token.text() != class_name)
        return false;

    if (!peek(Token::Type::LeftParen).has_value())
        return false;
    consume();

    while (consume().type() != Token::Type::RightParen && !eof()) { };

    return (peek(Token::Type::Semicolon).has_value() || peek(Token::Type::LeftCurly).has_value());
}


bool Parser::match_constructor() {
    save_state();
    ScopeGuard state_guard = [this] { load_state(); };

    std::vector<Token> name_tokens;
    bool first = true;
    do {
        if(!first) {
            if(!match(Token::Type::ColonColon))
                return false;
            consume();
        }
        else
            first = false;

        if(match(Token::Type::Tilde))
            return false; // destructor
        name_tokens.push_back(consume());
    } while(!peek(Token::Type::LeftParen).has_value());

    if(name_tokens.size() < 2)
        return false;

    // The last and second to last token in the name should be the same
    if(name_tokens.back().text() != name_tokens.end()[-2].text())
        return false;

    if (!peek(Token::Type::LeftParen).has_value())
        return false;
    consume();

    while (consume().type() != Token::Type::RightParen && !eof()) { };

    if(match(Token::Type::Colon)) {
        // consume member initializer list
        while (peek().type() != Token::Type::LeftCurly && !eof()) {
            consume();
        };
    }

    return (peek(Token::Type::Semicolon).has_value() || peek(Token::Type::LeftCurly).has_value());
}


bool Parser::match_destructor(std::string_view class_name)
{
    save_state();
    ScopeGuard state_guard = [this] { load_state(); };

    if (match_keyword("virtual"))
        consume();

    if (!match(Token::Type::Tilde))
        return false;
    consume();

    auto token = peek();

    if (token.text() != class_name)
        return false;
    consume();

    if (!peek(Token::Type::LeftParen).has_value())
        return false;
    consume();

    while (consume().type() != Token::Type::RightParen && !eof()) { };

    if (match_keyword("override"))
        consume();

    return (peek(Token::Type::Semicolon).has_value() || peek(Token::Type::LeftCurly).has_value());
}

bool Parser::match_destructor()
{
    save_state();
    ScopeGuard state_guard = [this] { load_state(); };

    std::vector<Token> name_tokens;
    bool first = true;
    bool tilde_found_on_the_last = false;
    do {
        if(!first) {
            if(!match(Token::Type::ColonColon))
                return false;
            consume();
        }
        else
            first = false;

        if(match(Token::Type::Tilde)) {
            tilde_found_on_the_last = true;
            consume();
        }
        else {
            tilde_found_on_the_last = false;
        }

        name_tokens.push_back(consume());
    } while(!peek(Token::Type::LeftParen).has_value());

    if (!tilde_found_on_the_last)
        return false;

    if(name_tokens.size() < 2)
        return false;

    // The last and second to last token in the name should be the same
    if(name_tokens.back().text() != name_tokens.end()[-2].text())
        return false;

    if (!peek(Token::Type::LeftParen).has_value())
        return false;
    consume();

    while (consume().type() != Token::Type::RightParen && !eof()) { };

    if (match_keyword("override"))
        consume();

    return (peek(Token::Type::Semicolon).has_value() || peek(Token::Type::LeftCurly).has_value());
}

void Parser::parse_constructor_or_destructor_impl(FunctionDeclaration& func, CtorOrDtor type)
{
    if (type == CtorOrDtor::Dtor) {
        if (match_keyword("virtual"))
            func.set_qualifiers({ consume().text() });
        consume(Token::Type::Tilde);
    }

    auto name = parse_name(func);
    func.set_name(name);

    consume(Token::Type::LeftParen);
    auto parameters = parse_parameter_list(func);
    if (parameters.has_value()) {
        if (type == CtorOrDtor::Dtor && !parameters->empty())
            error("Destructor declaration that takes parameters"sv);
        else
            func.set_parameters(parameters.value());
    }

    consume(Token::Type::RightParen);

    if (type == CtorOrDtor::Dtor && match_keyword("override"))
        consume();

    // TODO: Parse =default, =delete.

    if(type == CtorOrDtor::Ctor && match(Token::Type::Colon)) {
        // consume member initializer list
        // TODO: Save it in an AST node
        while (peek().type() != Token::Type::LeftCurly && !eof()) {
            consume();
        };
    }

    intrusive_ptr<FunctionDefinition const> body;
    Position ctor_end {};
    if (peek(Token::Type::LeftCurly).has_value()) {
        body = parse_function_definition(func);
        ctor_end = body->end();
    } else {
        ctor_end = position();
        if (match_attribute_specification())
            consume_attribute_specification(); // we don't use the value of __attribute__
        consume(Token::Type::Semicolon);
    }

    func.set_definition(std::move(body));
    func.set_end(ctor_end);
}

intrusive_ptr<Constructor const> Parser::parse_constructor(ASTNode const& parent)
{
    auto ctor = create_ast_node<Constructor>(parent, position(), {});
    parse_constructor_or_destructor_impl(*ctor, CtorOrDtor::Ctor);
    return ctor;
}

intrusive_ptr<Destructor const> Parser::parse_destructor(ASTNode const& parent)
{
    auto ctor = create_ast_node<Destructor>(parent, position(), {});
    parse_constructor_or_destructor_impl(*ctor, CtorOrDtor::Dtor);
    return ctor;
}

bool Parser::match_using_namespace_declaration()
{
    save_state();
    ScopeGuard state_guard = [this] { load_state(); };

    if (!match_keyword("using"))
        return false;
    consume();

    if (!match_keyword("namespace"))
        return false;
    consume();

    return true;
}

bool Parser::match_using_type_declaration()
{
    save_state();
    ScopeGuard state_guard = [this] { load_state(); };

    if (!match_keyword("using"))
        return false;
    consume();

    if (!match(Token::Type::Identifier))
        return false;

    return true;
}

bool Parser::match_typedef_declaration()
{
    save_state();
    ScopeGuard state_guard = [this] { load_state(); };

    if (!match_keyword("typedef"))
        return false;
    consume();

    // FIXME: typedef void (*fn)()

    if (!match_type())
        return false;

    if (!match(Token::Type::Identifier))
        return false;

    return true;
}

intrusive_ptr<UsingNamespaceDeclaration const> Parser::parse_using_namespace_declaration(ASTNode const& parent)
{
    auto decl = create_ast_node<UsingNamespaceDeclaration>(parent, position(), {});

    consume_keyword("using");
    consume_keyword("namespace");

    auto name = parse_name(*decl);

    decl->set_end(position());
    consume(Token::Type::Semicolon);

    decl->set_name(name);

    return decl;
}

intrusive_ptr<TypedefDeclaration const> Parser::parse_typedef_declaration(Cpp::ASTNode const& parent)
{
    auto decl = create_ast_node<TypedefDeclaration>(parent, position(), {});

    consume_keyword("typedef");

    auto type = parse_type(*decl);
    decl->set_alias(*type.get());

    auto name = parse_name(*decl);
    decl->set_name(name);

    decl->set_end(position());
    consume(Token::Type::Semicolon);

    return decl;
}

intrusive_ptr<TypedefDeclaration const> Parser::parse_using_type_declaration(Cpp::ASTNode const& parent)
{
    auto decl = create_ast_node<TypedefDeclaration>(parent, position(), {});

    // FIXME: These can also be templated.
    consume_keyword("using");

    auto name = parse_name(*decl);
    decl->set_name(name);

    if (match(Token::Type::Equals)) {
        consume();
        auto type = parse_type(*decl);
        decl->set_alias(*type.get());
    }

    decl->set_end(position());
    consume(Token::Type::Semicolon);

    return decl;
}

}
