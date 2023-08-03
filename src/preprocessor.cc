/*
 * Copyright (c) 2021, Itamar S. <itamar8910@gmail.com>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#include <cassert>
#include <string>
#include <string_view>
#include <vector>
#include "cpp_parser/preprocessor.hh"
#include "cpp_parser/genericlexer.hh"
#include "cpp_parser/lexer.hh"

namespace Cpp {
    Preprocessor::Preprocessor(std::string const& filename, std::string_view program)
            : m_filename(filename)
            , m_program(program)
    {
    }

    std::vector<Token> Preprocessor::process_and_lex()
    {
        Lexer lexer { m_program };
        lexer.set_ignore_whitespace(true);
        auto tokens = lexer.lex();

        m_unprocessed_tokens = tokens;

        for (size_t token_index = 0; token_index < tokens.size(); ++token_index) {
            auto& token = tokens[token_index];
            m_current_line = token.start().line;
            if (token.type() == Token::Type::PreprocessorStatement) {
                handle_preprocessor_statement(token.text());
                m_processed_tokens.push_back(tokens[token_index]);
                continue;
            }

            if (m_state != State::Normal)
                continue;

            if (token.type() == Token::Type::IncludeStatement) {
                if (token_index >= tokens.size() - 1 || tokens[token_index + 1].type() != Token::Type::IncludePath)
                    continue;
                handle_include_statement(tokens[token_index + 1].text());
                if (m_options.keep_include_statements) {
                    m_processed_tokens.push_back(tokens[token_index]);
                    m_processed_tokens.push_back(tokens[token_index + 1]);
                }
                ++token_index; // Also skip IncludePath token
                continue;
            }

            if (token.type() == Token::Type::Identifier) {
                if (auto defined_value = m_definitions.find(std::string(token.text())); defined_value != m_definitions.end()) {
                    auto last_substituted_token_index = do_substitution(tokens, token_index, defined_value->second);
                    token_index = last_substituted_token_index;
                    continue;
                }
            }

            m_processed_tokens.push_back(token);
        }

        return m_processed_tokens;
    }

    static void consume_whitespace(GenericLexer& lexer)
    {
        auto ignore_line = [&] {
            for (;;) {
                if (lexer.consume_specific("\\\n"sv)) {
                    lexer.ignore(2);
                } else {
                    lexer.ignore_until('\n');
                    lexer.ignore();
                    break;
                }
            }
        };
        for (;;) {
            if (lexer.consume_specific("//"sv)) {
                ignore_line();
            } else if (lexer.consume_specific("/*"sv)) {
                lexer.ignore_until("*/");
                lexer.ignore(2);
            } else if (lexer.next_is("\\\n"sv)) {
                lexer.ignore(2);
            } else if (lexer.is_eof() || !lexer.next_is(isspace)) {
                break;
            } else {
                lexer.ignore();
            }
        }
    }

    void Preprocessor::handle_preprocessor_statement(std::string_view line)
    {
        GenericLexer lexer(line);

        consume_whitespace(lexer);
        lexer.consume_specific('#');
        consume_whitespace(lexer);
        auto keyword = lexer.consume_until(' ');
        lexer.ignore();
        if (keyword.empty() || std::all_of(keyword.begin(), keyword.end(), isspace))
            return;

        handle_preprocessor_keyword(keyword, lexer);
    }

    void Preprocessor::handle_include_statement(std::string_view include_path)
    {
        m_included_paths.push_back(include_path);
        if (definitions_in_header_callback) {
            for (auto& def : definitions_in_header_callback(include_path))
                m_definitions.insert(def);
        }
    }

    template<typename T, typename U>
    bool contains(T const& target, U&& value) {
        return std::find(target.begin(), target.end(), value) != target.end();
    }
    void Preprocessor::handle_preprocessor_keyword(std::string_view keyword, GenericLexer& line_lexer)
    {
        if (keyword == "include") {
            // Should have called 'handle_include_statement'.
            assert(false);
        }

        if (keyword == "else") {
            if (m_options.ignore_invalid_statements && m_current_depth == 0)
                return;
            assert(m_current_depth > 0);
            if (contains(m_depths_of_not_taken_branches, m_current_depth - 1)) {
                std::erase_if(m_depths_of_not_taken_branches, ([this](auto x) { return x == m_current_depth - 1; }));
                m_state = State::Normal;
            }
            if (contains(m_depths_of_taken_branches, m_current_depth - 1)) {
                m_state = State::SkipElseBranch;
            }
            return;
        }

        if (keyword == "endif") {
            if (m_options.ignore_invalid_statements && m_current_depth == 0)
                return;
            assert(m_current_depth > 0);
            --m_current_depth;
            if (contains(m_depths_of_not_taken_branches, m_current_depth)) {
                std::erase_if(m_depths_of_not_taken_branches, [this](auto x) { return x == m_current_depth; });
            }
            if (contains(m_depths_of_taken_branches, m_current_depth)) {
                std::erase_if(m_depths_of_taken_branches, [this](auto x) { return x == m_current_depth; });
            }
            m_state = State::Normal;
            return;
        }

        if (keyword == "define") {
            if (m_state == State::Normal) {
                auto definition = create_definition(line_lexer.consume_all());
                if (definition.has_value())
                    m_definitions.insert(std::make_pair(definition->key, *definition));
            }
            return;
        }
        if (keyword == "undef") {
            if (m_state == State::Normal) {
                auto key = line_lexer.consume_until(' ');
                line_lexer.consume_all();
                m_definitions.erase(std::string(key));
            }
            return;
        }
        if (keyword == "ifdef") {
            ++m_current_depth;
            if (m_state == State::Normal) {
                auto key = line_lexer.consume_until(' ');
                line_lexer.ignore();
                if (m_definitions.contains(std::string(key))) {
                    m_depths_of_taken_branches.push_back(m_current_depth - 1);
                    return;
                } else {
                    m_depths_of_not_taken_branches.push_back(m_current_depth - 1);
                    m_state = State::SkipIfBranch;
                    return;
                }
            }
            return;
        }
        if (keyword == "ifndef") {
            ++m_current_depth;
            if (m_state == State::Normal) {
                auto key = line_lexer.consume_until(' ');
                line_lexer.ignore();
                if (!m_definitions.contains(std::string(key))) {
                    m_depths_of_taken_branches.push_back(m_current_depth - 1);
                    return;
                } else {
                    m_depths_of_not_taken_branches.push_back(m_current_depth - 1);
                    m_state = State::SkipIfBranch;
                    return;
                }
            }
            return;
        }
        if (keyword == "if") {
            ++m_current_depth;
            if (m_state == State::Normal) {
                // FIXME: Implement #if logic
                // We currently always take #if branches.
                m_depths_of_taken_branches.push_back(m_current_depth - 1);
            }
            return;
        }

        if (keyword == "elif") {
            if (m_options.ignore_invalid_statements && m_current_depth == 0)
                return;
            assert(m_current_depth > 0);
            // FIXME: Evaluate the elif expression
            // We currently always treat the expression in #elif as true.
            if (contains(m_depths_of_not_taken_branches, m_current_depth - 1) /* && should_take*/) {
                std::erase_if(m_depths_of_not_taken_branches, [this](auto x) { return x == m_current_depth - 1; });
                m_state = State::Normal;
            }
            if (contains(m_depths_of_taken_branches, m_current_depth - 1)) {
                m_state = State::SkipElseBranch;
            }
            return;
        }
        if (keyword == "pragma") {
            line_lexer.consume_all();
            return;
        }
        if (keyword == "error") {
            line_lexer.consume_all();
            return;
        }

        if (!m_options.ignore_unsupported_keywords) {
            dbgln("Unsupported preprocessor keyword: {}", keyword);
            assert(false);
        }
    }

    size_t Preprocessor::do_substitution(std::vector<Token> const& tokens, size_t token_index, Definition const& defined_value)
    {
        if (defined_value.value.empty())
            return token_index;

        Substitution sub;
        sub.defined_value = defined_value;

        auto macro_call = parse_macro_call(tokens, token_index);

        if (!macro_call.has_value())
            return token_index;

        std::vector<Token> original_tokens;
        for (size_t i = token_index; i <= macro_call->end_token_index; ++i) {
            original_tokens.push_back(tokens[i]);
        }
        assert(!original_tokens.empty());

        auto processed_value = evaluate_macro_call(*macro_call, defined_value);
        m_substitutions.push_back({ original_tokens, defined_value, processed_value });

        Lexer lexer(processed_value);
        lexer.lex_iterable([&](auto token) {
            if (token.type() == Token::Type::Whitespace)
                return;
            token.set_start(original_tokens.front().start());
            token.set_end(original_tokens.front().end());
            m_processed_tokens.push_back(token);
        });
        return macro_call->end_token_index;
    }

    std::optional<Preprocessor::MacroCall> Preprocessor::parse_macro_call(std::vector<Token> const& tokens, size_t token_index)
    {
        auto name = tokens[token_index];
        ++token_index;

        if (token_index >= tokens.size() || tokens[token_index].type() != Token::Type::LeftParen)
            return MacroCall { name, {}, token_index - 1 };
        ++token_index;

        std::vector<MacroCall::Argument> arguments;
        std::optional<MacroCall::Argument> current_argument;

        size_t paren_depth = 1;
        for (; token_index < tokens.size(); ++token_index) {
            auto& token = tokens[token_index];
            if (token.type() == Token::Type::LeftParen)
                ++paren_depth;
            if (token.type() == Token::Type::RightParen)
                --paren_depth;

            if (paren_depth == 0) {
                if (current_argument.has_value())
                    arguments.push_back(*current_argument);
                break;
            }

            if (paren_depth == 1 && token.type() == Token::Type::Comma) {
                if (current_argument.has_value())
                    arguments.push_back(*current_argument);
                current_argument = {};
            } else {
                if (!current_argument.has_value())
                    current_argument = MacroCall::Argument {};
                current_argument->tokens.push_back(token);
            }
        }

        if (token_index >= tokens.size())
            return {};

        return MacroCall { name, move(arguments), token_index };
    }

    std::optional<Preprocessor::Definition> Preprocessor::create_definition(std::string_view line)
    {
        Lexer lexer { line };
        lexer.set_ignore_whitespace(true);
        auto tokens = lexer.lex();
        if (tokens.empty())
            return {};

        if (tokens.front().type() != Token::Type::Identifier)
            return {};

        Definition definition;
        definition.filename = m_filename;
        definition.line = m_current_line;

        definition.key = tokens.front().text();

        if (tokens.size() == 1)
            return definition;

        size_t token_index = 1;
        // Parse macro parameters (if any)
        if (tokens[token_index].type() == Token::Type::LeftParen) {
            ++token_index;
            while (token_index < tokens.size() && tokens[token_index].type() != Token::Type::RightParen) {
                auto param = tokens[token_index];
                if (param.type() != Token::Type::Identifier)
                    return {};

                if (token_index + 1 >= tokens.size())
                    return {};

                ++token_index;

                if (tokens[token_index].type() == Token::Type::Comma)
                    ++token_index;
                else if (tokens[token_index].type() != Token::Type::RightParen)
                    return {};

                definition.parameters.emplace_back(param.text());
            }
            if (token_index >= tokens.size())
                return {};
            ++token_index;
        }

        if (token_index < tokens.size())
            definition.value = remove_escaped_newlines(line.substr(tokens[token_index].start().column));

        return definition;
    }

    std::string Preprocessor::remove_escaped_newlines(std::string_view value)
    {
        static constexpr auto escaped_newline = "\\\n"sv;
        std::string processed_value;
        GenericLexer lexer { value };
        while (!lexer.is_eof()) {
            processed_value.append(lexer.consume_until(escaped_newline));
            lexer.ignore(escaped_newline.length());
        }
        return processed_value;
    }

    template<typename T, typename U>
    std::optional<size_t> find_first_index(T const& target, U&& value) {
        auto found = std::find(target.begin(), target.end(), value);
        if(found == target.end()) return std::nullopt;
        return found - target.begin();
    }

    std::string Preprocessor::evaluate_macro_call(MacroCall const& macro_call, Definition const& definition)
    {
        if (macro_call.arguments.size() != definition.parameters.size()) {
            dbgln("mismatch in # of arguments for macro call: {}", macro_call.name.text());
            return {};
        }

        Lexer lexer { definition.value };
        std::string processed_value;
        lexer.lex_iterable([&](auto token) {
            if (token.type() != Token::Type::Identifier) {
                processed_value.append(token.text());
                return;
            }

            auto param_index = find_first_index(definition.parameters, std::string(token.text()));
            if (!param_index.has_value()) {
                processed_value.append(token.text());
                return;
            }

            auto& argument = macro_call.arguments[*param_index];
            for (auto& arg_token : argument.tokens) {
                processed_value.append(arg_token.text());
            }
        });

        return processed_value;
    }

};
