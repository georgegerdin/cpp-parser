/*
 * Copyright (c) 2021, Itamar S. <itamar8910@gmail.com>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#include <string>
#include <functional>
#include <unordered_map>
#include <optional>
#include <string_view>
#include <vector>
#include "token.hh"
#include "genericlexer.hh"

namespace Cpp {

    class Preprocessor {

    public:
        explicit Preprocessor(std::string const& filename, std::string_view program);
        std::vector<Token> process_and_lex();
        std::vector<std::string_view> included_paths() const { return m_included_paths; }

        struct Definition {
            std::string key;
            std::vector<std::string> parameters;
            std::string value;
            std::string filename;
            size_t line { 0 };
            size_t column { 0 };
        };
        using Definitions = std::unordered_map<std::string, Definition>;

        struct Substitution {
            std::vector<Token> original_tokens;
            Definition defined_value;
            std::string processed_value;
        };

        Definitions const& definitions() const { return m_definitions; }
        std::vector<Substitution> const& substitutions() const { return m_substitutions; }

        void set_ignore_unsupported_keywords(bool ignore) { m_options.ignore_unsupported_keywords = ignore; }
        void set_ignore_invalid_statements(bool ignore) { m_options.ignore_invalid_statements = ignore; }
        void set_keep_include_statements(bool keep) { m_options.keep_include_statements = keep; }

        std::function<Definitions(std::string_view)> definitions_in_header_callback { nullptr };

        std::vector<Token> const& unprocessed_tokens() const { return m_unprocessed_tokens; }

    private:
        void handle_preprocessor_statement(std::string_view);
        void handle_include_statement(std::string_view);
        void handle_preprocessor_keyword(std::string_view keyword, GenericLexer& line_lexer);
        std::string remove_escaped_newlines(std::string_view value);

        size_t do_substitution(std::vector<Token> const& tokens, size_t token_index, Definition const&);
        std::optional<Definition> create_definition(std::string_view line);

        struct MacroCall {
            Token name;
            struct Argument {
                std::vector<Token> tokens;
            };
            std::vector<Argument> arguments;
            size_t end_token_index { 0 };
        };
        std::optional<MacroCall> parse_macro_call(std::vector<Token> const& tokens, size_t token_index);
        std::string evaluate_macro_call(MacroCall const&, Definition const&);

        std::string m_filename;
        std::string m_program;

        std::vector<Token> m_unprocessed_tokens;
        std::vector<Token> m_processed_tokens;
        Definitions m_definitions;
        std::vector<Substitution> m_substitutions;

        size_t m_current_line { 0 };
        size_t m_current_depth { 0 };
        std::vector<size_t> m_depths_of_taken_branches;
        std::vector<size_t> m_depths_of_not_taken_branches;

        enum class State {
            Normal,
            SkipIfBranch,
            SkipElseBranch
        };
        State m_state { State::Normal };

        std::vector<std::string_view> m_included_paths;

        struct Options {
            bool ignore_unsupported_keywords { false };
            bool ignore_invalid_statements { false };
            bool keep_include_statements { false };
        } m_options;
    };
}
