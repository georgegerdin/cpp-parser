/*
 * Copyright (c) 2018-2020, Andreas Kling <kling@serenityos.org>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#include <string_view>
#include <vector>
#include <functional>
#include "token.hh"

namespace Cpp {

    class Lexer {
    public:
        explicit Lexer(std::string_view, size_t start_line = 0);

        std::vector<Token> lex();
        template<typename Callback>
        void lex_iterable(Callback);

        void set_ignore_whitespace(bool value) { m_options.ignore_whitespace = value; }

    private:
        char peek(size_t offset = 0) const;
        char consume();
        void lex_impl(std::function<void(Token)>);

        std::string_view m_input;
        size_t m_index { 0 };
        Position m_previous_position { 0, 0 };
        Position m_position { 0, 0 };

        struct Options {
            bool ignore_whitespace { false };
        } m_options;
    };

    template<typename Callback>
    void Lexer::lex_iterable(Callback callback)
    {
        return lex_impl(std::move(callback));
    }

}