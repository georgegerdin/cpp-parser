/*
 * Copyright (c) 2020, the SerenityOS developers.
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#include <fmt/format.h>
#include "cpp_parser/token.hh"

namespace Cpp {

    bool Position::operator<(Position const& other) const
    {
        return line < other.line || (line == other.line && column < other.column);
    }
    bool Position::operator>(Position const& other) const
    {
        return !(*this < other) && !(*this == other);
    }
    bool Position::operator==(Position const& other) const
    {
        return line == other.line && column == other.column;
    }
    bool Position::operator<=(Position const& other) const
    {
        return !(*this > other);
    }

    std::string Token::to_string() const
    {
        return fmt::format("{}  {}:{}-{}:{} ({})", type_to_string(m_type), start().line, start().column, end().line, end().column, text());
    }

    std::string Token::type_as_string() const
    {
        return type_to_string(m_type);
    }

}