#pragma once

#include <vector>
#include <optional>
#include <functional>
#include <cassert>
#include <filesystem>
#include <fmt/core.h>
#include "intrusive_ptr.hh"

template<typename... Parameters>
void dbgln(const char* fmtstr, Parameters &&... parameters) {
    auto result = fmt::vformat(fmtstr, fmt::make_format_args(parameters...));
    printf("%s\n", result.c_str() );
}

template<typename... Parameters>
void out(FILE* file, const char* fmtstr, Parameters &&... parameters) {
    auto result = fmt::vformat(fmtstr, fmt::make_format_args(parameters...));
    fprintf(file, "%s", result.c_str() );
}

template<typename... Parameters>
void outln(FILE* file, const char* fmtstr, Parameters &&... parameters) {
    auto result = fmt::vformat(fmtstr, fmt::make_format_args(parameters...));
    fprintf(file, "%s\n", result.c_str() );
}

template<typename... Parameters>
void out(const char* fmtstr, Parameters &&... parameters) {
    auto result = fmt::vformat(fmtstr, fmt::make_format_args(parameters...));
    printf("%s", result.c_str() );
}

template<typename... Parameters>
void outln(const char* fmtstr, Parameters &&... parameters) {
    auto result = fmt::vformat(fmtstr, fmt::make_format_args(parameters...));
    printf("%s\n", result.c_str() );
}


template<typename T, typename U>
struct copy_const {
    enum { value = std::is_const<T>::value };

    using type = typename std::conditional<
            value,
            typename std::add_const<U>::type,
            typename std::remove_const<U>::type
    >::type;
};

template<typename T, typename U>
using copy_const_t = typename copy_const<T, U>::type;

template<typename OutputType, typename InputType>
copy_const_t<InputType, OutputType>* assert_cast(InputType* input)
{
    static_assert(std::is_base_of_v<InputType, OutputType>);
    return static_cast<copy_const_t<InputType, OutputType>*>(input);
}

template<typename OutputType, typename InputType>
copy_const_t<InputType, OutputType>& assert_cast(InputType& input)
{
    static_assert(std::is_base_of_v<InputType, OutputType>);
    return static_cast<copy_const_t<InputType, OutputType>&>(input);
}

template<typename ValueT, typename ErrorT>
class [[nodiscard]] Result {
public:
    using ValueType = ValueT;
    using ErrorType = ErrorT;

    Result(ValueType const& res)
            : m_result(res)
    {
    }

    Result(ValueType&& res)
            : m_result(move(res))
    {
    }

    Result(ErrorType const& error)
            : m_error(error)
    {
    }

    Result(ErrorType&& error)
            : m_error(move(error))
    {
    }

    Result(Result&& other) = default;
    Result(Result const& other) = default;
    ~Result() = default;

    ValueType& value()
    {
        return m_result.value();
    }

    ErrorType& error()
    {
        return m_error.value();
    }

    bool is_error() const
    {
        return m_error.has_value();
    }

    ValueType release_value()
    {
        return m_result.release_value();
    }

    ErrorType release_error()
    {
        return m_error.release_value();
    }

private:
    std::optional<ValueType> m_result;
    std::optional<ErrorType> m_error;
};

inline std::string join_strings(const char spacer, std::vector<std::string_view> const& src){
    std::string result;
    if(src.size() < 1) return "";
    for(int i = 0; i < src.size() - 1; ++i) {
        result+= src[i];
        result+= spacer;
    }
    result+= src.back();
    return result;
}

template<typename Callback>
class ScopeGuard {
public:
    ScopeGuard(Callback callback)
            : m_callback(std::move(callback))
    {
    }

    ~ScopeGuard()
    {
        m_callback();
    }

private:
    Callback m_callback;
};

template<typename Callback>
class ArmedScopeGuard {
public:
    ArmedScopeGuard(Callback callback)
            : m_callback(std::move(callback))
    {
    }

    ~ArmedScopeGuard()
    {
        if (m_armed)
            m_callback();
    }

    void disarm() { m_armed = false; }

private:
    Callback m_callback;
    bool m_armed { true };
};


class DirIterator {
public:
    enum Flags {
        None,
        SkipDots
    };

    DirIterator(std::string path, Flags flags)
            : m_path(path)
            , m_flags(flags)
    {
        for (const auto& entry : std::filesystem::directory_iterator(m_path)) {
            auto relative_path = std::filesystem::relative(entry.path(), m_path);
            if(m_flags == SkipDots && entry.path().string().starts_with('.'))
                continue;
            m_result.push_back(relative_path);
        }
        m_iterator = m_result.begin();
    }

    bool has_next() const {
        if(m_iterator != m_result.end())
            return true;
        return false;
    }

    std::string next_path() {
        auto result = *m_iterator;
        ++m_iterator;
        return result;
    }
protected:
    Flags m_flags;
    std::string m_path;
    std::vector<std::string> m_result;
    std::vector<std::string>::iterator m_iterator;
};