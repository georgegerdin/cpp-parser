#pragma once

#include <vector>
#include <optional>
#include <functional>
#include <cassert>
#include <fmt/core.h>


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

//
//  intrusive_ptr
//
//  A smart pointer that uses intrusive reference counting.
//
//  Relies on unqualified calls to
//
//      void intrusive_ptr_add_ref(T * p);
//      void intrusive_ptr_release(T * p);
//
//          (p != 0)
//
//  The object is responsible for destroying itself.
//

template<class T> class intrusive_ptr
{
private:
    typedef intrusive_ptr this_type;

public:
    typedef T element_type;

    constexpr intrusive_ptr() noexcept : px( 0 )
    {
    }

    intrusive_ptr( T * p, bool add_ref = true ) : px( p )
    {
        if( px != 0 && add_ref ) intrusive_ptr_add_ref( px );
    }

    template<class U>
    intrusive_ptr( intrusive_ptr<U> const & rhs ) : px( rhs.get() )
    {
        if( px != 0 ) intrusive_ptr_add_ref( px );
    }

    intrusive_ptr(intrusive_ptr const & rhs) : px( rhs.px )
    {
        if( px != 0 ) intrusive_ptr_add_ref( px );
    }

    ~intrusive_ptr()
    {
        if( px != 0 ) intrusive_ptr_release( px );
    }

    template<class U> intrusive_ptr & operator=(intrusive_ptr<U> const & rhs)
    {
        this_type(rhs).swap(*this);
        return *this;
    }

// Move support
    intrusive_ptr(intrusive_ptr && rhs) noexcept : px( rhs.px )
    {
        rhs.px = 0;
    }

    intrusive_ptr & operator=(intrusive_ptr && rhs) noexcept
    {
        this_type( static_cast< intrusive_ptr && >( rhs ) ).swap(*this);
        return *this;
    }

    template<class U> friend class intrusive_ptr;

    template<class U>
    intrusive_ptr(intrusive_ptr<U> && rhs) : px( rhs.px )
    {
        rhs.px = 0;
    }

    template<class U>
    intrusive_ptr & operator=(intrusive_ptr<U> && rhs) noexcept
    {
        this_type( static_cast< intrusive_ptr<U> && >( rhs ) ).swap(*this);
        return *this;
    }

    intrusive_ptr & operator=(intrusive_ptr const & rhs)
    {
        this_type(rhs).swap(*this);
        return *this;
    }

    intrusive_ptr & operator=(T * rhs)
    {
        this_type(rhs).swap(*this);
        return *this;
    }

    void reset()
    {
        this_type().swap( *this );
    }

    void reset( T * rhs )
    {
        this_type( rhs ).swap( *this );
    }

    void reset( T * rhs, bool add_ref )
    {
        this_type( rhs, add_ref ).swap( *this );
    }

    T * get() const noexcept
    {
        return px;
    }

    T * detach() noexcept
    {
        T * ret = px;
        px = 0;
        return ret;
    }

    T & operator*() const noexcept
    {
        assert( px != 0 );
        return *px;
    }

    T * operator->() const noexcept
    {
        assert( px != 0 );
        return px;
    }

// implicit conversion to "bool"
    operator bool () const noexcept
    {
        return px != 0;
    }

    void swap(intrusive_ptr & rhs) noexcept
    {
        T * tmp = px;
        px = rhs.px;
        rhs.px = tmp;
    }

private:

    T * px;
};

template<class T, class U> inline bool operator==(intrusive_ptr<T> const & a, intrusive_ptr<U> const & b) noexcept
{
    return a.get() == b.get();
}

template<class T, class U> inline bool operator!=(intrusive_ptr<T> const & a, intrusive_ptr<U> const & b) noexcept
{
    return a.get() != b.get();
}

template<class T, class U> inline bool operator==(intrusive_ptr<T> const & a, U * b) noexcept
{
    return a.get() == b;
}

template<class T, class U> inline bool operator!=(intrusive_ptr<T> const & a, U * b) noexcept
{
    return a.get() != b;
}

template<class T, class U> inline bool operator==(T * a, intrusive_ptr<U> const & b) noexcept
{
    return a == b.get();
}

template<class T, class U> inline bool operator!=(T * a, intrusive_ptr<U> const & b) noexcept
{
    return a != b.get();
}


template<class T> inline bool operator<(intrusive_ptr<T> const & a, intrusive_ptr<T> const & b) noexcept
{
    return std::less<T *>()(a.get(), b.get());
}

template<class T> void swap(intrusive_ptr<T> & lhs, intrusive_ptr<T> & rhs) noexcept
{
    lhs.swap(rhs);
}

template<typename T>
class intrusive_ref_counter {
public:
    intrusive_ref_counter() noexcept : m_ref_counter(0) { }
    intrusive_ref_counter(intrusive_ref_counter const&) noexcept : m_ref_counter(0) { }
    intrusive_ref_counter& operator= (intrusive_ref_counter const&) noexcept { return *this; }
    unsigned int use_count() const noexcept
    {
        return m_ref_counter;
    }
protected:
    ~intrusive_ref_counter() { }
    mutable unsigned int m_ref_counter;

    friend void intrusive_ptr_add_ref<T>(const intrusive_ref_counter<T>* p) noexcept;
    friend void intrusive_ptr_release<T>(const intrusive_ref_counter<T>* p) noexcept;
};

template< typename DerivedT >
inline void intrusive_ptr_add_ref(const intrusive_ref_counter<DerivedT>* p) noexcept
{
    p->m_ref_counter++;
}

template< typename DerivedT>
inline void intrusive_ptr_release(const intrusive_ref_counter< DerivedT>* p) noexcept
{
    p->m_ref_counter--;
    if (p->m_ref_counter == 0)
        delete static_cast< const DerivedT* >(p);
}