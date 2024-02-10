//
// Created by george on 2024-02-03.
//

#ifndef INTRUSIVE_PTR_HH
#define INTRUSIVE_PTR_HH

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

    intrusive_ptr & operator=(T& rhs) {
        this_type(&rhs).swap(*this);
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

    operator T* () const noexcept
    {
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

    bool is_null() const noexcept
    {
        if(px == nullptr) return false;
        return true;
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

#endif //INTRUSIVE_PTR_HH