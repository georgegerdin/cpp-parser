#include <cassert>
#include "cpp_parser/parser.hh"
#include "cpp_parser/traverse_ast.hh"

void test_search_ast() {
    auto c_string = "namespace test {     "
                    "struct A {           "
                    "   struct B {        "
                    "   };                "
                    "   int b;            "
                    "};                   "
                    "                     "
                    "inline bool c() {    "
                    "   return true;      "
                    "}                    "
                    "                     "
                    "int main(int argc    "
                    "    , char** argv) { "
                    "   int a = 0;        "
                    "   float b = 0.0f    "
                    "}                    "
                    "}                    "
    ;

    Cpp::Preprocessor preprocessor("test.cc", c_string);
    auto results = preprocessor.process_and_lex();
    Cpp::Parser parser(results, "test.cc");
    auto translation_unit = parser.parse();

    auto ns = find_ast_node_shallow<Cpp::NamespaceDeclaration>(translation_unit.get());
    auto found_nodes = find_ast_node_shallow<Cpp::StructOrClassDeclaration>(ns.front());
    assert(found_nodes.size() == 1);
}

void test_template_function() {
    auto c_string = "class Parser {                         \n"
                    "    template<class T, class... Args>   \n"
                    "    NonnullRefPtr<T>                   \n"
                    "    create_ast_node(ASTNode const& parent, Position const& start, Optional<Position> end, Args&&... args) \n"
                    "    {                                  \n"
                    "        auto node = adopt_ref(*new T(&parent, start, end, m_filename, forward<Args>(args)...)); \n"
                    "        if (m_saved_states.is_empty()) { \n"
                    "            m_nodes.append(node);        \n"
                    "        } else {                         \n"
                    "            m_state.state_nodes.append(node); \n"
                    "        }                                \n"
                    "        return node;                     \n"
                    "     }                                   \n"
                    "};                    \n"
    ;

    Cpp::Preprocessor preprocessor("test.cc", c_string);
    auto results = preprocessor.process_and_lex();
    Cpp::Parser parser(results, "test.cc");
    auto translation_unit = parser.parse();

    translation_unit->dump();
}

void test_unary_expression() {
    auto c_string = "int main() {     \n"
                    "    &renault;    \n"
                    "    *renault;    \n"
                    "    *new T(1);   \n"
                    "}                \n"
    ;
    Cpp::Preprocessor preprocessor("test.cc", c_string);
    auto results = preprocessor.process_and_lex();
    Cpp::Parser parser(results, "test.cc");
    auto translation_unit = parser.parse();

    translation_unit->dump();
}

void test_final() {
    auto c_string = "struct S final : public Base {   \n"
                    "   int m;    \n"
                    "};           \n"
                    "struct R final {   \n"
                    "   int m;    \n"
                    "};           \n"
    ;
    Cpp::Preprocessor preprocessor("test.cc", c_string);
    auto results = preprocessor.process_and_lex();
    Cpp::Parser parser(results, "test.cc");
    auto translation_unit = parser.parse();

    translation_unit->dump();
}


void test_constructor() {
    auto c_string = "S::S() : m(a) {        \n"
                    "}               \n"
                    "S::~S() {       \n"
                    "}"
    ;
    Cpp::Preprocessor preprocessor("test.cc", c_string);
    auto results = preprocessor.process_and_lex();
    Cpp::Parser parser(results, "test.cc");
    auto translation_unit = parser.parse();

    translation_unit->dump();
}
void test_if_constexpr() {
    auto c_string = "void foo() {                       \n"
                    "    if (bar) {                     \n"
                    "    }                              \n"
                    "    if constexpr(bar) {            \n"
                    "    }                              \n"
                    "}"
    ;
    Cpp::Preprocessor preprocessor("test.cc", c_string);
    auto results = preprocessor.process_and_lex();
    Cpp::Parser parser(results, "test.cc");
    auto translation_unit = parser.parse();

    translation_unit->dump();
}

int main(int, char** argv) {
    test_search_ast();
    test_template_function();
    test_unary_expression();
    test_final();
    test_constructor();
    test_if_constexpr();
    printf("All tests passed.\n");
    return 0;
}