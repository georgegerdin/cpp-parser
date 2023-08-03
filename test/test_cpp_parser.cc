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

int main(int, char** argv) {
    test_search_ast();
    printf("All tests passed.\n");
    return 0;
}