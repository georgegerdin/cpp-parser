#include <fmt/format.h>
#include <variant>
#include "cpp_parser/util.hh"
#include "cpp_parser/lexer.hh"
#include "cpp_parser/ast.hh"
#include "cpp_parser/preprocessor.hh"
#include "cpp_parser/parser.hh"
#include "cpp_parser/traverse_ast.hh"

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



int main(int argc, char* argv[]) {
    printf("argc: %d\n", argc);
    printf("argv[0]: %s\n", argv[0]);

    const char* test_string = "+";
    Cpp::Token tok {Cpp::Token::Type::Plus, {0, 0}, {0, 1}, test_string};
    printf("Token: %s\n", tok.to_string().c_str());


    outln("=========LEXER=====================");
    Cpp::Lexer lexer(c_string);
    auto tokens = lexer.lex();
    dbgln("Num tokens: {}", tokens.size());
    for(auto&& tok : tokens) {
        dbgln("Token: {}", tok.to_string().c_str());
    }
    outln("=========PREPROCESSOR==============");
    Cpp::Preprocessor preprocessor("test.cc", c_string);
    auto results = preprocessor.process_and_lex();
    for(auto&& tok : results) {
        dbgln("Token: {}", tok.to_string().c_str());
    }

    outln("==========PARSER===================");
    Cpp::Parser parser(results, "test.cc");
    auto translation_unit = parser.parse();
    translation_unit->dump();

    outln("==========TRAVERSE AST=============");
    {
        traverse_ast_tree(translation_unit, [] (Cpp::AstNodePtr node_ptr) {
            std::visit(Cpp::overloaded{
                [](Cpp::ASTNode const* astnode) { outln("ASTNode {}", astnode->class_name()); },
                [](Cpp::TranslationUnit const*) { outln("TranslationUnit"); },
                [](Cpp::StructOrClassDeclaration const* soc) { outln("StructOrClass {}", soc->full_name()); },
                [](Cpp::NamespaceDeclaration const* nd) { outln("Namespace {}", nd->full_name()); },
                [](Cpp::FunctionDeclaration const* fd) {
                    outln("Function {}()->{}", fd->full_name(), fd->return_type()->to_string());
                    outln("(");
                    for(auto&& param : fd->parameters()) {
                        outln("{} {}", param->type()->to_string(), param->full_name());
                    }
                    outln(")");
                }
            }, node_ptr);
        });
    }

    outln("===========SEARCH AST RECURSIVE=======");
    auto found_nodes = find_ast_node_recursively<Cpp::StructOrClassDeclaration>(translation_unit);
    for(auto&& node : found_nodes) {
        outln("StructOrClass: {}", node->full_name());
    }

    outln("===========SEARCH AST SHALLOW=======");
    auto ns = find_ast_node_shallow<Cpp::NamespaceDeclaration>(translation_unit.get());
    found_nodes = find_ast_node_shallow<Cpp::StructOrClassDeclaration>(ns.front());
    for(auto&& node : found_nodes) {
        outln("StructOrClass: {}", node->full_name());
    }
    return 0;
}