#pragma once

#include <variant>

#include "ast.hh"
#include "util.hh"

namespace Cpp {

template<class... Ts>
struct overloaded : Ts... { using Ts::operator()...; };
template<class... Ts>
overloaded(Ts...) -> overloaded<Ts...>;

using AstNodePtr = std::variant<
Cpp::ASTNode const*
, Cpp::TranslationUnit const*
, Cpp::NamespaceDeclaration const*
, Cpp::StructOrClassDeclaration const*
, Cpp::FunctionDeclaration const*
>;



enum RecursePolicy{
    NoRecurse,
    Recurse
};

auto visit_declarations = [](auto const& target, auto callback, RecursePolicy recurse_policy) {
    for(auto&& declaration : target->declarations()) {
        if(declaration->is_struct_or_class())
            traverse_ast_tree(static_cast<Cpp::StructOrClassDeclaration const*>(declaration.get()), callback, recurse_policy);
        else if(declaration->is_namespace())
            traverse_ast_tree(static_cast<Cpp::NamespaceDeclaration const*>(declaration.get()), callback, recurse_policy);
        else if(declaration->is_function())
            traverse_ast_tree(static_cast<Cpp::FunctionDeclaration const*>(declaration.get()), callback, recurse_policy);
        else
            traverse_ast_tree(static_cast<Cpp::ASTNode const*>(declaration.get()), callback, recurse_policy);
    }
};

template<typename T>
void traverse_ast_tree(Cpp::StructOrClassDeclaration const* struct_or_class, T callback, RecursePolicy recurse_policy) {
    AstNodePtr struct_or_class_ptr = struct_or_class;
    callback(struct_or_class_ptr);

    if(recurse_policy == RecursePolicy::Recurse)
        visit_declarations(struct_or_class, callback, recurse_policy);
}

template<typename T>
void traverse_ast_tree(Cpp::NamespaceDeclaration const* namespace_decl, T callback, RecursePolicy recurse_policy) {
    AstNodePtr namespace_ptr = namespace_decl;
    callback(namespace_ptr);

    if(recurse_policy == RecursePolicy::Recurse)
        visit_declarations(namespace_decl, callback, recurse_policy);
}

template<typename T>
void traverse_ast_tree(Cpp::FunctionDeclaration const* function_decl, T callback, RecursePolicy) {
    AstNodePtr function_decl_ptr = function_decl;
    callback(function_decl_ptr);
}

template<typename T>
void traverse_ast_tree(Cpp::ASTNode const* ast_node, T callback, RecursePolicy) {
    AstNodePtr ast_node_ptr = ast_node;
    callback(ast_node_ptr);
}

template<typename T>
void traverse_ast_tree(intrusive_ptr<Cpp::TranslationUnit> translation_unit, T callback) {
    AstNodePtr translation_unit_ptr = translation_unit.get();
    callback(translation_unit_ptr);

    visit_declarations(translation_unit, callback, RecursePolicy::Recurse);
}

template<typename T, typename Tcallback>
void visit_ast_node(T const* parent_node, Tcallback callback) {
    AstNodePtr parent_node_ptr = parent_node;
    callback(parent_node_ptr);

    visit_declarations(parent_node, callback, RecursePolicy::NoRecurse);
}


template<typename T, typename U>
std::vector<T const*> find_ast_node_recursively(intrusive_ptr<U> const& parent) {
std::vector<T const*> result;

traverse_ast_tree(parent, [&](AstNodePtr node_ptr) {
std::visit(overloaded{
        [&](Cpp::StructOrClassDeclaration const* node) {
            result.push_back(node);
        },
        [](auto const*) {  },
}, node_ptr);
});
return result;
}

template<typename T, typename U>
std::vector<T const*> find_ast_node_shallow(U const& parent) {
    std::vector<T const*> result;

    visit_ast_node(parent, [&](AstNodePtr node_ptr) {
        std::visit(overloaded{
                [&](T const* node) {
                    result.push_back(node);
                },
                [](auto const*) {  },
        }, node_ptr);
    });
    return result;
}

}