CMAKE_MINIMUM_REQUIRED(VERSION 3.0)

project(Precompiler)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++20")

find_package(fmt)

add_library(cpp-parser
    src/ast.cc
    src/genericlexer.cc
    src/lexer.cc
    src/parser.cc
    src/preprocessor.cc
    src/token.cc
)

add_executable(cpp-parser-example
        examples/cpp_parser_example.cc
)

add_executable(test-cpp-parser
        test/test_cpp_parser.cc
)

target_link_libraries(cpp-parser PUBLIC fmt::fmt)
target_include_directories(cpp-parser PUBLIC include)
target_link_libraries(cpp-parser-example PUBLIC cpp-parser)
target_link_libraries(test-cpp-parser PUBLIC cpp-parser)