cmake_minimum_required(VERSION 3.9)

project(fun_rt DESCRIPTION "runtime for fun4fun")

# compile_commands.json
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

# Source files
file(GLOB SOURCES ${PROJECT_SOURCE_DIR}/src/*.c)

add_library(fun_rt STATIC ${SOURCES})

# Include directories
target_include_directories(fun_rt PRIVATE ${PROJECT_SOURCE_DIR}/include)

