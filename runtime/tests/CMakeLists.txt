# GoogleTest requires at least C++14
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

include(FetchContent)

FetchContent_Declare(
  googletest
  URL https://github.com/google/googletest/archive/refs/tags/v1.14.0.zip
)

set(gtest_force_shared_crt ON CACHE BOOL "" FORCE)
FetchContent_MakeAvailable(googletest)


file(GLOB TEST_SOURCES *.cpp)

add_executable(
  fun_rt_test
  ${TEST_SOURCES}
)

target_link_libraries(
  fun_rt_test
  GTest::gtest_main
  fun_rt
)

include(GoogleTest)

target_include_directories(fun_rt_test PRIVATE ${PROJECT_SOURCE_DIR}/include)

gtest_discover_tests(fun_rt_test)


