
set(ASP_TESTS_USE_PCH OFF CACHE BOOL "Set to true to enable precompiled headers for tests")

add_library(_asp_test_utils asp_test_utils.cpp)
target_include_directories(_asp_test_utils PUBLIC "")
target_link_libraries(_asp_test_utils PUBLIC GTest::gmock asp::asp dooc::named_parameters)
if (ASP_TESTS_USE_PCH)
    target_precompile_headers(_asp_test_utils PUBLIC asp_test_utils.hpp)
endif ()

function(asp_add_test FILE)
    string(REPLACE ".cpp" "" TEST "${FILE}")
    add_executable(${TEST} ${FILE})

    target_link_libraries(${TEST}
            PRIVATE
            GTest::gmock_main
            asp::asp
            dooc::named_parameters
            mp-units::mp-units
    )

    if (WIN32 AND ${CMAKE_CXX_COMPILER_ID} STREQUAL MSVC)
        target_compile_options(${TEST} PRIVATE /bigobj)
    endif ()
    if (ASP_TESTS_USE_PCH)
        target_precompile_headers(${TEST} REUSE_FROM _asp_test_utils)
        target_include_directories(${TEST} PRIVATE "")
    else ()
        target_link_libraries(${TEST} PRIVATE _asp_test_utils)
    endif ()

    gtest_discover_tests(${TEST} DISCOVERY_MODE ${ASP_TEST_DISCOVERY})
endfunction()

asp_add_test(asp-bp-tests.cpp)
asp_add_test(asp-events-tests.cpp)
asp_add_test(asp-geometry-tests.cpp)
asp_add_test(asp-subable-tests.cpp)
asp_add_test(asp-tests.cpp)
asp_add_test(asp-textrender-tests.cpp)
asp_add_test(asp-widget-tests.cpp)
asp_add_test(asp-layouts-tests.cpp)
asp_add_test(asp-dynamic-list-tests.cpp)
asp_add_test(asp-basic-widgets-tests.cpp)
