
set(CGUI_TESTS_USE_PCH ON CACHE BOOL "Set to true to enable precompiled headers for tests")

add_library(_cgui_test_utils cgui_test_utils.cpp)
target_include_directories(_cgui_test_utils PUBLIC "")
target_link_libraries(_cgui_test_utils PUBLIC GTest::gmock cgui::cgui dooc::named_parameters)
if (CGUI_TESTS_USE_PCH)
    target_precompile_headers(_cgui_test_utils PUBLIC cgui_test_utils.hpp)
endif ()

function(cgui_add_test FILE)
    string(REPLACE ".cpp" "" TEST "${FILE}")
    add_executable(${TEST} ${FILE})

    target_link_libraries(${TEST}
            PRIVATE
            GTest::gmock_main
            cgui::cgui
            dooc::named_parameters
    )

    if (WIN32 AND ${CMAKE_CXX_COMPILER_ID} STREQUAL MSVC)
        target_compile_options(${TEST} PRIVATE /bigobj)
    endif ()
    if (CGUI_TESTS_USE_PCH)
        target_precompile_headers(${TEST} REUSE_FROM _cgui_test_utils)
        target_include_directories(${TEST} PRIVATE "")
    else ()
        target_link_libraries(${TEST} PRIVATE _cgui_test_utils)
    endif ()

    gtest_discover_tests(${TEST} DISCOVERY_MODE PRE_TEST)
endfunction()

cgui_add_test(cgui-bp-tests.cpp)
cgui_add_test(cgui-geometry-tests.cpp)
cgui_add_test(cgui-subable-tests.cpp)
cgui_add_test(cgui-tests.cpp)
cgui_add_test(cgui-textrender-tests.cpp)
cgui_add_test(cgui-widget-tests.cpp)
