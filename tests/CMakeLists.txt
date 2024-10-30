
function(cgui_add_test FILE)
    string(REPLACE ".cpp" "" TEST "${FILE}")
    add_executable(${TEST} ${FILE})

    target_link_libraries(${TEST}
            PRIVATE
            GTest::gmock_main
            cgui::cgui
            dooc::named_parameters
    )

    target_include_directories(${TEST} PRIVATE "")

    if (WIN32 AND ${CMAKE_CXX_COMPILER_ID} STREQUAL MSVC)
        target_compile_options(${TEST} PRIVATE /bigobj)
    endif ()

    gtest_discover_tests(${TEST} DISCOVERY_MODE PRE_TEST)
endfunction()

cgui_add_test(cgui-bp-tests.cpp)
cgui_add_test(cgui-geometry-tests.cpp)
cgui_add_test(cgui-subable-tests.cpp)
cgui_add_test(cgui-tests.cpp)
cgui_add_test(cgui-textrender-tests.cpp)
cgui_add_test(cgui-widget-tests.cpp)
