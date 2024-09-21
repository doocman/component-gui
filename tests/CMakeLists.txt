
add_executable(cgui-tests cgui-tests.cpp)

target_link_libraries(cgui-tests
        PRIVATE
        GTest::gmock_main
        cgui::cgui
        dooc::named_parameters
)

if (WIN32 AND ${CMAKE_CXX_COMPILER_ID} STREQUAL MSVC)
    target_compile_options(cgui-tests PRIVATE /bigobj)
endif ()

gtest_discover_tests(cgui-tests DISCOVERY_MODE PRE_TEST)
