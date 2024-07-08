
add_executable(cgui-tests cgui-tests.cpp)

target_link_libraries(cgui-tests
        PRIVATE
        GTest::gmock_main
        cgui::cgui
)

gtest_discover_tests(cgui-tests DISCOVERY_MODE PRE_TEST)
