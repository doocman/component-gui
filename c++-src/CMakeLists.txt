
asp_add_library(asp)

target_include_directories(asp PUBLIC inc)
target_sources(asp PRIVATE src/asp.cpp)

target_link_libraries(asp PUBLIC
        mp-units::mp-units
)
if (ASP_MP_UNITS_MODULES)
    target_compile_definitions(asp PUBLIC
            ASP_MP_UNITS_MODULES=1
    )
endif ()
