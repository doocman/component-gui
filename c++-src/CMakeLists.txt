
asp_add_library(asp)

target_include_directories(asp PUBLIC inc)

target_link_libraries(asp PUBLIC
        mp-units::mp-units
)
if (ASP_MP_UNITS_MODULES)
    target_compile_definitions(asp PUBLIC
            ASP_MP_UNITS_MODULES=1
    )
endif ()
if (ASP_CXX_MODULE)
    target_compile_definitions(asp PRIVATE ASP_CXX_MODULE=1)
    target_sources(asp PUBLIC FILE_SET CXX_MODULES FILES src/asp.cpp)
else ()
    target_sources(asp PRIVATE src/asp.cpp)
endif ()
