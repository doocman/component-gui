
add_library(asp_macro_inc INTERFACE)
target_include_directories(asp_macro_inc INTERFACE macro_inc)

asp_add_library(asp_headers INTERFACE)

target_include_directories(asp_headers INTERFACE inc)

target_link_libraries(asp_headers INTERFACE
        mp-units::mp-units
        asp_macro_inc
)
if (ASP_MP_UNITS_MODULES)
    target_compile_definitions(asp_headers INTERFACE
            ASP_MP_UNITS_MODULES=1
    )
endif ()
if (ASP_CXX_MODULE)
    asp_add_library(asp_modules)
    target_compile_definitions(asp_modules PUBLIC ASP_CXX_MODULE=1)
    target_sources(asp_modules PUBLIC FILE_SET CXX_MODULES FILES src/asp.cpp)
    target_link_libraries(asp_modules PRIVATE asp::asp_headers PUBLIC asp_macro_inc)
    add_library(asp::asp ALIAS asp_modules)
else ()
    add_library(asp::asp ALIAS asp_headers)
    #target_sources(asp PRIVATE src/asp.cpp)
endif ()
