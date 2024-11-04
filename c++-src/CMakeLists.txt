
cgui_add_library(cgui)

target_include_directories(cgui PUBLIC inc)
target_sources(cgui PRIVATE src/cgui.cpp)

target_link_libraries(cgui PUBLIC
)
