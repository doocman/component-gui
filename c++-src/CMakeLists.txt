
asp_add_library(asp)

target_include_directories(asp PUBLIC inc)
target_sources(asp PRIVATE src/asp.cpp)

target_link_libraries(asp PUBLIC
)
