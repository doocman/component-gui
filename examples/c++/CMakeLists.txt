
message(STATUS "ASP add examples")

if (ASP_SDL_STATIC)
    set(ASP_SDL_LINK SDL3::SDL3-static)
else ()
    set(ASP_SDL_LINK SDL3::SDL3-shared)
endif ()

message(STATUS "ASP: download google fonts for examples")
file(DOWNLOAD "https://github.com/google/fonts/raw/main/apache/robotoslab/RobotoSlab%5Bwght%5D.ttf" "${CMAKE_CURRENT_BINARY_DIR}/font.ttf")
if (NOT "${dl_status}" MATCHES " *0\;.*")
    message(FATAL_ERROR "Failed to download font: ${dl_status}")
endif ()
asp_embedd_binary("${CMAKE_CURRENT_BINARY_DIR}/font.ttf" asp_example_font)
target_link_libraries(asp_embedded_asp_example_font INTERFACE
        freetype::freetype
)

function(asp_add_example NAME)
    add_executable(${NAME} ${NAME}.cpp)
    target_link_libraries(${NAME} PRIVATE
            cmpr::add_warnings
            asp::asp
            ${ASP_SDL_LINK}
            #SDL3::SDL3main
    )
    target_include_directories(${NAME} PRIVATE "")
endfunction()

if (NOT ASP_SDL_STATIC)
    add_custom_target(asp_examples_copy_sdl
            COMMAND
            "${CMAKE_COMMAND}" -E copy
            "$<TARGET_FILE:${ASP_SDL_LINK}>" "$<TARGET_FILE_DIR:hello-world>"
            COMMENT "Copied SDL3 shared library file to example output"
    )
endif ()

function(asp_add_sdl_example NAME)
    asp_add_example(${NAME})
    if (NOT ASP_SDL_STATIC)
        add_dependencies(${NAME} asp_examples_copy_sdl)
    endif ()
endfunction()

asp_add_sdl_example(hello-world)
asp_add_sdl_example(dynamic_list)

target_link_libraries(hello-world PRIVATE asp_embedded::asp_example_font)
target_link_libraries(dynamic_list PRIVATE asp_embedded::asp_example_font fmt::fmt)
