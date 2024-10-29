
message(STATUS "CGUI add examples")

if (CGUI_SDL_STATIC)
    set(CGUI_SDL_LINK SDL2::SDL2-static)
else ()
    set(CGUI_SDL_LINK SDL2::SDL2)
endif ()

message(STATUS "CGUI: download google fonts for examples")
#CGUI_DOWNLOAD_GOOGLEFONTS()

file(DOWNLOAD "https://github.com/google/fonts/raw/main/apache/robotoslab/RobotoSlab%5Bwght%5D.ttf" "${CMAKE_CURRENT_BINARY_DIR}/font.ttf")
cgui_embedd_binary("${CMAKE_CURRENT_BINARY_DIR}/font.ttf" cgui_example_font)

function(cgui_add_example NAME)
    add_executable(${NAME} ${NAME}.cpp)
    target_link_libraries(${NAME} PRIVATE
            cmpr::add_warnings
            cgui::cgui
            ${CGUI_SDL_LINK}
            SDL2::SDL2main
    )
    target_include_directories(${NAME} PRIVATE "")
endfunction()

if (NOT CGUI_SDL_STATIC)
    add_custom_target(cgui_examples_copy_sdl
            COMMAND
            "${CMAKE_COMMAND}" -E copy
            "$<TARGET_FILE:SDL2>" "$<TARGET_FILE_DIR:hello-world>"
            COMMENT "Copied SDL2 shared library file to example output"
    )
endif ()

function(cgui_add_sdl_example NAME)
    cgui_add_example(${NAME})
    if (NOT CGUI_SDL_STATIC)
        add_dependencies(${NAME} cgui_examples_copy_sdl)
    endif ()
endfunction()

cgui_add_sdl_example(hello-world)
cgui_add_sdl_example(dynamic_list)

target_link_libraries(hello-world PRIVATE cgui_embedded::cgui_example_font)
target_link_libraries(dynamic_list PRIVATE cgui_embedded::cgui_example_font fmt::fmt)
