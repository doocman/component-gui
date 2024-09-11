
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
endfunction()

cgui_add_example(hello-world)
target_link_libraries(hello-world PRIVATE cgui_embedded::cgui_example_font)
add_custom_command(TARGET hello-world POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy $<TARGET_FILE:SDL2> $<TARGET_FILE_DIR:hello-world>
        COMMENT "Copied SDL2 shared library file to example output"
)
