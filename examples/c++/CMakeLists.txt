
if (CGUI_SDL_STATIC)
    set (CGUI_SDL_LINK SDL2::SDL2-static)
else ()
    set (CGUI_SDL_LINK SDL2::SDL2)
endif ()

function(cgui_add_example NAME)
    add_executable(${NAME} ${NAME}.cpp)
    target_link_libraries(${NAME} PRIVATE
            cmpr::add_warnings
            cgui::cgui
            SDL2::SDL2main
            ${CGUI_SDL_LINK}
    )
    if (${CMAKE_CXX_COMPILER_ID} STREQUAL MSVC)
    target_compile_options(${NAME} PRIVATE
            /fsanitize=address
    )
    else ()
        message(WARNING "ASDA")
    endif ()
endfunction()

cgui_add_example(hello-world)
