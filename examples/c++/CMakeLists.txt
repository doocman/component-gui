
function(cgui_add_example NAME)
    add_executable(${NAME} ${NAME}.cpp)
    target_link_libraries(${NAME} PRIVATE
            cmpr::add_warnings
            cgui::cgui
            SDL2::SDL2main
            SDL2::SDL2
    )
endfunction()

cgui_add_example(hello-world)
