
function(cgui_add_example NAME)
    add_executable(${NAME} ${NAME}.cpp)
    target_link_libraries(${NAME} PRIVATE
            cmpr::add_warnings
            cgui::cgui
            SDL2::SDL2main
    )
endfunction()

cgui_add_example(hello-world)
