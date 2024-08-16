

set(_CGUI_SDL_FOUND OFF)

if (CGUI_SDL_STATIC)
    set(_CGUI_SDL_LIB SDL2::SDL2-static)
else ()
    set(_CGUI_SDL_LIB SDL2::SDL2)
endif ()

foreach (E IN LISTS CGUI_SDL_FETCH)
    if(_CGUI_SDL_FOUND)
        break()
    else ()
        if(E STREQUAL "fetch")
            fetchcontent_declare(
                    sdl2
                    GIT_REPOSITORY https://github.com/libsdl-org/SDL.git
                    GIT_TAG 92fe3b19c868ad062c323dde2cfc9d8b4bfdd785 # release-2.30.4
            )
            if (CGUI_SDL_STATIC)
                set(SDL_STATIC ON)
                set(SDL_SHARED OFF)
            else ()
                set(SDL_SHARED ON)
                set(SDL_STATIC OFF)
            endif ()

            set(SDL_CMAKE_DEBUG_POSTFIX "" CACHE STRING "" FORCE)

            fetchcontent_makeavailable(sdl2)

        elseif (E STREQUAL "system")
            find_package(SDL2 REQUIRED)
            if(NOT TARGET ${_CGUI_SDL_LIB})
                if(NOT TARGET SDL2)
                    add_library(SDL2 INTERFACE)
                    target_link_libraries(SDL2 INTERFACE "${SDL2_LIBRARIES}")
                    target_include_directories(SDL2 INTERFACE "${SDL2_INCLUDE_DIRS}")
                endif ()
                if (NOT TARGET SDL2::SDL2)
                    add_library(SDL2::SDL2 ALIAS SDL2)
                endif ()
                set(CGUI_SDL_STATIC SDL2::SDL2)
            endif ()
        else ()
            message(SEND_ERROR "Unrecognized CGUI_SDL_FETCH option '${E}'")
            break()
        endif ()
    endif ()
endforeach ()


