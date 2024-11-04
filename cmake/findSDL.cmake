

set(_CGUI_SDL_FOUND OFF)

if (CGUI_SDL_STATIC)
    set(_CGUI_SDL_LIB SDL3::SDL3-static)
else ()
    set(_CGUI_SDL_LIB SDL3::SDL3)
endif ()

foreach (E IN LISTS CGUI_SDL_FETCH)
    if(_CGUI_SDL_FOUND)
        break()
    else ()
        if(E STREQUAL "fetch")
            fetchcontent_declare(
                    sdl3
                    GIT_REPOSITORY https://github.com/libsdl-org/SDL.git
                    GIT_TAG 802686699449289109f58b9ec4e6200f061e9dd2 # main (SDL3-prerelease)
            )
            if (CGUI_SDL_STATIC)
                set(SDL_STATIC ON)
                set(SDL_SHARED OFF)
            else ()
                set(SDL_SHARED ON)
                set(SDL_STATIC OFF)
            endif ()

            set(SDL_CMAKE_DEBUG_POSTFIX "" CACHE STRING "" FORCE)

            fetchcontent_makeavailable(sdl3)

            if (TARGET SDL3-shared)
                set_target_properties(SDL3-shared PROPERTIES
                        CMAKE_C_FLAGS_INIT "${CMAKE_C_FLAGS_INIT} -DSDL_DISABLE_VERSIONED_SYMBOLS"
                        CMAKE_CXX_FLAGS_INIT "${CMAKE_CXX_FLAGS_INIT} -DSDL_DISABLE_VERSIONED_SYMBOLS"
                )
            endif ()
            if (TARGET SDL3-static)
                set_target_properties(SDL3-static PROPERTIES
                        CMAKE_C_FLAGS_INIT "${CMAKE_C_FLAGS_INIT} -DSDL_DISABLE_VERSIONED_SYMBOLS"
                        CMAKE_CXX_FLAGS_INIT "${CMAKE_CXX_FLAGS_INIT} -DSDL_DISABLE_VERSIONED_SYMBOLS"
                )
            endif ()

        elseif (E STREQUAL "system")
            find_package(SDL3 REQUIRED)
            if(NOT TARGET ${_CGUI_SDL_LIB})
                if(NOT TARGET SDL3)
                    add_library(SDL3 INTERFACE)
                    target_link_libraries(SDL3 INTERFACE "${SDL3_LIBRARIES}")
                    target_include_directories(SDL3 INTERFACE "${SDL3_INCLUDE_DIRS}")
                endif ()
                if (NOT TARGET SDL3::SDL3)
                    add_library(SDL3::SDL3 ALIAS SDL3)
                endif ()
                set(CGUI_SDL_STATIC SDL3::SDL3)
            endif ()
        else ()
            message(SEND_ERROR "Unrecognized CGUI_SDL_FETCH option '${E}'")
            break()
        endif ()
    endif ()
endforeach ()


