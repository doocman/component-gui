
cmake_minimum_required(VERSION 3.25)

project(component-gui LANGUAGES CXX)

cmake_policy(SET CMP0077 NEW)

set(CGUI_SDL_FETCH fetch CACHE STRING
        "List of options to use for linking sdl. Options: fetch, system"
)
set(_CGUI_SDL_STATIC_DEFAULT OFF)

include(FetchContent)

include(cmake/findSDL.cmake)
include(cmake/findDoocNP.cmake)
include(cmake/findFreeType.cmake)

if (PROJECT_IS_TOP_LEVEL)
    set(_CGUI_SDL_STATIC_DEFAULT ON)
    set(_CGUI_DEFAULT_EXAMPLES ON)
    set(CMAKE_CXX_STANDARD 20)
    set(CMAKE_CXX_STANDARD_REQUIRED ON)
    set(_CGUI_DEVELOP_DEFAULT ON)
    add_subdirectory(cmake-presets)
    function(cgui_add_library NAME)
        add_library(${NAME} "${ARGN}")
        add_library(cgui::${NAME} ALIAS ${NAME})
        set(INC_VIS PRIVATE)
        if (${ARGC} EQUAL 2)
            if (${ARGN} STREQUAL INTERFACE)
                set(INC_VIS INTERFACE)
            endif ()
        endif ()
        target_link_libraries(${NAME} ${INC_VIS} cmpr::add_warnings)
    endfunction()
else ()
    set(_CGUI_DEFAULT_EXAMPLES OFF)
    set(_CGUI_DEVELOP_DEFAULT OFF)
    function(cgui_add_library NAME)
        add_library("${ARGV}")
        add_library(cgui::${NAME} ALIAS ${NAME})
    endfunction()
endif ()

if (WIN32)
    set(_CGUI_SDL_STATIC_DEFAULT ON)
endif ()

set(CGUI_SDL_STATIC ${_CGUI_SDL_STATIC_DEFAULT} CACHE BOOL "")
set(CGUI_EXAMPLES ${_CGUI_DEFAULT_EXAMPLES} CACHE BOOL "")
set(CGUI_DEVELOP ${_CGUI_DEVELOP_DEFAULT} CACHE BOOL "")

add_subdirectory(c++-src)

if (CGUI_EXAMPLES)
    add_subdirectory(examples/c++)
endif ()
if (CGUI_DEVELOP)
    include(cmake/findGTest.cmake)
    add_subdirectory(tests)
endif ()

