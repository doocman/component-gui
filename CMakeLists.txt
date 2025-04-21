
cmake_minimum_required(VERSION 3.28)

project(aspect-gui LANGUAGES CXX)

cmake_policy(SET CMP0077 NEW)

set(ASP_SDL_FETCH fetch CACHE STRING
        "List of options to use for linking sdl. Options: fetch, system"
)
set(_ASP_SDL_STATIC_DEFAULT OFF)

include(cmake/embeddBinary.cmake)

if (EXISTS "${CMAKE_CURRENT_LIST_DIR}/local-cmake")
    add_subdirectory(local-cmake)
endif ()

function(ASP_DOWNLOAD_GOOGLEFONTS)
    include("${CMAKE_SOURCE_DIR}/cmake/findGoogleFonts.cmake")
endfunction()

if (PROJECT_IS_TOP_LEVEL)
    set(_ASP_SDL_STATIC_DEFAULT ON)
    set(_ASP_DEFAULT_EXAMPLES ON)
    set(CMAKE_CXX_STANDARD 23)
    set(CMAKE_CXX_STANDARD_REQUIRED ON)
    set(_ASP_DEVELOP_DEFAULT ON)
    add_subdirectory(cmake-presets)
    function(asp_add_library NAME)
        add_library(${NAME} "${ARGN}")
        add_library(asp::${NAME} ALIAS ${NAME})
        set(INC_VIS PRIVATE)
        if (${ARGC} EQUAL 2)
            if (${ARGN} STREQUAL INTERFACE)
                set(INC_VIS INTERFACE)
            endif ()
        endif ()
        target_link_libraries(${NAME} ${INC_VIS} cmpr::add_warnings)
    endfunction()
else ()
    set(_ASP_DEFAULT_EXAMPLES OFF)
    set(_ASP_DEVELOP_DEFAULT OFF)
    function(asp_add_library NAME)
        add_library("${ARGV}")
        add_library(asp::${NAME} ALIAS ${NAME})
    endfunction()
endif ()

if (WIN32)
    set(_ASP_SDL_STATIC_DEFAULT ON)
endif ()

set(ASP_SDL_STATIC
        OFF
        #${_ASP_SDL_STATIC_DEFAULT}
        CACHE BOOL "")
set(ASP_EXAMPLES ${_ASP_DEFAULT_EXAMPLES} CACHE BOOL "")
set(ASP_DEVELOP ${_ASP_DEVELOP_DEFAULT} CACHE BOOL "")

include(FetchContent)

include(cmake/findSDL.cmake)
include(cmake/findDoocNP.cmake)
include(cmake/findFreeType.cmake)
include(cmake/findFmt.cmake)
include(cmake/findMpUnits.cmake)

add_subdirectory(c++-src)

if (ASP_EXAMPLES)
    add_subdirectory(examples/c++)
endif ()
if (ASP_DEVELOP)
    enable_testing()
    include(cmake/findGTest.cmake)
    if (${CMAKE_CROSSCOMPILING})
        set(_ASP_DEFAULT_TEST_DISCOVERY PRE_TEST)
    else ()
        set(_ASP_DEFAULT_TEST_DISCOVERY POST_BUILD)
    endif ()
    set(ASP_TEST_DISCOVERY ${_ASP_DEFAULT_TEST_DISCOVERY} CACHE STRING "Strategy to use for gtest. Cross-compiling tests needs this to be set to PRE_TEST, but test discovery may interfere with CLions test list.")
    add_subdirectory(tests)
endif ()

