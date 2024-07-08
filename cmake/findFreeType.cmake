
fetchcontent_declare(
        freetype
        GIT_REPOSITORY https://gitlab.freedesktop.org/freetype/freetype.git
        GIT_TAG 920c5502cc3ddda88f6c7d85ee834ac611bb11cc # 2.13.2
)

set(FT_DISABLE_HARFBUZZ ON)

fetchcontent_makeavailable(freetype)

if (NOT TARGET freetype::freetype)
    add_library(freetype::freetype ALIAS freetype)
endif ()
