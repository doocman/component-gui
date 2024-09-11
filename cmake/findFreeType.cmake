
fetchcontent_declare(
        freetype
        GIT_REPOSITORY https://gitlab.freedesktop.org/freetype/freetype.git
        GIT_TAG 42608f77f20749dd6ddc9e0536788eaad70ea4b5 # 2.13.3
)

set(FT_DISABLE_HARFBUZZ ON)

fetchcontent_makeavailable(freetype)

if (NOT TARGET freetype::freetype)
    add_library(freetype::freetype ALIAS freetype)
endif ()
