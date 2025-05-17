
fetchcontent_declare(
        mp-units
        GIT_REPOSITORY https://github.com/mpusz/mp-units.git
        GIT_TAG 6e06eddf205deaf6c2f2f63500c8c70ec220a99f # v2.4.0
        SOURCE_SUBDIR src
        OVERRIDE_FIND_PACKAGE
)

if (${PROJECT_IS_TOP_LEVEL})
    set(MP_UNITS_API_CONTRACTS "NONE" CACHE STRING "" FORCE)
    set(MP_UNITS_BUILD_CXX_MODULES OFF)
endif ()

find_package(mp-units)
#fetchcontent_makeavailable(mp-units)
