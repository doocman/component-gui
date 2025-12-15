
fetchcontent_declare(
        mp-units
        GIT_REPOSITORY https://github.com/mpusz/mp-units.git
        GIT_TAG 6e06eddf205deaf6c2f2f63500c8c70ec220a99f # v2.4.0
        SOURCE_SUBDIR src
        OVERRIDE_FIND_PACKAGE
        SYSTEM EXCLUDE_FROM_ALL
)

if (${PROJECT_IS_TOP_LEVEL} OR ASP_CXX_MODULE)
    set(MP_UNITS_API_CONTRACTS "NONE" CACHE STRING "" FORCE)
    set(MP_UNITS_BUILD_CXX_MODULES ON)
endif ()

if (MP_UNITS_BUILD_CXX_MODULES)
    set(ASP_MP_UNITS_MODULES 1)
endif ()

find_package(mp-units)
#fetchcontent_makeavailable(mp-units)
