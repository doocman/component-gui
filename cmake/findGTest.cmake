
#if(NOT TARGET GMock::GMock)

fetchcontent_declare(
        gtest
        GIT_REPOSITORY https://github.com/google/googletest.git
        GIT_TAG f8d7d77c06936315286eb55f8de22cd23c188571 # v1.14.0
)

fetchcontent_makeavailable(gtest)
include(GoogleTest)
