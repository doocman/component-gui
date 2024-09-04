
#if(NOT TARGET GMock::GMock)

fetchcontent_declare(
        gtest
        GIT_REPOSITORY https://github.com/google/googletest.git
        GIT_TAG b514bdc898e2951020cbdca1304b75f5950d1f59 # v1.15.2
)

fetchcontent_makeavailable(gtest)
include(GoogleTest)
