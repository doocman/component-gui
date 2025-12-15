
fetchcontent_declare(
        fmt
        GIT_REPOSITORY https://github.com/fmtlib/fmt.git
        GIT_TAG 407c905e45ad75fc29bf0f9bb7c5c2fd3475976f # 12.1.0
        #OVERRIDE_FIND_PACKAGE
        SYSTEM EXCLUDE_FROM_ALL
)

fetchcontent_makeavailable(fmt)

