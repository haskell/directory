AC_INIT([Haskell directory package], [1.0], [libraries@haskell.org], [directory])

# Safety check: Ensure that we are in the correct source directory.
AC_CONFIG_SRCDIR([include/HsDirectory.h])

AC_CONFIG_HEADERS([include/HsDirectoryConfig.h])

AC_ARG_WITH([cc],
            [C compiler],
            [CC=$withval])
AC_PROG_CC()

# check for specific header (.h) files that we are interested in
AC_CHECK_HEADERS([sys/types.h unistd.h sys/stat.h])

AC_OUTPUT
