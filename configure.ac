AC_INIT([Haskell directory package], [1.0], [libraries@haskell.org], [directory])

# Safety check: Ensure that we are in the correct source directory.
AC_CONFIG_SRCDIR([include/HsDirectory.h])

AC_CONFIG_HEADERS([include/HsDirectoryConfig.h])

AC_ARG_WITH([cc],
            [C compiler],
            [CC=$withval])
# inherit the C compiler from Cabal
AC_ARG_WITH([gcc],
            [C compiler],
            [CC=$withval])
# avoid warnings when run via Cabal
AC_ARG_WITH([compiler],
            [GHC compiler],
            [])
AC_PROG_CC()

# check for specific header (.h) files that we are interested in
AC_CHECK_HEADERS([fcntl.h limits.h sys/types.h sys/stat.h time.h])

AC_CHECK_FUNCS([utimensat])

# EXTEXT is defined automatically by AC_PROG_CC;
# we just need to capture it in the header file
AC_DEFINE_UNQUOTED([EXE_EXTENSION], ["$EXEEXT"],
                   [Filename extension of executable files])

AC_OUTPUT
