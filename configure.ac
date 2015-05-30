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

# EXTEXT is defined automatically by AC_PROG_CC;
# we just need to capture it in the header file
AC_DEFINE_UNQUOTED([EXE_EXTENSION], ["$EXEEXT"],
                   [Filename extension of executable files])

AC_OUTPUT
