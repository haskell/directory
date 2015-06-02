#include <HsDirectoryConfig.h>

#ifdef HAVE_SYS_STAT_H
# include <sys/stat.h>
#endif

module System.Directory.Internal.Windows where
#ifdef mingw32_HOST_OS
import System.Posix.Types

s_IRUSR :: CMode
s_IRUSR = (#const S_IRUSR)

s_IWUSR :: CMode
s_IWUSR = (#const S_IWUSR)

s_IXUSR :: CMode
s_IXUSR = (#const S_IXUSR)

s_IFDIR :: CMode
s_IFDIR = (#const S_IFDIR)

#endif
