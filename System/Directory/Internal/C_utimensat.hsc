{-# LANGUAGE CApiFFI #-}

module System.Directory.Internal.C_utimensat where
#include <HsDirectoryConfig.h>
#ifdef HAVE_UTIMENSAT
#ifdef HAVE_FCNTL_H
# include <fcntl.h>
#endif
#ifdef HAVE_TIME_H
# include <time.h>
#endif
#ifdef HAVE_SYS_STAT_H
# include <sys/stat.h>
#endif
import Prelude ()
import System.Directory.Internal.Prelude
import Data.Time.Clock.POSIX (POSIXTime)
import qualified System.Posix as Posix

data CTimeSpec = CTimeSpec EpochTime CLong

instance Storable CTimeSpec where
    sizeOf    _ = #{size struct timespec}
    alignment _ = #{alignment struct timespec}
    poke p (CTimeSpec sec nsec) = do
      (#poke struct timespec, tv_sec)  p sec
      (#poke struct timespec, tv_nsec) p nsec
    peek p = do
      sec  <- #{peek struct timespec, tv_sec } p
      nsec <- #{peek struct timespec, tv_nsec} p
      return (CTimeSpec sec nsec)

utimeOmit :: CTimeSpec
utimeOmit = CTimeSpec (CTime 0) (#const UTIME_OMIT)

toCTimeSpec :: POSIXTime -> CTimeSpec
toCTimeSpec t = CTimeSpec (CTime sec) (truncate $ 10 ^ (9 :: Int) * frac)
  where
    (sec,  frac)  = if frac' < 0 then (sec' - 1, frac' + 1) else (sec', frac')
    (sec', frac') = properFraction (toRational t)

foreign import capi "sys/stat.h utimensat" c_utimensat
  :: Posix.Fd -> CString -> Ptr CTimeSpec -> CInt -> IO CInt

#endif
