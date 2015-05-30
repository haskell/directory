module System.Directory.Internal where

#ifndef mingw32_HOST_OS
# include <HsUnixConfig.h>
#endif

#ifdef HAVE_UTIMENSAT
# include <fcntl.h>
# include <sys/stat.h>
import Data.Time.Clock.POSIX (POSIXTime)
import Foreign
import Foreign.C
import System.Posix.Types
#endif

#ifdef HAVE_UTIMENSAT

data CTimeSpec = CTimeSpec EpochTime CLong

instance Storable CTimeSpec where
    sizeOf    _ = #size struct timespec
    alignment _ = alignment (undefined :: CInt)
    poke p (CTimeSpec sec nsec) = do
      (#poke struct timespec, tv_sec ) p sec
      (#poke struct timespec, tv_nsec) p nsec
    peek p = do
      sec  <- #{peek struct timespec, tv_sec } p
      nsec <- #{peek struct timespec, tv_nsec} p
      return (CTimeSpec sec nsec)

c_AT_FDCWD :: Integral a => a
c_AT_FDCWD = (#const AT_FDCWD)

utimeOmit :: CTimeSpec
utimeOmit = CTimeSpec (CTime 0) (#const UTIME_OMIT)

toCTimeSpec :: POSIXTime -> CTimeSpec
toCTimeSpec t = CTimeSpec (CTime sec) (truncate $ 10 ^ (9 :: Int) * frac)
  where
    (sec,  frac)  = if frac' < 0 then (sec' - 1, frac' + 1) else (sec', frac')
    (sec', frac') = properFraction (toRational t)

foreign import ccall unsafe "utimensat" c_utimensat
  :: CInt -> CString -> Ptr CTimeSpec -> CInt -> IO CInt

#endif // HAVE_UTIMENSAT
