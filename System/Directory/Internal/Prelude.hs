{-# OPTIONS_HADDOCK hide #-}
-- |
-- Stability: unstable
-- Portability: portable
--
-- Internal modules are always subject to change from version to version.

module System.Directory.Internal.Prelude
  ( module Prelude
  , module Control.Arrow
  , module Control.Concurrent
  , module Control.Exception
  , module Control.Monad
  , module Data.Bits
  , module Data.Char
  , module Data.Foldable
  , module Data.Function
  , module Data.Maybe
  , module Data.Monoid
  , module Data.IORef
  , module Data.Traversable
  , module Foreign
  , module Foreign.C
  , module GHC.IO.Encoding
  , module GHC.IO.Exception
  , module System.Environment
  , module System.Exit
  , module System.IO
  , module System.IO.Error
  , module System.Posix.Types
  , module System.Timeout
  , Void
  ) where
import Data.Void (Void)
import Control.Arrow (second)
import Control.Concurrent
  ( forkIO
  , killThread
  , newEmptyMVar
  , putMVar
  , readMVar
  , takeMVar
  , forkFinally
  )
import Control.Exception
  ( Exception(displayException)
  , SomeException
  , bracket
  , bracket_
  , bracketOnError
  , catch
  , finally
  , mask
  , onException
  , throwIO
  , try
  )
import Control.Monad ((>=>), (<=<), unless, when, replicateM, replicateM_)
import Data.Bits ((.&.), (.|.), complement)
import Data.Char (isAlpha, isAscii, toLower, toUpper)
import Data.Foldable (for_)
import Data.Function (on)
import Data.Maybe (catMaybes, fromMaybe, maybeToList)
import Data.Monoid ((<>), mconcat, mempty)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Traversable (for)
import Foreign
  ( Ptr
  , Storable
    ( alignment
    , peek
    , peekByteOff
    , peekElemOff
    , poke
    , pokeByteOff
    , pokeElemOff
    , sizeOf
    )
  , alloca
  , allocaArray
  , allocaBytes
  , allocaBytesAligned
  , maybeWith
  , nullPtr
  , plusPtr
  , with
  , withArray
  )
import Foreign.C
  ( CInt(..)
  , CLong(..)
  , CString
  , CTime(..)
  , CUChar(..)
  , CULong(..)
  , CUShort(..)
  , CWString
  , CWchar(..)
  , throwErrnoIfMinus1Retry_
  , throwErrnoIfMinus1_
  , throwErrnoIfNull
  )
import GHC.IO.Exception
  ( IOErrorType
    ( InappropriateType
    , InvalidArgument
    , OtherError
    , UnsupportedOperation
    )
  )
import GHC.IO.Encoding (getFileSystemEncoding)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO
  ( Handle
  , IOMode(ReadMode, WriteMode)
  , hClose
  , hFlush
  , hGetBuf
  , hPutBuf
  , hPutStr
  , hPutStrLn
  , openBinaryTempFile
  , stderr
  , stdout
  )
import System.IO.Error
  ( IOError
  , catchIOError
  , doesNotExistErrorType
  , illegalOperationErrorType
  , ioeGetErrorString
  , ioeGetErrorType
  , ioeGetLocation
  , ioeSetErrorString
  , ioeSetFileName
  , ioeSetLocation
  , isAlreadyExistsError
  , isDoesNotExistError
  , isIllegalOperation
  , isPermissionError
  , mkIOError
  , modifyIOError
  , permissionErrorType
  , tryIOError
  , userError
  )
import System.Posix.Types (EpochTime)
import System.Timeout (timeout)
