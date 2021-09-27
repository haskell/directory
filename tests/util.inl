#define T(f) (T.f _t __FILE__ __LINE__)

import Prelude ()
import System.Directory.Internal.Prelude
import System.Directory
import Util (TestEnv)
import qualified Util as T
-- This comment prevents "T" above from being treated as the function-like
-- macro defined earlier.
