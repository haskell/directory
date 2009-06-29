import System.Directory

main = do
  getHomeDirectory               >>= print
  getAppUserDataDirectory "test" >>= print
  getUserDocumentsDirectory      >>= print
  getTemporaryDirectory          >>= print
  return ()
