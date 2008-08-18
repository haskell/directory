import System.Directory

tmp1 = "renameFile001.tmp1"
tmp2 = "renameFile001.tmp2"

main = do
  writeFile tmp1 "test"
  renameFile tmp1 tmp2
  readFile tmp2 >>= print
  writeFile tmp1 "test2"
  renameFile tmp2 tmp1  
  readFile tmp1 >>= print
  
