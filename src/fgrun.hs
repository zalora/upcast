import System.Environment (getArgs)

import Data.List
import Upcast.Shell

main = do
  (command:args) <- getArgs
  _ <- fgrunProxy (exec command args)
  return ()
