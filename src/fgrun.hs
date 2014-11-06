import System.Environment (getArgs)

import Data.List
import Upcast.Command

main = do
  args <- getArgs
  let cmd = Cmd Local (intercalate " " args) "fgrun"
  _ <- fgrun cmd
  return ()
