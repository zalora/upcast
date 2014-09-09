module Upcast.Deploy where

import qualified Data.Text as T
import Data.Text (Text(..))

import Upcast.Interpolate (n)

import Upcast.Nix
import Upcast.Command
import Upcast.DeployCommands
import Upcast.Types
import Upcast.Temp

setupAgent privkeys = setupAgentF sshAddKey privkeys

setupAgentF liftKey keyvals = do
    agentSocket <- randomTempFileName "ssh-agent.sock."
    spawn $ sshAgent agentSocket
    mapM_ (fgrun . liftKey agentSocket) $ keyvals
    fgrun $ sshListKeys agentSocket
    return agentSocket

deploymentInfo :: DeployContext -> IO (Either String Value)
deploymentInfo ctx =
    let info = nixDeploymentInfo ctx (T.unpack $ expressionFile ctx) (uuid ctx)
        in do
          i <- fgconsume_ info
          return $ nixValue i

