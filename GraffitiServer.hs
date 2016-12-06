module GraffitiServer where

import Network.Socket
import Control.Concurrent

port = 73313

main = withSocketsDo $ do
    -- Create the socket for the server
    sock <- socket AF_INET Stream defaultProtocol
    
    -- Bind the socket to the port we want it on
    bind (SockAddr port inADDR_ANY)
    
    -- Output success message
    purStrLn "Socket set up successfully.."
    serverLoop sock

serverLoop sock = do
    (client, _) <- accept sock