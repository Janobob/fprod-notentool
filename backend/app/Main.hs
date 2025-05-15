module Main where

import Network.Wai.Handler.Warp (run)
import Servant
import Api

main :: IO ()
main = do
    run 8080 $ serve api server
