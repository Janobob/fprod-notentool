module Main where

import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (simpleCors)
import Servant
import Api
import Database (initializeDatabase)

main :: IO ()
main = do
    putStrLn "Initializing database..."
    pool <- initializeDatabase
    putStrLn "Starting server on port 8080..."
    run 8080 $ simpleCors $ serve api (server pool)
