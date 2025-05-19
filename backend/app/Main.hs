module Main where

import Network.Wai.Handler.Warp (run)
import Servant
import Api
import Database (initializeDatabase)

main :: IO ()
main = do
    putStrLn "Initializing database..."
    pool <- initializeDatabase
    putStrLn "Starting server on port 8080..."
    run 8080 $ serve api (server pool)