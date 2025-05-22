{-# LANGUAGE OverloadedStrings #-}

import Network.Wai (Middleware)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors
    ( cors
    , CorsResourcePolicy(..)
    , simpleCorsResourcePolicy
    )
import Servant
import Api
import Database (initializeDatabase)

main :: IO ()
main = do
    putStrLn "Initializing database..."
    pool <- initializeDatabase
    putStrLn "Starting server on port 8080..."
    run 8080 $ cors (const $ Just corsPolicy) $ serve api (server pool)

corsPolicy :: CorsResourcePolicy
corsPolicy =
    simpleCorsResourcePolicy
        { corsOrigins = Just (["http://localhost:8000"], True)
        , corsRequestHeaders = ["Content-Type"]
        , corsMethods = ["GET", "POST", "PUT", "DELETE", "OPTIONS"]
        }