{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api
    ( API
    , api
    , server
    ) where

import Servant
import Database.Core (DbPool)
import Api.Types
import Api.Handlers.Semester (semesterHandlers)
import Api.Handlers.Module (moduleHandlers)
import Api.Handlers.Exam (examHandlers)

api :: Proxy API
api = Proxy

server :: DbPool -> Server API
server pool = 
         semesterHandlers pool
    :<|> moduleHandlers pool
    :<|> examHandlers pool