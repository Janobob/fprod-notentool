{-# LANGUAGE OverloadedStrings #-}

module Api.Handlers.Module
    ( moduleHandlers
    ) where

import Servant
import Control.Monad.IO.Class (liftIO)
import Models
import Database.Core (DbPool)
import Database.Module
import Api.Types (ModuleAPI)

moduleHandlers :: DbPool -> Server ModuleAPI
moduleHandlers pool = 
         getModulesHandler
    :<|> getModuleHandler
    :<|> getExamsForModuleHandler
    :<|> createModuleHandler
    :<|> updateModuleHandler
    :<|> deleteModuleHandler
  where
    getModulesHandler :: Handler [Module]
    getModulesHandler = liftIO $ getModules pool

    getModuleHandler :: Int -> Handler Module
    getModuleHandler mid = do
        maybeModule <- liftIO $ getModuleById pool mid
        case maybeModule of
            Just module' -> return module'
            Nothing -> throwError err404

    getExamsForModuleHandler :: Int -> Handler [Exam]
    getExamsForModuleHandler mid = liftIO $ getExamsForModule pool mid

    createModuleHandler :: Module -> Handler Module
    createModuleHandler module' = liftIO $ createModule pool module'

    updateModuleHandler :: Int -> Module -> Handler Module
    updateModuleHandler mid module' = liftIO $ updateModule pool mid module'

    deleteModuleHandler :: Int -> Handler NoContent
    deleteModuleHandler mid = do
        liftIO $ deleteModule pool mid
        return NoContent