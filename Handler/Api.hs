{-# Language OverloadedStrings #-}
module Handler.Api where

import           Data.Serviette
import           Import
import           Data.Aeson



-- | Handlers

getApiR :: Handler Value
getApiR = do
  return $ String "Serviette - SQL JSON API"

postApiR :: Handler Value
postApiR = do
  sql <- requireJsonBody :: Handler Data.Serviette.SqlQuery
  liftIO $ print $  Data.Serviette.rawSqlStr sql
  case eitherDecode $  Data.Serviette.rawSqlStr sql of
    Right x -> return $ String x
    Left x -> return $ String $ pack x
