module Handler.Api where

import           Data.Serviette
import           Import

-- | Handlers

getApiR :: Handler Value
getApiR = do
  return $ String "Serviette - SQL JSON API"

postApiR :: Handler Value
postApiR = do
  sql <- requireJsonBody :: Handler Data.Serviette.SqlQuery
  -- let sqlR = SqlResultQuery (getActionArg sql) (getSelectTableArg sql) (getJoinTableArg sql) (getWhereConditionArg sql)
  return $ String $ Data.Serviette.rawSqlStr sql
