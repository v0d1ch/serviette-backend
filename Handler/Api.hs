module Handler.Api where

import           Import


-- | Handlers

getApiR :: Handler Value
getApiR = do
  return $ String "Serviette - SQL JSON API"

postApiR :: Handler Value
postApiR = do
  return $ String "Serviette - SQL JSON API"

  -- sql <- requireJsonBody :: Handler SqlQuery
  -- let sqlR = SqlResultQuery (getActionArg sql) (getSelectTableArg sql) (getJoinTableArg sql) (getWhereConditionArg sql)
  -- return $ String $ rawSqlStr sqlR
