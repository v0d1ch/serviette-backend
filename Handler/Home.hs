module Handler.Home where

import qualified Data.Aeson as A
import           Import

getHomeR :: Handler Value
getHomeR = do
  return $ A.String "serviette"

