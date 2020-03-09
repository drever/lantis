module Util (
     GeneralError (..)
   , throwServantErr
   , unique
    ) where

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.UTF8 as BS

import Control.Monad.Trans.Except
import System.Directory
import System.IO

import Servant (Handler, throwError, err500)
import Servant.Server (ServerError (..))

import Control.Monad.Trans

type GeneralError = String

throwServantErr :: ExceptT GeneralError IO a -> Handler a
throwServantErr x = do
  i <- liftIO $ runExceptT x
  case i of
    (Left e) -> throwError $ (err500 { errBody = BS.fromString e })
    (Right v) -> return v

convertError :: GeneralError -> ServerError
convertError e = err500

unique :: Eq a => [a] -> [a]
unique = reverse . foldl (\acc x -> if x `elem` acc then acc else x:acc) []
