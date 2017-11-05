module Util (
     GeneralError (..)
   , guardedFileOp
   , throwServantErr
   , unique
    ) where

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.UTF8 as BS

import Control.Monad.Trans.Except
import Control.Monad.Trans.Either
import System.Directory
import System.IO

import Servant (ServantErr (..), Handler, throwError, err500)

import Control.Monad.Trans

type GeneralError = String

guardedFileOp :: (FilePath -> IO b) -> FilePath -> EitherT String IO b
guardedFileOp op fp = do
    b <- liftIO $ doesFileExist fp
    if b
        then liftIO $ op fp
        else left $ "file not found: " ++ fp

throwServantErr :: ExceptT GeneralError IO a -> Handler a
throwServantErr x = do
  i <- liftIO $ runExceptT x
  case i of
    (Left e) -> throwError (err500 { errBody = BS.fromString e })
    (Right v) -> return v

convertError :: GeneralError -> ServantErr
convertError e = ServantErr 500 e BS.empty []

unique :: Eq a => [a] -> [a]
unique = reverse . foldl (\acc x -> if x `elem` acc then acc else x:acc) []
