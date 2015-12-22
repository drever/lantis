module Util (
     GeneralError (..)
   , listDirectory
   , guardedFileOp
   , throwServantErr 
    ) where

import qualified Data.ByteString.Lazy as BS

import Control.Monad.Trans.Either
import System.Directory
import System.IO

import Servant (ServantErr (..))

import Control.Monad.Trans

type GeneralError = String

listDirectory :: FilePath -> IO [FilePath]
listDirectory fp = fmap (filter (\p -> p /= "." && p /= "..")) (getDirectoryContents  fp)

guardedFileOp :: (FilePath -> IO b) -> FilePath -> EitherT String IO b
guardedFileOp op fp = do
    b <- liftIO $ doesFileExist fp
    if b 
        then liftIO $ op fp
        else left $ "file not found: " ++ fp

throwServantErr :: Functor m => EitherT GeneralError m a -> EitherT ServantErr m a
throwServantErr = bimapEitherT convertError id

convertError :: GeneralError -> ServantErr
convertError e = ServantErr 500 e BS.empty []

