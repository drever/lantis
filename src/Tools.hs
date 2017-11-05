module Tools where

import Util
import IO
import Data.Either
import Control.Monad.Trans.Except
import Model
import Data.Monoid

import System.Directory (listDirectory)

import Data.Yaml
import qualified Data.Text as T

import Control.Monad
import Control.Monad.IO.Class (liftIO)

convertProjects :: ExceptT GeneralError IO ()
convertProjects = do
    x <- liftIO (listDirectory "data/project/")
    ps <- mapM (readProject "data/project" . read) x
    liftIO $ mapM_ (\p -> encodeFile  ("data/project/" ++ show (projectId p) ++ ".yaml") p) ps

convertIssues :: ExceptT GeneralError IO ()
convertIssues = do
    x <- liftIO (listDirectory "data/issue/")
    ps <- mapM (readIssue "data/issue" . read) x
    liftIO $ mapM_ (\p -> encodeFile  ("data/issue/" ++ show (issueId p) ++ ".yaml") p) ps

