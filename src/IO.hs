{-# LANGUAGE OverloadedStrings #-}

module IO (
    nextId
  , createIssue
  , deleteIssue
  , projectIdForIssue
  , readIssue
  , setIssueStatus
  , readProject
  , removeIssueIdFromProject
    ) where

import Control.Monad.Trans
import Control.Monad

import System.IO
import System.Directory (removeFile, doesFileExist)

import Model (User (..), Issue (..), Project (..), Status (..), ProjectId, IssueId, UserId, emptyIssue, addIssue, removeIssue)
import Util (listDirectory, guardedFileOp, GeneralError)

import qualified Data.Text as T
import Text.ParserCombinators.Parsec (ParseError)

import Control.Monad.Trans.Either

import Data.Yaml

import Data.List.Split

nextId :: FilePath -> IO Int
nextId fp = do
    c <- map (read . head . splitOn ".") `fmap` listDirectory fp
    if null c 
        then return 1 
        else return $ maximum c + 1

createIssue :: FilePath -> FilePath -> ProjectId -> EitherT String IO Issue
createIssue ip pp pi = do
    project <- readProject pp pi
    i <- liftIO $ nextId ip
    let newIssue = emptyIssue i (projectId project)
    liftIO $ putStrLn $ "Create new issue " ++ show i ++ " for project " ++ show pi
    liftIO $ encodeFile (ip ++ "/" ++ show i ++ ".yaml") newIssue
    liftIO $ encodeFile (pp ++ "/" ++ show pi ++ ".yaml") (addIssue newIssue project)
    return newIssue

deleteIssue :: FilePath -> FilePath -> IssueId -> EitherT String IO IssueId
deleteIssue ip pp i = do
    liftIO $ putStrLn $ "Delete issue " ++ show i
    liftIO $ removeFile ip'
    pid <- projectIdForIssue pp i
    removeIssueIdFromProject pp i pid
    right i
    where ip' = ip ++ show i ++ ".yaml" 

removeIssueIdFromProject :: FilePath -> IssueId -> ProjectId -> EitherT GeneralError IO ()
removeIssueIdFromProject fp iid pid = do
    liftIO $ putStrLn $ "Remove issue " ++ show iid ++ " from project " ++ show pid
    mp <- (liftIO $ decodeFile (pp' pid))
    maybe (left $ "Project " ++ show pid ++ " not found")
          (\p -> liftIO $ encodeFile (pp' pid) (removeIssue iid p))
          mp 
    where pp' pid = fp ++ "/" ++ show pid ++ ".yaml"

projectIdForIssue :: FilePath -> IssueId -> EitherT GeneralError IO ProjectId
projectIdForIssue fp i = do
    ps <- liftIO $ listDirectory fp
    projectId `fmap` projectIdForIssue' fp i ps

projectIdForIssue' :: FilePath -> IssueId -> [FilePath] -> EitherT GeneralError IO Project
projectIdForIssue' fp i [] = left $ "No project containting issue " ++ show i ++ " found"
projectIdForIssue' fp i (x:xs) = 
        maybe 
            (projectIdForIssue' fp i xs)  
            (\x -> do p <- readProject fp x
                      if i `elem` projectIssues p
                         then return p
                         else projectIdForIssue' fp i xs)
            (idForFilePath x)

idForFilePath :: FilePath -> Maybe Int
idForFilePath fp = do
    let sp = splitOn "." fp
    if (length sp > 1) && ((sp !! 1) == "yaml") && (isNumeric $ head sp)
        then Just $ (read . head) sp
        else Nothing
    where isNumeric s = (length $ filter (`notElem` ['0'.. '9']) s) == 0
    
    
readIssue :: FilePath -> IssueId -> EitherT GeneralError IO Issue
readIssue fp ii = do
    liftIO . putStrLn $ "Read issue " ++ show ii
    i <- liftIO (decodeFileEither (fp ++ "/" ++ show ii ++ ".yaml"))
    bimapEitherT show id (hoistEither i)

setIssueStatus :: FilePath -> IssueId -> Status -> EitherT GeneralError IO Issue
setIssueStatus fp i s = do
    liftIO . putStrLn $ "Change status of issue " ++ show i ++ " to " ++ show s
    mi <- liftIO $ decodeFile (yamlFile fp i)
    maybe 
        (left $ "Could not update issue status. Issue not found: " ++ show i)
        (\ri -> do
            let ii = ri { issueStatus = s }
            liftIO $ encodeFile (yamlFile fp i) ii
            right ii)
        mi

yamlFile :: FilePath -> Int -> FilePath
yamlFile fp i = fp ++ "/" ++ show i ++ ".yaml"

readProject :: FilePath -> ProjectId -> EitherT GeneralError IO Project
readProject fp pi = do
    liftIO . putStrLn $ "Read project " ++ show pi
    p <- liftIO (decodeFileEither (fp ++ "/" ++ show pi ++ ".yaml"))
    bimapEitherT show id (hoistEither p)
