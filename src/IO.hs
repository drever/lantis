{-# LANGUAGE OverloadedStrings #-}

module IO (
    createIssue
  , writeIssue
  , deleteIssue
  , projectIdForIssue
  , readIssue
  , setIssueStatus
  , setIssueCategory
  , setIssueDescription
  , setIssueSummary
  , readProject
  , removeIssueIdFromProject
    ) where

import Control.Monad.Trans.Except
import Control.Monad
import Control.Monad.IO.Class

import System.IO
import System.Directory (listDirectory, removeFile, doesFileExist)

import Model (User (..), Issue (..), Project (..), Status (..), Category (..), ProjectId, IssueId, UserId, emptyIssue, addIssue, removeIssue)
import Util (guardedFileOp, GeneralError)

import qualified Data.Text as T
import Text.ParserCombinators.Parsec (ParseError)

import Control.Monad.Trans.Either

import Data.Yaml

import Data.List.Split
import Data.Time
import Data.Maybe

nextId :: FilePath -> IO Int
nextId fp = do
    c <- mapMaybe idForFilePath `fmap` listDirectory fp
    if null c
        then return 1
        else return $ maximum c + 1

createIssue :: FilePath -> FilePath -> ProjectId -> ExceptT String IO Issue
createIssue ip pp pi = do
    project <- readProject pp pi
    i <- liftIO $ nextId ip
    t <- liftIO $ getCurrentTime
    let newIssue = emptyIssue i (projectId project) t
    liftIO $ putStrLn $ "Create new issue " ++ show i ++ " for project " ++ show pi
    liftIO $ encodeFile (yamlFile ip i) newIssue
    liftIO $ encodeFile (yamlFile pp pi) (addIssue newIssue project)
    return newIssue

writeIssue :: FilePath -> Issue -> ExceptT String IO ()
writeIssue fp i = liftIO $ encodeFile fp i

deleteIssue :: FilePath -> FilePath -> IssueId -> ExceptT String IO IssueId
deleteIssue ip pp i = do
    liftIO $ putStrLn $ "Delete issue " ++ show i
    liftIO . removeFile $ yamlFile ip i
    pid <- projectIdForIssue pp i
    removeIssueIdFromProject pp i pid
    return i

removeIssueIdFromProject :: FilePath -> IssueId -> ProjectId -> ExceptT GeneralError IO ()
removeIssueIdFromProject fp iid pid = do
    liftIO $ putStrLn $ "Remove issue " ++ show iid ++ " from project " ++ show pid
    mp <- (liftIO $ decodeFile (yamlFile fp pid))
    maybe (throwE $ "Project " ++ show pid ++ " not found")
          (\p -> liftIO $ encodeFile (yamlFile fp pid) (removeIssue iid p))
          mp

projectIdForIssue :: FilePath -> IssueId -> ExceptT GeneralError IO ProjectId
projectIdForIssue fp i = do
    ps <- liftIO $ listDirectory fp
    projectId `fmap` projectIdForIssue' fp i ps

projectIdForIssue' :: FilePath -> IssueId -> [FilePath] -> ExceptT GeneralError IO Project
projectIdForIssue' fp i [] = throwE $ "No project containting issue " ++ show i ++ " found"
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


readIssue :: FilePath -> IssueId -> ExceptT GeneralError IO Issue
readIssue fp ii = do
    liftIO . putStrLn $ "Read issue " ++ show ii
    i <- liftIO (decodeFileEither (yamlFile fp ii))
    case i of
      (Right i') -> return i'
      (Left err) -> throwE (show err)

setIssueStatus :: FilePath -> IssueId -> Status -> ExceptT GeneralError IO Issue
setIssueStatus fp i s = do
    liftIO . putStrLn $ "Change status of issue " ++ show i ++ " to " ++ show s
    mi <- liftIO $ decodeFile (yamlFile fp i)
    t <- liftIO getCurrentTime
    maybe
        (throwE $ "Could not update issue status. Issue not found: " ++ show i)
        (\ri -> do
            let ii = ri { issueStatus = s, issueLastUpdate = t }
            liftIO $ encodeFile (yamlFile fp i) ii
            return ii)
        mi

setIssueCategory :: FilePath -> IssueId -> Maybe Category -> ExceptT GeneralError IO Issue
setIssueCategory fp i c = do
    liftIO . putStrLn $ "Change category of issue " ++ show i ++ " to " ++ show c
    mi <- liftIO $ decodeFile (yamlFile fp i)
    t <- liftIO getCurrentTime
    maybe
        (throwE $ "Could not update issue status. Issue not found: " ++ show i)
        (\ri -> do
            let ii = ri { issueCategory = c, issueLastUpdate = t }
            liftIO $ encodeFile (yamlFile fp i) ii
            return ii)
        mi

setIssueDescription :: FilePath -> IssueId -> String -> ExceptT GeneralError IO Issue
setIssueDescription fp i d = do
    liftIO . putStrLn $ "Change description of issue " ++ show i ++ " to " ++ show d
    mi <- liftIO $ decodeFile (yamlFile fp i)
    t <- liftIO getCurrentTime
    maybe
        (throwE $ "Could not update issue description. Issue not found: " ++ show i)
        (\ri -> do
            let ii = ri { issueDescription = T.pack d, issueLastUpdate = t }
            liftIO $ encodeFile (yamlFile fp i) ii
            return ii)
        mi

setIssueSummary :: FilePath -> IssueId -> String -> ExceptT GeneralError IO Issue
setIssueSummary fp i d = do
    liftIO . putStrLn $ "Change summary of issue " ++ show i ++ " to " ++ show d
    mi <- liftIO $ decodeFile (yamlFile fp i)
    t <- liftIO getCurrentTime
    maybe
        (throwE $ "Could not update issue summary. Issue not found: " ++ show i)
        (\ri -> do
            let ii = ri { issueSummary = T.pack d, issueLastUpdate = t }
            liftIO $ encodeFile (yamlFile fp i) ii
            return ii)
        mi

yamlFile :: FilePath -> Int -> FilePath
yamlFile fp i = fp ++ "/" ++ show i ++ ".yaml"

readProject :: FilePath -> ProjectId -> ExceptT GeneralError IO Project
readProject fp pi = do
    liftIO . putStrLn $ "Read project " ++ show pi
    p <- liftIO (decodeFileEither (fp ++ "/" ++ show pi ++ ".yaml"))
    case p of
      Right p' -> return p'
      Left err -> throwE (show err)

