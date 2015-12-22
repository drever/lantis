{-# LANGUAGE OverloadedStrings #-}

module IO (
    readData
  , pappend
  , writeUser
  , renderProject
  , renderIssue
  , nextId
  , createIssue
  , deleteIssue
  , projectIdForIssue
  , readIssue
  , setIssueStatus
  , readProject
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

readData :: (String -> Either ParseError a) -> FilePath -> EitherT ParseError IO a
readData parser fp = do
    p <- lift $ readFile fp
    hoistEither $ parser p 

pappend :: String -> T.Text -> T.Text -> T.Text
pappend k v = T.append (T.pack k `T.append` " " `T.append` v `T.append` "\n")

writeUser :: FilePath -> User -> IO ()
writeUser fp p = writeFile fp $ T.unpack $  
    pappend "ID" (T.pack $ show $ userId p) $
    pappend "Name" (userName p) ""

renderProject :: Project -> T.Text
renderProject p = 
    pappend "ID" (T.pack $ show $ projectId p) $
    pappend "Name" (projectName p) $
    pappend "Issues" (T.pack $ show $ projectIssues p) $ 
    pappend "Status" (T.pack $ show $ projectStatus p) ""

renderIssue :: Issue -> T.Text
renderIssue i = 
    pappend "ID" (T.pack $ show $ issueId i) $
    pappend "Project" (T.pack $ show $ issueProject i) $
    pappend "Status" (T.pack $ show $ issueStatus i) $
    pappend "Summary" (issueSummary i) $
    pappend "Description" (issueDescription i) ""

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
    liftIO $ putStrLn $ "Creating new issue with id " ++ show i
    liftIO $ encodeFile (ip ++ "/" ++ show i ++ ".yaml") newIssue
    liftIO $ encodeFile (pp ++ "/" ++ show pi ++ ".yaml") (addIssue newIssue project)
    return newIssue

deleteIssue :: FilePath -> FilePath -> IssueId -> EitherT String IO IssueId
deleteIssue ip pp i = do
    liftIO $ putStr $ "Deleting issue " ++ show i
    liftIO $ removeFile ip'
    pid <- projectIdForIssue pp i
    p <- readProject pp pid    
    liftIO $ writeFile (pp ++ "/" ++ show pid ++ ".yaml") (T.unpack $ renderProject $ removeIssue i p) 
    return i
    where ip' = ip ++ show i ++ ".yaml"
  
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
setIssueStatus fp i s = undefined {-do
    h <- guardedFileOp (`openFile` ReadMode) isf
    issue <- liftIO $ hGetContents h
    parsedIssue <- cid s `fmap` hoistEither (parseIssue issue)
    liftIO $ hClose h
    liftIO $ writeFile isf (T.unpack $ renderIssue parsedIssue)
    return parsedIssue
    where cid :: Status -> Issue -> Issue
          cid s i = i { issueStatus = s }
          isf = fp ++ "/" ++ show i-}

readProject :: FilePath -> ProjectId -> EitherT GeneralError IO Project
readProject fp pi = do
    liftIO . putStrLn $ "Read project " ++ show pi
    p <- liftIO (decodeFileEither (fp ++ "/" ++ show pi ++ ".yaml"))
    bimapEitherT show id (hoistEither p)

{-readProject fp pi = do
    projectH <- guardedFileOp (`openFile` ReadMode) (fp ++ "/" ++ show pi)
    p <- liftIO $ hGetContents projectH
    let parsedProject = hoistEither $ parseProject p
    proj <- bimapEitherT show id parsedProject
    liftIO $ hClose projectH
    return proj   -}
