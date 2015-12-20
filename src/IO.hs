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
    c <- map read `fmap` listDirectory fp
    if null c 
        then return 1 
        else return $ maximum c + 1

createIssue :: FilePath -> FilePath -> ProjectId -> EitherT String IO Issue
createIssue ip pp pi = undefined {-do
    projectH <- guardedFileOp (`openFile` ReadMode) (pp ++ "/" ++ show pi)
    p <- liftIO $ hGetContents projectH
    let parsedProject = hoistEither $ parseProject p
    proj <- bimapEitherT show id parsedProject
    liftIO $ hClose projectH
    i <- liftIO $ nextId ip
    let newIssue = emptyIssue i (projectId proj)
    liftIO $ writeFile (ip ++ "/" ++ show i) (T.unpack $ renderIssue newIssue)
    liftIO $ writeFile (pp ++ "/" ++ show (projectId proj)) (T.unpack $ renderProject (addIssue newIssue proj))
    return newIssue-}

deleteIssue :: FilePath -> FilePath -> IssueId -> EitherT String IO IssueId
deleteIssue ip pp i = do
    liftIO $ removeFile ip'
    pid <- projectIdForIssue pp i
    p <- readProject pp pid    
    liftIO $ writeFile (pp ++ "/" ++ show pid) (T.unpack $ renderProject $ removeIssue i p) 
    return i
    where ip' = ip ++ show i
  
projectIdForIssue :: FilePath -> IssueId -> EitherT GeneralError IO ProjectId
projectIdForIssue fp i = do
    ps <- liftIO $ listDirectory fp
    let pids = filterM (\pf -> do  
                   p <- readProject fp (read pf) 
		   return $ i `elem` projectIssues p) ps
    l <- length `fmap` pids
    if l > 0
        then fmap (read . head) pids
        else left $ "Invalid issue. An issue has to belong to exactly one project. This issue is orphaned." ++ show i

readIssue :: FilePath -> IssueId -> EitherT GeneralError IO Issue
readIssue fp ii = do
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
    p <- liftIO (decodeFileEither (fp ++ "/" ++ show pi ++ ".yaml"))
    bimapEitherT show id (hoistEither p)

{-readProject fp pi = do
    projectH <- guardedFileOp (`openFile` ReadMode) (fp ++ "/" ++ show pi)
    p <- liftIO $ hGetContents projectH
    let parsedProject = hoistEither $ parseProject p
    proj <- bimapEitherT show id parsedProject
    liftIO $ hClose projectH
    return proj   -}
