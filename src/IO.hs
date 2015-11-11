{-# LANGUAGE OverloadedStrings #-}

module IO (
    readData
  , readUser
  , pappend
  , writeUser
  , renderProject
  , renderIssue
  , nextId
  , listUser
  , createUser
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
import Parse (parseIssue, parseProject, parseUser)
import Util (listDirectory, guardedFileOp, GeneralError)

import qualified Data.Text as T
import Text.ParserCombinators.Parsec (ParseError)

import Control.Monad.Trans.Either

readData :: (String -> Either ParseError a) -> FilePath -> EitherT ParseError IO a
readData parser fp = do
    p <- lift $ readFile fp
    hoistEither $ parser p 

readUser :: FilePath -> UserId -> EitherT ParseError IO User
readUser fp ui = readData parseUser (fp ++ "/" ++ show ui)

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

listUser :: FilePath -> EitherT ParseError IO [User]
listUser fp = do
    f <- liftIO $ listDirectory fp
    mapM (readUser fp . read) f
 
createUser :: FilePath -> T.Text -> IO User
createUser fp n = do
    i <- nextId fp
    let u = User n i
    writeUser (fp ++"/" ++ show i) u
    return u 

createIssue :: FilePath -> FilePath -> ProjectId -> EitherT String IO Issue
createIssue ip pp pi = do
    projectH <- guardedFileOp (`openFile` ReadMode) (pp ++ "/" ++ show pi)
    p <- liftIO $ hGetContents projectH
    let parsedProject = hoistEither $ parseProject p
    proj <- bimapEitherT show id parsedProject
    liftIO $ hClose projectH
    i <- liftIO $ nextId ip
    let newIssue = emptyIssue i (projectId proj)
    liftIO $ writeFile (ip ++ "/" ++ show i) (T.unpack $ renderIssue newIssue)
    liftIO $ writeFile (pp ++ "/" ++ show (projectId proj)) (T.unpack $ renderProject (addIssue newIssue proj))
    return newIssue

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
    b <- liftIO $ doesFileExist (fp ++ "/" ++ show ii)
    if b
        then do p <- lift $ readFile (fp ++ "/" ++ show ii)
		bimapEitherT show id $ hoistEither $ parseIssue p
        else left $ "File not found for issue id " ++ show ii

setIssueStatus :: FilePath -> IssueId -> Status -> EitherT GeneralError IO Issue
setIssueStatus fp i s = do
    h <- guardedFileOp (`openFile` ReadMode) isf
    issue <- liftIO $ hGetContents h
    parsedIssue <- cid s `fmap` hoistEither (parseIssue issue)
    liftIO $ hClose h
    liftIO $ writeFile isf (T.unpack $ renderIssue parsedIssue)
    return parsedIssue
    where cid :: Status -> Issue -> Issue
          cid s i = i { issueStatus = s }
          isf = fp ++ "/" ++ show i

readProject :: FilePath -> ProjectId -> EitherT GeneralError IO Project
readProject fp pi = do
    projectH <- guardedFileOp (`openFile` ReadMode) (fp ++ "/" ++ show pi)
    p <- liftIO $ hGetContents projectH
    let parsedProject = hoistEither $ parseProject p
    proj <- bimapEitherT show id parsedProject
    liftIO $ hClose projectH
    return proj   
