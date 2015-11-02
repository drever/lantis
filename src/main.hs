
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

{-# LANGUAGE FlexibleInstances #-}

import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BS
import Data.Time

import System.Directory
import System.IO

import Control.Monad.Trans.Either
import Control.Monad.Trans
import Control.Monad

import Text.ParserCombinators.Parsec
import qualified Text.Blaze as B
import qualified Text.Blaze.Html5 as BH
import qualified Text.Blaze.Html5.Attributes as A

import Data.Aeson
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.HTML.Blaze

-- util
--
listDirectory :: FilePath -> IO [FilePath]
listDirectory fp = fmap (filter (\p -> p /= "." && p /= "..")) (getDirectoryContents  fp)

guardedFileOp :: (FilePath -> IO b) -> FilePath -> EitherT String IO b
guardedFileOp op fp = do
    b <- liftIO $ doesFileExist fp
    if b 
        then liftIO $ op fp
        else left $ "file not found: " ++ fp

throwServantErr = bimapEitherT convertError id

-- 
-- ADTs

data Status =
      InProgress
    | Done
    | New
    | Feedback
    | Acknowledged
    | Confirmed
    | Assigned
    | Resovled
    | Closed deriving (Show, Read, Generic, Eq)

data Project = Project {
    projectName :: T.Text
  , projectId :: ProjectId
  , projectIssues :: [IssueId]
  , projectStatus :: [Status]
} deriving (Show)

data User = User {
    userName :: T.Text
  , userId :: UserId
} deriving (Show, Generic)

data Category = Bug | Feature | ActionItem deriving (Show, Read, Generic)

type ProjectId = Int
type UserId = Int
type IssueId = Int
type CommentId = Int

data Priority = Low | High | Urgent deriving (Show, Generic)

data Severity = Minor | Major deriving (Show, Generic)

data Reproducibility = Sometimes | Always deriving (Show, Generic)

data Resolution = ResolutionOpen | ResolutionClosed deriving (Show, Generic)

data Relationship = RelationshipParent IssueId | RelationshipRelated IssueId deriving (Show, Generic)

data ViewStatus = Public | Private deriving (Show, Read, Generic)

data Comment = Comment {
    commentCommenter :: User
  , commentId :: CommentId
  , commentText :: T.Text
  , commentDate :: UTCTime
} deriving (Show)

data Issue = Issue {
    issueStatus :: Status
  , issueSummary :: T.Text
  , issueDescription :: T.Text 
  , issueTags :: [T.Text]
  , issueRelationships :: [Relationship]
  , issueId :: IssueId
  , issueProject :: ProjectId
  , issueCategory :: Maybe Category 
  , issueDateSubmitted :: UTCTime
  , issueLastUpdate :: UTCTime
  , issueReporter :: UserId
  , issueViewStatus :: ViewStatus
  , issueAssignedTo :: Maybe UserId
  , issueSeverity :: Maybe Severity
  , issuePriority :: Maybe Priority
  , issueReproducibility :: Maybe Reproducibility 
  , issueResolution :: Maybe Resolution
} deriving (Show, Generic)

emptyIssue :: IssueId -> ProjectId -> Issue
emptyIssue i p =
        Issue
	    New -- status
	    "empty" --summary 
	    "empty" --description
	    [] -- tags
	    [] -- relationships
	    i -- issueId 
	    p -- issueProject :: ProjectId
	    Nothing -- issueCategory :: Maybe Category 
	    (UTCTime (fromGregorian 1970 0 0) 0)--issueDateSubmitted :: UTCTime
	    (UTCTime (fromGregorian 1970 0 0) 0)--issueLastUpdate :: UTCTime
	    0 --issueReporter :: UserId
	    Public --issueViewStatus :: ViewStatus
	    Nothing --issueAssignedTo :: Maybe User
	    Nothing --issueSeverity :: Maybe Severity
	    Nothing --issuePriority :: Maybe Priority
	    Nothing --issueReproducibility :: Maybe Reproducibility 
	    Nothing --issueResolution :: Maybe Resolution

-- | Environment
--

userDir = "data/user/"
issueDir = "data/issue/"
projectDir = "data/project/"

jsDir = "webroot/js"
cssDir = "webroot/css"

imgDir = "webroot/img"

-- | 
-- Data manipulation

changeId :: User -> Int -> User
changeId (User n i) = User n

addIssue :: Issue -> Project -> Project
addIssue i p = p { projectIssues = issueId i:projectIssues p }

removeIssue :: IssueId -> Project -> Project
removeIssue i p = p { projectIssues = filter (/=i) (projectIssues p) }

-- |
-- IO

type GeneralError = String

convertError :: GeneralError -> ServantErr
convertError e = ServantErr 103 e BS.empty []

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
 
-- Parser
parseUser :: String -> Either ParseError User
parseUser = parse userParser "Could not parse user"

userParser :: GenParser Char st User
userParser = do
    i <- read `fmap` preferenceParser "ID"
    n <- T.pack `fmap` preferenceParser "Name"
    return $ User n i

parseIssue :: String -> Either GeneralError Issue
parseIssue = rethrow . parse issueParser "Could not parse issue"

rethrow :: Either ParseError a -> Either GeneralError a
rethrow (Left e) = Left $ show e
rethrow (Right x) = Right x

issueParser :: GenParser Char st Issue
issueParser = do
    i <- read `fmap` preferenceParser "ID"
    p <- read `fmap` preferenceParser "Project"
    s <- read `fmap` preferenceParser "Status"
    su <- T.pack `fmap` preferenceParser "Summary"
    d <- T.pack `fmap` preferenceParser "Description"
    return $
        Issue
	    s -- status
	    su --summary 
	    d --description
	    [] -- tags
	    [] -- relationships
	    i -- issueId 
	    p -- issueProject :: ProjectId
	    Nothing -- issueCategory :: Maybe Category 
	    (UTCTime (fromGregorian 1970 0 0) 0)--issueDateSubmitted :: UTCTime
	    (UTCTime (fromGregorian 1970 0 0) 0)--issueLastUpdate :: UTCTime
	    0 --issueReporter :: UserId
	    Public --issueViewStatus :: ViewStatus
	    Nothing --issueAssignedTo :: Maybe User
	    Nothing --issueSeverity :: Maybe Severity
	    Nothing --issuePriority :: Maybe Priority
	    Nothing --issueReproducibility :: Maybe Reproducibility 
	    Nothing --issueResolution :: Maybe Resolution

parseProject :: String -> Either ParseError Project
parseProject = parse projectParser "Could not parse project"

projectParser :: GenParser Char st Project
projectParser = do
    i <- read `fmap` preferenceParser "ID"
    n <- T.pack `fmap` preferenceParser "Name"
    is <- read `fmap` preferenceParser "Issues"
    ss <- read `fmap` preferenceParser "Status"
    return $ Project n i is ss

preferenceParser :: String -> GenParser Char st String
preferenceParser p = do
    n <- string p
    spaces
    m <- many $ noneOf "\n"
    char '\n'
    return m

-- blaze
--

instance B.ToMarkup Issue where
    toMarkup = card

instance B.ToMarkup Project where
    toMarkup p = BH.html $ do
        BH.head $
            BH.title "lantis" 
        BH.body $ do
        BH.h1 $ BH.toHtml (projectName p)
        BH.ul $ 
            mapM_ (BH.li . BH.toHtml) (projectIssues p)

instance B.ToMarkup Status where
    toMarkup = BH.toHtml . show

instance B.ToMarkup (Project, [Issue]) where
    toMarkup (p, is) = BH.html $ do
        BH.head $ do
             BH.title "lantis"
             BH.link BH.! A.rel "stylesheet" BH.! A.type_ "text/css" BH.! A.href "../css/lantis.css"
             BH.script BH.! A.src "../js/jquery-2.1.4.js" $ "" 
             BH.script BH.! A.src "../js/lantis.js" $ ""
             BH.script $ BH.toHtml $ "lantis.projectId = " ++ show (projectId p)
        BH.div BH.! A.id "header" $ do
             BH.body $ BH.img BH.! A.src "../img/lantis.png"
             BH.h1 $ BH.toHtml (projectName p)
        BH.div BH.! A.id "content" $ do
             controls
             mapM_ (column is) (projectStatus p)

controls :: BH.Markup
controls = 
    BH.div BH.! A.id "controls" $ 
        BH.button BH.! A.onclick "lantis.createIssue(lantis.projectId)" $ "New issue"

column :: [Issue] -> Status -> BH.Markup
column is s = BH.div BH.! A.id (BH.toValue $ show s) BH.! A.class_ "column" BH.! A.ondragover "lantis.allowDrag(event)" BH.! A.ondrop "lantis.drop(event)" $ do
    BH.h1 $ BH.toHtml s
    mapM_ card (filter (\x -> issueStatus x == s) is)

card :: Issue -> BH.Markup
card i = BH.div BH.! A.id (BH.toValue ("issue" ++ show (issueId i))) BH.! A.class_ "card" BH.! A.draggable (BH.toValue True) BH.! A.ondragstart "lantis.drag(event)" $
    BH.toHtml $ BH.html $ do
      BH.button BH.! A.class_ "delete" BH.! A.onclick  (BH.toValue $ "lantis.deleteIssue(" ++ show (issueId i) ++ ")") $ "X"
      BH.h2 $ BH.string (T.unpack $ issueSummary i)
      BH.ul $ 
               BH.li $ BH.toMarkup $ "#" ++ show (issueId i)
      BH.string (T.unpack $ issueDescription i)

-- Servant
--

instance FromText Status where
    fromText = Just . read . T.unpack

instance ToJSON User
instance FromJSON User

instance ToJSON Issue
instance FromJSON Issue

instance ToJSON Category
instance FromJSON Category

instance ToJSON Status
instance FromJSON Status

instance ToJSON Priority
instance FromJSON Priority

instance ToJSON Severity
instance FromJSON Severity

instance ToJSON Reproducibility
instance FromJSON Reproducibility

instance FromJSON Resolution
instance ToJSON Resolution

instance FromJSON Relationship
instance ToJSON Relationship

instance FromJSON ViewStatus
instance ToJSON ViewStatus

type UserAPI = "users" :> ReqBody '[JSON] User :> Post '[JSON] User
         :<|> "createIssue" :> Capture "id" ProjectId :> Post '[HTML] Issue
         :<|> "deleteIssue" :> Capture "id" IssueId :> Post '[JSON] IssueId
         :<|> "users" :> Get '[JSON] [User]
         :<|> "issue" :> Capture "id" IssueId :> Get '[HTML] Issue
         :<|> "project" :> Capture "id" ProjectId :> Get '[HTML] (Project, [Issue])
         :<|> "setIssueStatus" :> Capture "id" IssueId :> QueryParam "status" Status :> Post '[HTML] Issue
         :<|> "js" :> Raw
         :<|> "css" :> Raw
         :<|> "img" :> Raw

userAPI :: Proxy UserAPI
userAPI = Proxy

server = createUserR 
   :<|> createIssueR 
   :<|> deleteIssueR 
   :<|> usersR
   :<|> issueR 
   :<|> projectR 
   :<|> setIssueStatusR
   :<|> serveDirectory jsDir
   :<|> serveDirectory cssDir
   :<|> serveDirectory imgDir

createUserR u = lift $ do
                  print u
                  i <- nextId userDir
                  let u' = changeId u i
                  writeUser (userDir ++ show i) u'
                  return u'

createIssueR p = throwServantErr $ createIssue issueDir projectDir p
deleteIssueR p = throwServantErr $ deleteIssue issueDir projectDir p

projectR x = throwServantErr $ do 
                 p <- readProject projectDir x
                 is <- mapM (readIssue issueDir) (projectIssues p)
                 return (p, is)

setIssueStatusR :: IssueId -> Maybe Status -> EitherT ServantErr IO Issue
setIssueStatusR i (Just s) = throwServantErr $ do
    liftIO $ putStrLn ("Setting issue status of issue " ++ show i ++ " to " ++ show s)
    setIssueStatus issueDir i s
setIssueStatusR _ Nothing = left err500

issueR x = bimapEitherT (const err500) id $ readIssue issueDir x

usersR = do
           liftIO $ putStrLn "Listing all users"
           bimapEitherT (const err500) id $ listUser userDir

app :: Application
app = serve userAPI server

main :: IO ()
main = run 8081 app
