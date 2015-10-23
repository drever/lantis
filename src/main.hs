
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

import qualified Data.Text as T
import Data.Time

import System.Directory

import Control.Monad.Trans.Either
import Control.Monad.Trans

import Text.ParserCombinators.Parsec
import qualified Text.Blaze as B
import qualified Text.Blaze.Html4.Strict as BH

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

-- 
-- ADTs

data Status = New | Feedback | Acknowledged | Confirmed | Assigned | Resovled | Closed deriving (Show, Read, Generic)

data Project = Project {
    projectName :: T.Text
  , projectId :: ProjectId
  , projectIssues :: [IssueId]
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
    issueState :: Status
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

-- | Environment
--

userDir = "data/user/"
issueDir = "data/issue/"
projectDir = "data/project/"

-- | 
-- Data manipulation

userChangeId :: User -> Int -> User
userChangeId (User n i) i' = User n i'

-- |
-- IO

type GeneralError = String

readData :: (String -> Either ParseError a) -> FilePath -> EitherT ParseError IO a
readData parser fp = do
    p <- lift $ readFile fp
    hoistEither $ parser p 

readUser :: FilePath -> EitherT ParseError IO User
readUser = readData parseUser
    

writeUser :: FilePath -> User -> IO ()
writeUser fp p = writeFile fp $ T.unpack $  
    "ID " `T.append` T.pack (show $ userId p) `T.append` "\n" `T.append`
    "Name " `T.append` userName p `T.append` "\n"

writeProject :: FilePath -> Project -> IO ()
writeProject fp p = writeFile fp $ T.unpack $
    "ID " `T.append` T.pack (show $ projectId p) `T.append` "\n" `T.append`
    "Name " `T.append` projectName p `T.append` "\n" `T.append`
    "Issues " `T.append` T.pack (show $ projectIssues p) `T.append` "\n"

nextId :: FilePath -> IO Int
nextId fp = do
    c <- (map read) `fmap` (listDirectory fp)
    if null c 
        then return 1 
        else return $ maximum c + 1

listUser :: FilePath -> EitherT ParseError IO [User]
listUser fp = do
    f <- liftIO $ listDirectory fp
    mapM (readUser . ((fp ++ "/") ++)) f
 
createUser :: FilePath -> T.Text -> IO User
createUser fp n = do
    i <- nextId fp
    let u = User n i
    writeUser (fp ++"/" ++ show i) u
    return u 

readIssue :: FilePath -> EitherT ParseError IO Issue
readIssue fp = do
    p <- lift $ readFile fp
    hoistEither $ parseIssue p

readProject :: FilePath -> EitherT ParseError IO Project
readProject fp = do
    p <- lift $ readFile fp
    hoistEither $ parseProject p

-- Parser
parseUser :: String -> Either ParseError User
parseUser = parse userParser "Could not parse user"

userParser :: GenParser Char st User
userParser = do
    i <- read `fmap` preferenceParser "ID"
    n <- T.pack `fmap` preferenceParser "Name"
    return $ User n i

parseIssue :: String -> Either ParseError Issue
parseIssue = parse issueParser "Could not parse issue"

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
    return $ Project n i is

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
    toMarkup i = BH.html $ do
        BH.head $ do
            BH.title "lantis issues tracker"
        BH.body $ do
            BH.p $ BH.toMarkup $ "Issue number " ++ (show $ issueId i)
            BH.string $ "Status " ++ (show $ issueState i)
            BH.h1 $ BH.string (show $ issueSummary i)
            BH.string (show $ issueDescription i)

instance B.ToMarkup Project where
    toMarkup p = BH.html $ do
        BH.head $ do
            BH.title $ "lantis" 
        BH.body $ do
        BH.p $ (BH.toHtml) (projectName p)
        BH.ul $ do
            mapM_ (BH.i . BH.toHtml) (projectIssues p)

-- Servant
--


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

type UserAPI = "user" :> Capture "id" UserId :> Get '[JSON] User
         :<|> "myuser" :> Get '[JSON] User
         :<|> "users" :> ReqBody '[JSON] User :> Post '[JSON] User
         :<|> "users" :> Get '[JSON] [User]
         :<|> "issue" :> Capture "id" IssueId :> Get '[HTML] Issue
         :<|> "project" :> Capture "id" ProjectId :> Get '[HTML] Project

myuser = User "Test 123" 15

userAPI :: Proxy UserAPI
userAPI = Proxy

server = (\x -> bimapEitherT (const err501) id $ readUser $ userDir ++ show x)
    :<|> (do
             liftIO $ putStrLn "Test"
             return myuser)
    :<|> (\u -> lift $ do
                  putStrLn $ show u
                  i <- nextId userDir
                  let u' = userChangeId u i
                  writeUser (userDir ++ show i) u'
                  return u')
   :<|> do
           liftIO $ putStrLn "Listing all users"
           bimapEitherT (const err501) id $ listUser userDir
   :<|> (\x -> bimapEitherT (const err501) id $ readIssue $ issueDir ++ show x)
   :<|> (\x -> bimapEitherT (const err501) id $ readProject $ projectDir ++ show x)

app :: Application
app = serve userAPI server

main :: IO ()
main = run 8081 app
