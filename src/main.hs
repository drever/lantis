
{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

import Data.Text hiding (maximum, map, filter, null)
import Data.Time

import System.Directory

import Control.Monad.Trans.Either
import Control.Monad.Trans

import Text.ParserCombinators.Parsec

import Data.Aeson
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

data Status = New | Feedback | Acknowledged | Confirmed | Assigned | Resovled | Closed deriving (Show)

data Project = Project {
    projectName :: Text
  , projectId :: Int
  , projectIssues :: [Issue]
} deriving (Show)

data User = User {
    userName :: Text
  , userId :: Int
} deriving (Show, Generic)

type Category = Text

data Priority = Low | High | Urgent deriving (Show)

data Severity = Minor | Major deriving (Show)

data Reproducibility = Sometimes | Always deriving (Show)

data Resolution = ResolutionOpen | ResolutionClosed deriving (Show)

data Relationship = RelationshipParent Issue Issue | RelationshipRelated Issue Issue deriving (Show)

data ViewStatus = Public | Private deriving (Show)

data Comment = Comment {
    commentCommenter :: User
  , commentId :: Int
  , commentText :: Text
  , commentDate :: UTCTime
} deriving (Show)

data Issue = Issue {
    issueState :: Status
  , issueSummary :: Text
  , issueDescription :: Text 
  , issueTags :: [Text]
  , issueRelationships :: [Relationship]
  , issueId :: Int
  , issueProject :: Project
  , issueCategory :: Maybe Category 
  , issueDateSubmitted :: UTCTime
  , issueLastUpdate :: UTCTime
  , issueReporter :: User
  , issueViewStatus :: ViewStatus
  , issueAssignedTo :: Maybe User
  , issueSeverity :: Maybe Severity
  , issuePriority :: Maybe Priority
  , issueReproducibility :: Maybe Reproducibility 
  , issueResolution :: Maybe Resolution
} deriving (Show)

-- |
-- IO

type GeneralError = String

readIssue :: FilePath -> IO Issue
readIssue = undefined

writeIssue :: FilePath -> Issue -> IO ()
writeIssue = undefined

readUser :: FilePath -> EitherT ParseError IO User
readUser fp = do
    p <- lift $ readFile fp 
    hoistEither $ parseUser p
    

writeUser :: FilePath -> User -> IO ()
writeUser fp p = writeFile fp $ unpack $  
    "Name " `append` userName p `append` "\n" `append`
    "ID " `append` pack (show $ userId p)

nextFreeUserId :: FilePath -> IO Int
nextFreeUserId fp = do
    c <- (map read . filter (\p -> p /= "." && p /= "..")) `fmap` getDirectoryContents fp
    if null c 
        then return 1 
        else return $ maximum c + 1

listUser :: FilePath -> EitherT ParseError IO [User]
listUser fp = do
    f <- liftIO $ filter (\p -> p /= "." && p /= "..") `fmap` getDirectoryContents fp
    mapM (readUser . ((fp ++ "/") ++)) f
 
createUser :: FilePath -> Text -> IO User
createUser fp n = do
    i <- nextFreeUserId fp
    let u = User n i
    writeUser (fp ++"/" ++ show i) u
    return u 

-- Parser
parseUser :: String -> Either ParseError User
parseUser = parse userParser "Could not parse user"

userParser = do
    n <- pack `fmap` preferenceParser "Name"
    i <- read `fmap` preferenceParser "ID"
    return $ User n i

preferenceParser p = do
    n <- string p
    spaces
    m <- many $ noneOf "\n"
    char '\n'
    return m

-- Servant
--


instance ToJSON User

type UserAPI = "user" :> Capture "id" Int :> Get '[JSON] User

server :: Int -> EitherT ServantErr IO User
server x = bimapEitherT (const err404) id $ readUser $ "user/" ++ show x

userAPI :: Proxy UserAPI
userAPI = Proxy

app :: Application
app = serve userAPI server

main :: IO ()
main = run 8081 app
