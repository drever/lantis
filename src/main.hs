
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

import Data.Aeson
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

-- 
-- ADTs

data Status = New | Feedback | Acknowledged | Confirmed | Assigned | Resovled | Closed deriving (Show)

data Project = Project {
    projectName :: T.Text
  , projectId :: Int
  , projectIssues :: [Issue]
} deriving (Show)

data User = User {
    userName :: T.Text
  , userId :: Int
} deriving (Show, Generic)

type Category = T.Text

data Priority = Low | High | Urgent deriving (Show)

data Severity = Minor | Major deriving (Show)

data Reproducibility = Sometimes | Always deriving (Show)

data Resolution = ResolutionOpen | ResolutionClosed deriving (Show)

data Relationship = RelationshipParent Issue Issue | RelationshipRelated Issue Issue deriving (Show)

data ViewStatus = Public | Private deriving (Show)

data Comment = Comment {
    commentCommenter :: User
  , commentId :: Int
  , commentText :: T.Text
  , commentDate :: UTCTime
} deriving (Show)

data Issue = Issue {
    issueState :: Status
  , issueSummary :: T.Text
  , issueDescription :: T.Text 
  , issueTags :: [T.Text]
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

-- | Environment
--

userDir = "data/user/"

-- | 
-- Data manipulation

userChangeId :: User -> Int -> User
userChangeId (User n i) i' = User n i'

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
writeUser fp p = writeFile fp $ T.unpack $  
    "Name " `T.append` userName p `T.append` "\n" `T.append`
    "ID " `T.append` T.pack (show $ userId p) `T.append` "\n"

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
 
createUser :: FilePath -> T.Text -> IO User
createUser fp n = do
    i <- nextFreeUserId fp
    let u = User n i
    writeUser (fp ++"/" ++ show i) u
    return u 

-- Parser
parseUser :: String -> Either ParseError User
parseUser = parse userParser "Could not parse user"

userParser = do
    n <- T.pack `fmap` preferenceParser "Name"
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
instance FromJSON User

type UserAPI = "user" :> Capture "id" Int :> Get '[JSON] User
         :<|> "myuser" :> Get '[JSON] User
         :<|> "users" :> ReqBody '[JSON] User :> Post '[JSON] User
         :<|> "users" :> Get '[JSON] [User]

myuser = User "Test 123" 15

userAPI :: Proxy UserAPI
userAPI = Proxy

server = (\x -> bimapEitherT (const err501) id $ readUser $ userDir ++ show x)
    :<|> (do
             liftIO $ putStrLn "Test"
             return myuser)
    :<|> (\u -> lift $ do
                  putStrLn $ show u
                  i <- nextFreeUserId userDir
                  let u' = userChangeId u i
                  writeUser (userDir ++ show i) u'
                  return u')
   :<|> do
           liftIO $ putStrLn "Listing all users"
           bimapEitherT (const err501) id $ listUser userDir
 
app :: Application
app = serve userAPI server

main :: IO ()
main = run 8081 app
