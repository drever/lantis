
{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

import Data.Text
import Data.Time

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

data Person = Person {
    personName :: Text
  , personId :: Int
} deriving (Show, Generic)

type Category = Text

data Priority = Low | High | Urgent deriving (Show)

data Severity = Minor | Major deriving (Show)

data Reproducibility = Sometimes | Always deriving (Show)

data Resolution = ResolutionOpen | ResolutionClosed deriving (Show)

data Relationship = RelationshipParent Issue Issue | RelationshipRelated Issue Issue deriving (Show)

data ViewStatus = Public | Private deriving (Show)

data Comment = Comment {
    commentCommenter :: Person
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
  , issueReporter :: Person
  , issueViewStatus :: ViewStatus
  , issueAssignedTo :: Maybe Person
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

readPerson :: FilePath -> EitherT ParseError IO Person
readPerson fp = do
    p <- lift $ readFile fp 
    hoistEither $ parsePerson p
    

writePerson :: FilePath -> Person -> IO ()
writePerson fp p = writeFile fp $ unpack $  
    "Name " `append` personName p `append` "\n" `append`
    "ID " `append` pack (show $ personId p)

parsePerson :: String -> Either ParseError Person
parsePerson = parse personParser "b"

personParser = do
    n <- pack `fmap` preferenceParser "Name"
    char '\n' 
    i <- read `fmap` preferenceParser "ID"
    return $ Person n i

preferenceParser p = do
    n <- string p
    spaces
    many alphaNum

-- Servant
--


instance ToJSON Person

type PersonAPI = "person" :> Capture "id" Int :> Get '[JSON] Person

server :: Int -> EitherT ServantErr IO Person
server x = bimapEitherT (const err404) id $ readPerson $ "examples/" ++ show x

personAPI :: Proxy PersonAPI
personAPI = Proxy

app :: Application
app = serve personAPI server

main :: IO ()
main = run 8081 app
