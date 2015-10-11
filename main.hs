
{-# LANGUAGE OverloadedStrings #-}

import Data.Text
import Data.Time

import Control.Monad.Trans.Either
import Control.Monad.Trans

import Text.ParserCombinators.Parsec

data Status = New | Feedback | Acknowledged | Confirmed | Assigned | Resovled | Closed deriving (Show)

data Project = Project {
    projectName :: Text
  , projectId :: Int
  , projectIssues :: [Issue]
} deriving (Show)

data Person = Person {
    personName :: Text
  , personId :: Int
} deriving (Show)

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
    m <- many alphaNum
    return m



