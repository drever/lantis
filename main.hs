
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

readPerson' :: String -> Either ParseError Person
readPerson' c = parse parsePerson "(unkown)" c

parsePerson :: String -> Either ParseError Person
parsePerson s = 
    let ss = parsePerson' s
    in do r <- do n <- nameFromList ss
                  return (Person n 1)
          case r of
              Nothing -> fail "Not all properties found"
              (Just p) -> return p
       
parsePerson' :: String -> Either ParseError [[String]]
parsePerson' input = parse personFile "(unknown)" input


nameFromList :: [[String]] -> Maybe String
nameFromList [] = Nothing
nameFromList ((n:ns):xs) = 
    if (n == "Name") 
    then Just $ intercalate " " ns
    else nameFromList xs

personFile = endBy line eol
line = sepBy cell (char ' ')
cell = many (noneOf " \n")
eol = char '\n'

--name = string "Name"
--id = string "ID"
