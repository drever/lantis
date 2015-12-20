{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Model (
     Project (..)
   , User (..)
   , Issue (..)
   , IssueE (..)
   , Status (..)
   , ViewStatus (..)
   , ProjectId
   , IssueId
   , UserId
   , emptyIssue
   , changeId
   , addIssue
   , removeIssue
   ) where

import Data.Time
import qualified Data.Text as T

import Data.Aeson

import Servant

import GHC.Generics

instance FromText Status where
    fromText = Just . read . T.unpack

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

type ProjectId = Int
type IssueId = Int

data Status =
      InProgress
    | Done
    | New
    | Feedback
    | Acknowledged
    | Confirmed
    | Assigned
    | Resovled
    | Closed deriving (Show, Read, Eq, Generic)

data Project = Project {
    projectName :: T.Text
  , projectId :: ProjectId
  , projectIssues :: [IssueId]
  , projectStatus :: [Status]
} deriving (Show, Generic)

data User = User {
    userName :: T.Text
  , userId :: UserId
} deriving (Show, Generic)

data Category = Bug | Feature | ActionItem deriving (Show, Read, Generic)

type UserId = Int
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
} deriving (Show, Generic)

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

newtype IssueE = IssueE Issue 

-- |
-- Aeson instances

instance ToJSON User
instance FromJSON User

instance ToJSON Project
instance FromJSON Project

instance ToJSON Status
instance FromJSON Status

instance ToJSON Issue
instance FromJSON Issue

instance ToJSON Category
instance FromJSON Category

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

-- | 
-- Data manipulation

changeId :: User -> Int -> User
changeId (User n i) = User n

addIssue :: Issue -> Project -> Project
addIssue i p = p { projectIssues = issueId i:projectIssues p }

removeIssue :: IssueId -> Project -> Project
removeIssue i p = p { projectIssues = filter (/=i) (projectIssues p) }
