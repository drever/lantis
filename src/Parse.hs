module Parse (
    parseUser
  , parseIssue
  , parseProject
    ) where

import qualified Data.Text as T
import Text.ParserCombinators.Parsec

import Data.Time

import Model (User (..), Issue (..), Project (..), ViewStatus (..))

import Util (GeneralError)

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

