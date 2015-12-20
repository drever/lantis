module Parse (
    parseUser
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

rethrow :: Either ParseError a -> Either GeneralError a
rethrow (Left e) = Left $ show e
rethrow (Right x) = Right x

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

