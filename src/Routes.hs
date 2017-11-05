{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Routes (
    userAPI
  , server
    ) where

import Servant
import Servant.HTML.Blaze

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Except
import qualified Control.Monad.Catch as C

import Model (Issue (..), Project (..), Status (..), User (..), IssueE (..), IssueDescription (..), IssueDescriptionE (..), Category (..), IssueId, ProjectId)
import IO (readIssue, deleteIssue, setIssueStatus, setIssueCategory, setIssueSummary, setIssueDescription, createIssue, readProject)

import Environment (userDir, issueDir, projectDir, jsDir, cssDir, imgDir)

import Util (throwServantErr, unique)

import Test.QuickCheck
import Testing

-- Servant
--

type UserAPI = "createIssue" :> Capture "id" ProjectId :> Post '[HTML] Issue
         :<|> "deleteIssue" :> Capture "id" IssueId :> Post '[JSON] IssueId
         :<|> "issue" :> Capture "id" IssueId :> Get '[HTML] Issue
         :<|> "issueEdit" :> Capture "id" IssueId :> Get '[HTML] IssueE
         :<|> "project" :> Capture "id" ProjectId :> Get '[HTML] (Project, [Issue])
         :<|> "setIssueStatus" :> Capture "id" IssueId :> QueryParam "status" Status :> Post '[HTML] Issue
         :<|> "setIssueCategory" :> Capture "id" IssueId :> QueryParam "category" Category :> Post '[HTML] IssueE
         :<|> "setIssueDescription" :> Capture "id" IssueId :> QueryParam "description" String :> Post '[HTML] IssueDescription
         :<|> "setIssueSummary" :> Capture "id" IssueId :> QueryParam "summary" String :> Post '[HTML] IssueE
         :<|> "mdIssueDescription" :> Capture "id" IssueId :> Get '[HTML] IssueDescription
         :<|> "mdIssueDescriptionEdit" :> Capture "id" IssueId :> Get '[HTML] IssueDescriptionE

         :<|> "js" :> Raw
         :<|> "css" :> Raw
         :<|> "img" :> Raw

         :<|> "testissue" :> Get '[HTML] Issue
         :<|> "testproject" :> Get '[HTML] Project
         :<|> "testprojectissues" :> Get '[HTML] (Project, [Issue])
         :<|> "testissueE" :> Get '[HTML] IssueE

userAPI :: Proxy UserAPI
userAPI = Proxy

server = createIssueR
   :<|> deleteIssueR
   :<|> issueR
   :<|> issueEditR
   :<|> projectR
   :<|> setIssueStatusR
   :<|> setIssueCategoryR
   :<|> setIssueDescriptionR
   :<|> setIssueSummaryR
   :<|> mdIssueDescriptionR
   :<|> mdIssueDescriptionEditR

   :<|> serveDirectoryFileServer jsDir
   :<|> serveDirectoryFileServer cssDir
   :<|> serveDirectoryFileServer imgDir

   :<|> testIssueR
   :<|> testProjectR
   :<|> testProjectIssuesR
   :<|> testIssueER

createIssueR :: ProjectId -> Handler Issue
createIssueR p = throwServantErr $ (createIssue issueDir projectDir p)

deleteIssueR :: IssueId -> Handler IssueId
deleteIssueR p = throwServantErr $ deleteIssue issueDir projectDir p

projectR :: ProjectId -> Handler (Project, [Issue])
projectR x = throwServantErr $ do
                 p <- readProject projectDir x
                 is <- mapM (readIssue issueDir) (projectIssues p)
                 return (p, is)

setIssueStatusR :: IssueId -> Maybe Status -> Handler Issue
setIssueStatusR _ Nothing = C.throwM err500
setIssueStatusR i (Just s) = throwServantErr $
    setIssueStatus issueDir i s

mdIssueDescriptionEditR :: IssueId -> Handler IssueDescriptionE
mdIssueDescriptionEditR i = throwServantErr $
    (IssueDescriptionE . issueDescription) `fmap` readIssue issueDir i

mdIssueDescriptionR :: IssueId -> Handler IssueDescription
mdIssueDescriptionR i = throwServantErr $
    (IssueDescription . issueDescription) `fmap` readIssue issueDir i

setIssueSummaryR :: IssueId -> Maybe String -> Handler IssueE
setIssueSummaryR _ Nothing = C.throwM err500
setIssueSummaryR i (Just s) = throwServantErr $
    IssueE `fmap` setIssueSummary issueDir i s

setIssueCategoryR :: IssueId -> Maybe Category -> Handler IssueE
setIssueCategoryR i c = throwServantErr $
    IssueE `fmap` setIssueCategory issueDir i c

setIssueDescriptionR :: IssueId -> Maybe String -> Handler IssueDescription
setIssueDescriptionR _ Nothing = C.throwM err500
setIssueDescriptionR i (Just d) = throwServantErr $
    (IssueDescription . issueDescription) `fmap` setIssueDescription issueDir i d

issueR :: IssueId -> Handler Issue
issueR x = throwServantErr $ readIssue issueDir x

issueEditR :: IssueId -> Handler IssueE
issueEditR x = throwServantErr $ fmap IssueE $ readIssue issueDir x

testIssueR :: Handler Issue
testIssueR = liftIO $ generate arbitrary

testProjectR :: Handler Project
testProjectR = liftIO $ generate arbitrary

testProjectIssuesR :: Handler (Project, [Issue])
testProjectIssuesR = do
    p <- liftIO $ generate arbitrary
    is <- liftIO $ generate arbitrary
    return (p { projectIssues = unique $ projectIssues p, projectStatus = unique $ projectStatus p }, is)

testIssueER :: Handler IssueE
testIssueER = liftIO $ generate arbitrary
