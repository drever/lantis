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
import Control.Monad.Trans.Either

import Model (Issue (..), Project (..), Status (..), User (..), IssueE (..), IssueId, ProjectId)
import IO (readIssue, deleteIssue, setIssueStatus, createIssue, readProject)

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
         :<|> "js" :> Raw
         :<|> "css" :> Raw
         :<|> "img" :> Raw

         :<|> "testissue" :> Get '[HTML] Issue
         :<|> "testproject" :> Get '[HTML] Project
         :<|> "testprojectissues" :> Get '[HTML] (Project, [Issue])

userAPI :: Proxy UserAPI
userAPI = Proxy

server = createIssueR 
   :<|> deleteIssueR 
   :<|> issueR
   :<|> issueEditR
   :<|> projectR 
   :<|> setIssueStatusR
   :<|> serveDirectory jsDir
   :<|> serveDirectory cssDir
   :<|> serveDirectory imgDir

   :<|> testIssueR
   :<|> testProjectR
   :<|> testProjectIssuesR

createIssueR :: ProjectId -> EitherT ServantErr IO Issue
createIssueR p = throwServantErr $ createIssue issueDir projectDir p

deleteIssueR :: IssueId -> EitherT ServantErr IO IssueId
deleteIssueR p = throwServantErr $ deleteIssue issueDir projectDir p

projectR :: ProjectId -> EitherT ServantErr IO (Project, [Issue])
projectR x = throwServantErr $ do 
                 p <- readProject projectDir x
                 is <- mapM (readIssue issueDir) (projectIssues p)
                 return (p, is)

setIssueStatusR :: IssueId -> Maybe Status -> EitherT ServantErr IO Issue
setIssueStatusR i (Just s) = throwServantErr $
    setIssueStatus issueDir i s
setIssueStatusR _ Nothing = left err500

issueR :: IssueId -> EitherT ServantErr IO Issue
issueR x = bimapEitherT (const err500) id $ readIssue issueDir x

issueEditR :: IssueId -> EitherT ServantErr IO IssueE
issueEditR x = bimapEitherT (const err500) id $ fmap IssueE $ readIssue issueDir x

testIssueR :: EitherT ServantErr IO Issue
testIssueR = liftIO $ generate arbitrary

testProjectR :: EitherT ServantErr IO Project
testProjectR = liftIO $ generate arbitrary

testProjectIssuesR :: EitherT ServantErr IO (Project, [Issue])
testProjectIssuesR = do
    p <- liftIO $ generate arbitrary
    is <- liftIO $ generate arbitrary
    return (p { projectIssues = unique $ projectIssues p, projectStatus = unique $ projectStatus p }, is)
