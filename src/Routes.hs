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

import Model (Issue (..), Project (..), Status (..), User (..), IssueE (..), IssueId, ProjectId, changeId)
import IO (writeUser, nextId, readIssue, deleteIssue, setIssueStatus, createIssue, readProject)

import Environment (userDir, issueDir, projectDir, jsDir, cssDir, imgDir)

import Util (throwServantErr)

-- Servant
--

type UserAPI = "users" :> ReqBody '[JSON] User :> Post '[JSON] User
         :<|> "createIssue" :> Capture "id" ProjectId :> Post '[HTML] Issue
         :<|> "deleteIssue" :> Capture "id" IssueId :> Post '[JSON] IssueId
         :<|> "issue" :> Capture "id" IssueId :> Get '[HTML] Issue
         :<|> "issueEdit" :> Capture "id" IssueId :> Get '[HTML] IssueE
         :<|> "project" :> Capture "id" ProjectId :> Get '[HTML] (Project, [Issue])
         :<|> "setIssueStatus" :> Capture "id" IssueId :> QueryParam "status" Status :> Post '[HTML] Issue
         :<|> "js" :> Raw
         :<|> "css" :> Raw
         :<|> "img" :> Raw

userAPI :: Proxy UserAPI
userAPI = Proxy

server = createUserR 
   :<|> createIssueR 
   :<|> deleteIssueR 
   :<|> issueR
   :<|> issueEditR
   :<|> projectR 
   :<|> setIssueStatusR
   :<|> serveDirectory jsDir
   :<|> serveDirectory cssDir
   :<|> serveDirectory imgDir

createUserR :: User -> EitherT ServantErr IO User
createUserR u = lift $ do
                  print u
                  i <- nextId userDir
                  let u' = changeId u i
                  writeUser (userDir ++ show i) u'
                  return u'

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
setIssueStatusR i (Just s) = throwServantErr $ do
    liftIO $ putStrLn ("Setting issue status of issue " ++ show i ++ " to " ++ show s)
    setIssueStatus issueDir i s
setIssueStatusR _ Nothing = left err500

issueR :: IssueId -> EitherT ServantErr IO Issue
issueR x = bimapEitherT (const err500) id $ readIssue issueDir x

issueEditR :: IssueId -> EitherT ServantErr IO IssueE
issueEditR x = bimapEitherT (const err500) id $ fmap IssueE $ readIssue issueDir x

