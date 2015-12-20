
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BS
import Data.Time

import System.Directory

import Control.Monad.Trans.Either
import Control.Monad.Trans
import Control.Monad

import Data.Aeson
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.HTML.Blaze

import Model
import Environment
import Util
import IO
import Markup
import Routes

import Tools

app :: Application
app = serve userAPI server

main :: IO ()
main = run 8081 app

-- Testing
--
-- 1) addIssue to project
-- 2) read project
-- 3) check 
--     a) list of projects length has grown by one
--     b) new issue is part of project issues

