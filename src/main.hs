
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BS
import Data.Time

import System.Directory

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
import Testing

import Tools

app :: Application
app = serve userAPI server

main :: IO ()
main = do
    let p = 8081
    putStrLn $ "Running lantis server at " ++ show p
    run p app
