{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Markup (
    ) where

import qualified Text.Blaze as B
import qualified Text.Blaze.Html5 as BH
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Internal

import Data.Maybe

import Text.Pandoc

import qualified Data.Text as T

import Model (Issue (..), Project (..), IssueE (..), IssueDescription (..), Status (..), categories)

instance B.ToMarkup Issue where
    toMarkup = card

instance B.ToMarkup Project where
    toMarkup p = BH.html $ do
        BH.head $
            BH.title "lantis" 
        BH.body $ do
        BH.h1 $ BH.toHtml (projectName p)
        BH.ul $ 
            mapM_ (BH.li . BH.toHtml) (projectIssues p)

instance B.ToMarkup IssueDescription where
    toMarkup (IssueDescription i) = BH.toHtml $ 
        writeHtml def md
        where (Right md) = readMarkdown def $ T.unpack i

instance B.ToMarkup Status where
    toMarkup = BH.toHtml . show

instance B.ToMarkup (Project, [Issue]) where
    toMarkup (p, is) = BH.html $ do
        BH.head $ do
             BH.title "lantis"
             BH.link BH.! A.rel "stylesheet" BH.! A.type_ "text/css" BH.! A.href "../css/lantis.css"
             BH.script BH.! A.src "../js/jquery-2.1.4.js" $ "" 
             BH.script BH.! A.src "../js/lantis.js" $ ""
             BH.script $ BH.toHtml $ "lantis.projectId = " ++ show (projectId p)
        BH.div BH.! A.id "header" $ do
             BH.body $ BH.img BH.! A.src "../img/lantis.png"
             BH.h1 $ BH.toHtml (projectName p)
        BH.div BH.! A.id "content" $ do
             controls
             mapM_ (column is) (projectStatus p)

instance B.ToMarkup IssueE where
    toMarkup (IssueE i) = do
        BH.div BH.! A.id (BH.toValue $ show (issueId i)) BH.! A.class_ "edit" $ do
             BH.button BH.! A.class_ "delete" BH.! A.onclick "lantis.hideIssue()" $ "X" 
             BH.div $ do
                 BH.h2 (do 
                     BH.toMarkup $ "#" ++ show (issueId i) ++ ": " 
                     BH.span BH.! A.id "title" 
                             BH.! (A.contenteditable $ B.toValue True) 
                             BH.! A.onfocus "lantis.setEditModeActive(this)" 
                             BH.! A.onblur "lantis.setIssueSummary(this);lantis.setEditModePassive(this)" 
                             BH.! A.onkeydown "if (event.keyCode == 13) { lantis.setIssueSummary(this);lantis.setEditModePassive(this) }" $ BH.toMarkup $ T.unpack (issueSummary i))
             BH.div . BH.ul $ do
                 BH.li . BH.toMarkup $ "Created: " ++ show (issueDateSubmitted i)
                 BH.li . BH.toMarkup $ "Last updated: " ++ show (issueLastUpdate i)
             BH.div . BH.i . BH.string . show $ issueStatus i
             BH.div . (BH.select ! A.onchange "lantis.setIssueCategory(this)") $
                 sequence_ (
                      ((BH.option !? (isNothing $ issueCategory i, A.selected "")) . BH.string $ "") : 
                      (map (\x -> (BH.option !? (maybe False (==x) (issueCategory i), A.selected "")) . BH.string $ show x) categories))
             renderMarkdown (issueDescription i)
             --BH.toHtml (IssueDescription $ issueDescription i)

renderMarkdown :: T.Text -> BH.Markup
renderMarkdown b = do
    let sp = T.splitOn "\n" b 
    BH.textarea BH.! A.class_ "modepassive" BH.! A.onfocus "lantis.setEditModeActive(this)" BH.! A.onblur "lantis.setIssueDescription(this);lantis.setEditModePassive(this)" $ sequence_ (map (BH.string . (++"\n") . T.unpack) sp)

controls :: BH.Markup
controls = 
    BH.div BH.! A.id "controls" $ 
        BH.button BH.! A.onclick "lantis.createIssue(lantis.projectId)" $ "New issue"

column :: [Issue] -> Status -> BH.Markup
column is s = BH.div BH.! A.id (BH.toValue $ show s) BH.! A.class_ "column" BH.! A.ondragover "lantis.allowDrag(event)" BH.! A.ondrop "lantis.drop(event)" $ do
    BH.h1 $ BH.toHtml s
    mapM_ card (filter (\x -> issueStatus x == s) is)

card :: Issue -> BH.Markup
card i = BH.div BH.! A.id (BH.toValue ("issue" ++ show (issueId i))) BH.! A.class_ "card" BH.! A.draggable (BH.toValue True) BH.! A.ondragstart "lantis.drag(event)" BH.! A.ondblclick "lantis.editIssue(lantis.issueIdFromCard(this))" $
    BH.toHtml $ BH.html $ do
      BH.button BH.! A.class_ "delete" BH.! A.onclick (BH.toValue $ "lantis.deleteIssue(" ++ show (issueId i) ++ ")") $ "X"
      BH.h2 $ BH.string $ "#" ++ show (issueId i) ++ ": " ++ (T.unpack $ issueSummary i)
      BH.string (T.unpack $ issueDescription i)


