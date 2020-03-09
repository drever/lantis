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

import Model (Issue (..), Project (..), IssueE (..), IssueDescription (..), IssueDescriptionE (..), Status (..), categories)

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

instance B.ToMarkup IssueDescriptionE where
    toMarkup (IssueDescriptionE i) = BH.toHtml . (BH.textarea ! A.autofocus "autofocus" ! A.onblur "lantis.setIssueDescription(this)") . BH.string $
        (T.unpack md)
        where (Right md) = runPure $ do md <- readMarkdown def $ i
                                        writeMarkdown def md

instance B.ToMarkup IssueDescription where
    toMarkup (IssueDescription i) = do
        (BH.div ! A.id "issueDescription" ! A.onclick "lantis.editIssueDescription(this)")
            (md)
        where (Right md) = runPure $ do md <- readMarkdown def i
                                        writeHtml5 def md


instance B.ToMarkup Status where
    toMarkup = BH.toHtml . show

instance B.ToMarkup (Project, [Issue]) where
    toMarkup (p, is) = BH.html $ do
        BH.head $ do
             BH.title "lantis"
             BH.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href "../css/lantis.css"
             BH.script ! A.src "../js/jquery-2.1.4.js" $ ""
             BH.script ! A.src "../js/lantis.js" $ ""
             BH.script $ BH.toHtml $ "lantis.projectId = " ++ show (projectId p)
        BH.div ! A.id "header" $ do
             BH.body $ BH.img ! A.src "../img/lantis.png"
             BH.h1 $ BH.toHtml (projectName p)
        BH.div ! A.id "content" $ do
             controls
             mapM_ (column is) (projectStatus p)

instance B.ToMarkup IssueE where
    toMarkup (IssueE i) = do
        BH.div ! A.id (BH.toValue $ show (issueId i)) ! A.class_ "edit" $ do
             BH.button ! A.class_ "delete" ! A.onclick "lantis.hideIssue()" $ "X"
             BH.div $ do
                 BH.h2 (do
                     BH.toMarkup $ "#" ++ show (issueId i) ++ ": "
                     BH.span ! A.id "title"
                             ! (A.contenteditable $ B.toValue True)
                             ! A.onfocus "lantis.setEditModeActive(this)"
                             ! A.onblur "lantis.setIssueSummary(this);lantis.setEditModePassive(this)"
                             ! A.onkeydown "if (event.keyCode == 13) { lantis.setIssueSummary(this);lantis.setEditModePassive(this) }" $ BH.toMarkup $ T.unpack (issueSummary i))
             BH.div . BH.ul $ do
                 BH.li . BH.toMarkup $ "Created: " ++ show (issueDateSubmitted i)
                 BH.li . BH.toMarkup $ "Last updated: " ++ show (issueLastUpdate i)
             BH.div . BH.i . BH.string . show $ issueStatus i
             BH.div . (BH.select ! A.onchange "lantis.setIssueCategory(this)") $
                 sequence_ (
                      ((BH.option !? (isNothing $ issueCategory i, A.selected "")) . BH.string $ "") :
                      (map (\x -> (BH.option !? (maybe False (==x) (issueCategory i), A.selected "")) . BH.string $ show x) categories))
             BH.toHtml . IssueDescription $ issueDescription i

controls :: BH.Markup
controls =
    BH.div ! A.id "controls" $
        BH.button ! A.onclick "lantis.createIssue(lantis.projectId)" $ "New issue"

column :: [Issue] -> Status -> BH.Markup
column is s = BH.div ! A.id (BH.toValue $ show s) ! A.class_ "column" ! A.ondragover "lantis.allowDrag(event)" ! A.ondrop "lantis.drop(event)" $ do
    BH.h1 $ BH.toHtml s
    mapM_ card (filter (\x -> issueStatus x == s) is)

card :: Issue -> BH.Markup
card i = BH.div ! A.id (BH.toValue ("issue" ++ show (issueId i))) ! A.class_ "card" ! A.draggable (BH.toValue True) ! A.ondragstart "lantis.drag(event)" ! A.ondblclick "lantis.editIssue(lantis.issueIdFromCard(this))" $
    BH.toHtml $ BH.html $ do
      BH.button ! A.class_ "delete" ! A.onclick (BH.toValue $ "lantis.deleteIssue(" ++ show (issueId i) ++ ")") $ "X"
      BH.h2 $ BH.string $ "#" ++ show (issueId i) ++ ": " ++ (T.unpack $ issueSummary i)
      BH.string (T.unpack $ issueDescription i)


