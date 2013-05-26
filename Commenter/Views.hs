{-# LANGUAGE OverloadedStrings #-}

module Commenter.Views where

import           Prelude hiding (div, span, head, id)
import           LBH.MP
import           LBH.Utils

import           Control.Monad
import           Data.Maybe
import           Data.Ord
import           Data.List hiding (head)
import qualified Data.Bson as B
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Monoid (mempty)
import           Data.Time
import           Hails.Web hiding (body)
import           Hails.HttpServer.Types
import           Text.Blaze.Html5 hiding (Tag, map)
import           Text.Blaze.Html5.Attributes hiding ( label, form, span
                                                    , title, style )
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Html.Renderer.Utf8
import qualified Text.Blaze.Html.Renderer.String as SR

--import		 Text.XHtml.Transitional
--import           LBH.ActiveCode ( extractActieCodeBlocks -- not a typo
                                --, activeCodeToInactiveBlocks ) 
import		 LBH.Views
import		 Commenter.Models

showPage :: [Comment] -> UserName -> B.ObjectId -> Html
showPage comments user postId = do
  createComment user postId Nothing  -- form for creating a new comment
  indexComments comments $ Just user   -- list all the existing comments

indexComments :: [Comment] -> Maybe UserName -> Html
indexComments coms muser = do
  let comments = sortBy (comparing (B.timestamp . fromJust . commentId)) coms
  div ! name "commentList" $ do
    forM_ comments $ \comment -> do
      case (commentInReplyTo comment) of
        Nothing -> do
          li $ showComment comment muser
          showAllReplies comment comments muser
        Just reply -> ""

createComment :: UserName -> B.ObjectId -> Maybe B.ObjectId -> Html
createComment username postId mparent = do
  script ! src "/static/js/comments.js" $ ""
  form ! action "/comments" ! method "POST" $ do
    input ! type_ "hidden" ! name "author" ! value (toValue username)
    input ! type_ "hidden" ! name "post" ! value (toValue $ show postId)
    case mparent of
      Just parent -> do
        input ! type_ "hidden" ! name "parent" ! value (toValue $ show parent)
      Nothing -> ""

showComment :: Comment -> Maybe UserName -> Html
showComment comment muser = do
  p $ do
    b $ toHtml $ commentAuthor comment
  p $ toHtml $ show $ B.timestamp $ fromJust $ commentId comment
  p $ toHtml $ commentText comment
  case muser of
    Just user -> do
      p $ "Reply to this comment:"
      createComment user (commentAssocPost comment) $ commentId comment
    Nothing -> ""

showAllReplies :: Comment -> [Comment] -> Maybe UserName -> Html
showAllReplies comment allComments muser = do
  let id = commentId comment
  --let comments = sortBy (comparing commentAssocPost) coms
  forM_ allComments $ \c -> do
    if ((commentInReplyTo c) == id)
      then do
        li $ showComment c muser
        showAllReplies c allComments muser
      else "" -- do nothing.

respondHtml ctitle content = okHtml $ renderHtml $ docTypeHtml $ do
  head $ do 
    title ctitle
    stylesheet "/static/css/bootstrap.css"
    stylesheet "/static/css/application.css"
  body $ do
    script ! src "/static/js/jquery.js" $ ""
    script ! src "/static/js/bootstrap.js" $ ""
    content

