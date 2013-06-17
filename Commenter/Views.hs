{-# LANGUAGE OverloadedStrings #-}

module Commenter.Views where

import           Prelude hiding (div, span, head, id)

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
import		 Data.String.Utils

import           Debug.Trace

import           Hails.Web hiding (body)
import           Hails.HttpServer.Types

import           Text.Blaze.Html5 hiding (Tag, map)
import           Text.Blaze.Html5.Attributes hiding ( label, form, span
                                                    , title, style )
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Html.Renderer.Utf8
import qualified Text.Blaze.Html.Renderer.String as SR
import           Text.Regex

import           LBH.MP
import           LBH.Utils
import		 LBH.Views

import		 Commenter.Models

url :: String
url = "https://www.comments.learnbyhacking.org"

showPage :: [Comment] -> UserName -> B.ObjectId -> Html
showPage comments user pid = do
  li ! id "username" $ toHtml user
  script ! src "http://ajax.googleapis.com/ajax/libs/jquery/1.7/jquery.js" $ ""
  script ! src "http://code.jquery.com/jquery-1.10.1.min.js" $ ""
  --script ! src "/static/js/comments.js" $ ""
  newComment user pid Nothing -- show form for making new comment
  indexComments comments pid user -- index all comments

indexComments :: [Comment] -> B.ObjectId -> UserName -> Html
indexComments coms pid user = trace ("comments: " ++ (show coms)) $ do
  let comments = sortBy (comparing (B.timestamp . fromJust . commentId)) coms
  ul ! id "root" $ do
    forM_ comments $ \c -> do
      trace ("processing comment: " ++ (show c)) $ do
        if (commentAssocPost c) == pid
          then trace "belongs to this post" $ case (commentInReplyTo c) of
            Nothing -> do
              trace ("creating div for: " ++ (show c)) $ do
                --let divid = toValue $ show $ fromJust $ commentId c
                --div ! class_ "comment" ! id divid $ do
                  --h6 $ "line break"
                  showComment c comments user
                  button ! class_ "reply-button" $ "Reply"
            Just reply -> ""  -- it'll be taken care of in showAllReplies
          else ""

showComment :: Comment -> [Comment] -> UserName -> Html
showComment comment allComments user = trace ("showing comment: " ++ (show comment)) $ do
  let cid = commentId comment
  let ltime = show $ utcToLocalTime (pdt) $ B.timestamp $ fromJust cid
  let divid = show $ fromJust cid
  let bqid = "text" ++ divid
  let lid = "p" ++ divid
  div ! id (toValue divid) ! class_ "comment" $ do
    h3 $ toHtml $ commentAuthor comment
    p $ toHtml $ take ((length ltime) - 3) ltime
    blockquote ! id (toValue bqid) $ toHtml $ (commentText comment)
    let parent = commentInReplyTo comment
    trace ("parent: " ++ (show parent)) $ case parent of
      Just r -> li ! id (toValue lid) $ toHtml $ show r
      Nothing -> li ! id (toValue lid) $ ""
    let author = commentAuthor comment
    if (author == user) && (author /= "Anonymous")
      then button ! class_ "edit-button" $ "Edit"
      else ""
    showAllReplies comment allComments user

newComment :: UserName -> B.ObjectId -> Maybe B.ObjectId -> Html
newComment username postId mparent = do
  let act = (url ++ "/" ++ (show postId) ++ "/comments")
  let pid = toValue $ show postId
  form ! id "commentForm" ! action (toValue act) ! method "POST" $ do
    input ! type_ "hidden" ! name "author" ! id "author" ! value (toValue username)
    input ! type_ "hidden" ! name "post" ! id "post" ! value pid
    input ! type_ "hidden" ! name "parent" ! value ""
    div $ do
      label ! for "text" $ h5 $ "Post a comment"
      textarea ! type_ "text" ! name "text" ! id "text" $ ""
    p $ input ! type_ "submit" ! value "Post"

showAllReplies :: Comment -> [Comment] -> UserName -> Html
showAllReplies comment allComments user = trace ("showing all replies for: " ++ (show comment)) $ do
  let cid = commentId comment
  forM_ allComments $ \c -> do
    if ((commentInReplyTo c) == cid)
      then trace ("found reply: " ++ (show c)) $ ul $ showComment c allComments user
      else ""

pdt :: TimeZone
pdt = TimeZone { timeZoneMinutes = -420,
                 timeZoneSummerOnly = True, 
                 timeZoneName = "PDT" }

respondHtml muser content = okHtml $ renderHtml $ docTypeHtml $ do
  head $ do
    stylesheet "/static/css/bootstrap.css"
    stylesheet "/static/css/application.css"
    script ! src "/static/js/jquery.min.js" $ ""
    script ! src "/static/js/jquery.cookie.js" $ ""
    script ! src "/static/js/bootstrap.min.js" $ ""
    script ! src "https://login.persona.org/include.js" $ ""
    script ! src "/static/js/application.js" $ ""
  body $ do
    script ! src "/static/js/jquery.js" $ ""
    script ! src "/static/js/bootstrap.js" $ ""
    content

