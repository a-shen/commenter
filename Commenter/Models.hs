{-# LANGUAGE OverloadedStrings
           , DeriveGeneric  #-}
module Commenter.Models ( Comment(..) ) where

import           Prelude hiding (lookup)

import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as S8
import		     Data.Aeson

import           GHC.Generics

import           Hails.Data.Hson
import           Hails.Web
import           Hails.Database
import           Hails.Database.Structured

import		     LBH.MP

data Comment = Comment
    { commentId        :: Maybe ObjectId
    , commentAuthor    :: UserName  -- author
    , commentAssocPost :: ObjectId -- what post it belongs to
    , commentText :: String -- the comment body text
    , commentInReplyTo :: Maybe ObjectId  -- comment it's in reply to
    } deriving Show

instance ToJSON Comment where
  toJSON (Comment i a p t mp) =  -- id, author, post, text, parent
    object [ "_id"    .= (show $ fromJust i)
           , "author" .= a
           , "post"   .= (show p)
           , "text"   .= t
           , "parent" .= case mp of
                           Just p -> show p
                           _ -> ""
           ]

instance DCRecord Comment where
  fromDocument doc = do
    let cid = lookupObjId "_id" doc
    author <- lookup "author" doc
    text <- lookup "text" doc -- body text
    post <- lookupObjId "post" doc -- associated post
    let parent = lookupObjId "parent" doc -- the comment it's in reply to
    return Comment { commentId = cid
                   , commentAuthor = author
                   , commentAssocPost = fromJust post
                   , commentText = text
                   , commentInReplyTo = parent }

  toDocument c =
    let mparent = commentInReplyTo c
        parent = if isJust mparent
                   then [ "parent" -: fromJust mparent ]
                   else []
    let mid = commentId c
        id = if isJust mparent
               then [ "_id" -: fromJust mid ]
               else []
    in id ++
       [ "author" -: commentAuthor c
       , "post" -: commentAssocPost c
       , "text" -: commentText c ]
       ++ parent

  recordCollection _ = "comments"

lookupObjId :: Monad m => FieldName -> HsonDocument -> m ObjectId
lookupObjId n d = case lookup n d of
    Just i -> return (i :: ObjectId)
    _ -> case do { s <- lookup n d; maybeRead s } of
          Just i -> return i
          _ -> fail $ "lookupObjId: cannot extract id from " ++ show n
  where maybeRead = fmap fst . listToMaybe . reads

