{-# LANGUAGE OverloadedStrings
           , MultiParamTypeClasses
           , ScopedTypeVariables
           , DeriveDataTypeable
           , OverloadedStrings #-}
module Commenter.Policy ( CommentPolicy
                        , personaLoginEmailToUid
                        , withCommentPolicy
                        ) where

import           Prelude hiding (lookup)

import           Data.Maybe
import qualified Data.List as List
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as S8
import           Data.Typeable
import qualified Data.Set as Set
import qualified Data.List as List

import           Control.Monad

import           LIO
import           LIO.DCLabel
import           LIO.DCLabel.Core
import           LIO.DCLabel.Privs.TCB (allPrivTCB)
import           LIO.TCB

import           Hails.Data.Hson
import           Hails.Web hiding (body)
import           Hails.Database
import           Hails.Database.Structured
import           Hails.PolicyModule
import           Hails.PolicyModule.Groups
import           Hails.PolicyModule.DSL
import           Hails.HttpServer.Types
import           Hails.HttpServer.Auth
import           Hails.Database.Structured hiding (findAll, findAllP)

import           Commenter.Models
import           LBH.MP


data CommentPolicy = CommentPolicyTCB DCPriv deriving Typeable

instance PolicyModule CommentPolicy where
   initPolicyModule priv = do
     setPolicy priv $ do
       database $ do
         readers ==> anybody
         writers ==> anybody
         --admins  ==> this
         admins  ==> anybody
       collection "comments" $ do
         access $ do
           readers ==> anybody
           writers ==> anybody
         clearance $ do
           --secrecy   ==> this
           secrecy   ==> anybody
           integrity ==> anybody
         document $ \doc -> do
           --let (Just u) = fromDocument doc
           readers ==> anybody
           writers ==> anybody
           --writers ==> (T.unpack $ userName u) \/ root \/ this
     return $ CommentPolicyTCB priv
       where this = privDesc priv
             root = principal "root"

instance DCLabeledRecord CommentPolicy Comment where
  endorseInstance _ = CommentPolicyTCB noPriv

withCommentPolicy :: DBAction a -> DC a
withCommentPolicy act = withPolicyModule $
  \(CommentPolicyTCB noPrivs) -> act

