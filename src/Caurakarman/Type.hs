{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Caurakarman.Type where

import           Data.Aeson
import qualified Data.Text as T
import           GHC.Generics

data Post = Post 
    {
        uri :: T.Text
    ,   body :: T.Text
    }
  deriving (Eq, Show, Generic)

instance  ToJSON Post
instance  FromJSON Post

data Success = Ok deriving (Show)
data Created = Created { created :: Bool } deriving (Generic, Show)

instance  ToJSON  Created
instance  FromJSON  Created

data PostPersistanceError = GeneralError | DocumentNotCreated deriving (Show)
