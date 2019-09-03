
module Caurakarman.Types where

import           Data.Aeson
import           Network.URI
import           Caurakarman.Prelude

data Post = Post
    {
        uri :: Text
    ,   body :: Text
    }
  deriving (Eq, Show, Generic)

instance  ToJSON Post
instance  FromJSON Post
data Success = Ok deriving (Show)
newtype Created = Created { created :: Bool } deriving (Generic, Show)

instance  ToJSON  Created
instance  FromJSON  Created

data PostPersistanceError = GeneralError | DocumentNotCreated deriving (Show)

data Item = Item
          { itemName :: Text
          , itemUrl :: URI
          , itemPrice :: Integer
          } deriving Show

newtype Keyword = Keyword Text deriving Show

