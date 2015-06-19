{-# LANGUAGE OverloadedStrings #-}

module Caurakarman.Persistence where

import           Caurakarman.Type
import           Control.Lens ((^.))
import           Data.Aeson
import qualified Data.Aeson.Lens as AesonL
import qualified Data.Text as T
import           Database.Bloodhound
import           Network.HTTP.Conduit
import           Network.HTTP.Client (defaultManagerSettings)

elasticsServer :: Server
elasticsServer = (Server "http://localhost:9200")

index :: IndexName
index = (IndexName "caurakarman")

withBH' = withBH defaultManagerSettings elasticsServer

data PostMapping = PostMapping deriving (Eq, Show)
instance ToJSON PostMapping where
  toJSON PostMapping = 
    object ["post" .=
      object ["properties" .= 
        object ["body" .= 
          object ["type" .= ("string" :: T.Text)]]]]

postMappingName :: MappingName
postMappingName = MappingName "post"

savePost :: Post -> IO (Either PostPersistanceError Success)
savePost post = do
  let splitUri = (T.splitOn "/") . uri $ post
      id_ = splitUri !! 2
  print id_
  reply <- withBH' $ indexDocument index postMappingName post (DocId id_)
  let json = (decode $ responseBody reply :: Maybe Value)
      mbCreated = json ^. AesonL.key "created" . AesonL.asBool
  case mbCreated of 
    Just True -> return $ Right Ok 
    Just False -> return $ Left DocumentNotCreated
    _   -> return $ Left GeneralError

