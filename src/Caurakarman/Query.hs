{-# LANGUAGE OverloadedStrings #-}

module Caurakarman.Query where

import           Caurakarman.Persistence (withBH', index, postMappingName)
import           Caurakarman.Type
import           Data.Aeson
import qualified Data.Text as T
import           Database.Bloodhound
import           Network.HTTP.Conduit (responseBody)


searchPostBody :: T.Text -> IO (Either String [Hit Post])
searchPostBody phrase = 
  do 
    let query = QueryMatchQuery $ mkMatchQuery (FieldName "body") (QueryString phrase)
        search = mkSearch (Just query) Nothing
    reply <- withBH' $ searchByType index postMappingName search
    let result = eitherDecode (responseBody reply) :: Either String (SearchResult Post)
    return $ fmap (hits . searchHits) result 