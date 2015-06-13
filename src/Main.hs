{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Main where

import qualified Data.Aeson.Lens as AesonL
import qualified Data.ByteString.Lazy.Char8 as C8
import qualified Data.Text as T
import           Control.Arrow 
import           Control.Concurrent.ParallelIO
import           Control.Lens ((^.))
import           Control.Monad 
import           Data.Aeson
import           Data.List (isInfixOf)
import           Data.List.Split (splitOn)
import           Database.Bloodhound
import           GHC.Generics
import           Network.HTTP.Conduit
import           Network.HTTP.Client (defaultManagerSettings)
import           Text.HandsomeSoup hiding (openUrl)
import           Text.Read (readMaybe)
import           Text.Regex.Posix
import           Text.XML.HXT.Core

elasticsServer :: Server
elasticsServer = (Server "http://localhost:9200")

index :: IndexName
index = (IndexName "caurakarman")

withBH' = withBH defaultManagerSettings elasticsServer

data Post = Post 
        { uri :: T.Text
        , body :: T.Text  
        }
      deriving (Eq, Show, Generic)

instance  ToJSON Post
instance  FromJSON Post

data PostMapping = PostMapping deriving (Eq, Show)
data PostPersistanceError = GeneralError | DocumentNotCreated deriving (Show)
data Success = Ok deriving (Show)
data Created = Created { created :: Bool } deriving (Generic, Show)

instance  ToJSON  Created
instance  FromJSON  Created

postMappingName :: MappingName
postMappingName = MappingName "post"

instance ToJSON PostMapping where
  toJSON PostMapping = 
    object ["post" .=
      object ["properties" .= 
        object ["body" .= 
          object ["type" .= ("string" :: T.Text)]]]]

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

openUrl :: String -> IO String
openUrl url = liftM C8.unpack $ simpleHttp url

prettyPrint :: IOSArrow XmlTree XmlTree -> IO ()
prettyPrint doc = do
    res <- runX . xshow $ doc >>> indentDoc
    mapM_ putStrLn res

parseNumPage :: String -> Maybe Int
parseNumPage text = 
    let  parsed = text =~ ("Page ([0-9]+) of ([0-9]+)"::String) :: [[String]]
      in case parsed of 
            ((x:y:z:_):_) -> readMaybe z
            _ -> Nothing

extractNumPage :: IOSArrow XmlTree (Maybe Int)
extractNumPage = css ".footer li.page-count" /> getText >>> arr parseNumPage

url = "http://fjb.kaskus.co.id/product/557117b614088dbf248b458c/ps4-stuff-trade-room-wts-wtb-barter--ori-only-----part-4"

extractPost = css ".postlist"

extractPostLink = css ".permalink > a" ! "href" >>. filter (\x -> not $ "post_reply" `isInfixOf` x)

extractEntry = xshow $ css ".entry"  >>> indentDoc

download :: String -> IO()
download u = do 
  contents <- openUrl u 
  let fileName = last $ splitOn "/" u
      page = parseHtml contents
  result <- runX  $ page >>> extractPost >>> (extractPostLink &&& extractEntry)
  mapM_ (\(uri_,body_)-> 
    let post = Post (T.pack uri_) (T.pack body_)
    in do
      saveResult <- savePost post
      case saveResult of 
        Left err -> print err
        _ -> print ""
    )
    result

fetchAll :: Int -> IO()
fetchAll n = do
  parallel_ $ map download (map (\x-> url ++ "/" ++ show x) [1..n])

searchPostBody :: T.Text -> IO (Either String [Hit Post])
searchPostBody phrase = 
  do 
    let query = QueryMatchQuery $ mkMatchQuery (FieldName "body") (QueryString phrase)
        search = mkSearch (Just query) Nothing
    reply <- withBH' $ searchByType index postMappingName search
    let result = eitherDecode (responseBody reply) :: Either String (SearchResult Post)
    return $ fmap (hits . searchHits) result 


main :: IO ()
main = do 
        contents <- openUrl url
        let page = parseHtml contents
        (numPage:_) <- runX . script  $ page
        case numPage of 
          Just n -> do 
            putStrLn "Start fetching..."
            fetchAll n
          Nothing -> putStrLn "Parse error"
    where script page = page >>> extractNumPage

