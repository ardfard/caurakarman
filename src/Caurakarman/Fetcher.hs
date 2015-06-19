{-# LANGUAGE OverloadedStrings #-}

module Caurakarman.Fetcher where

import           Caurakarman.Persistence
import           Caurakarman.Type
import           Control.Monad (liftM)
import           Control.Concurrent.ParallelIO (parallel_)
import qualified Data.ByteString.Lazy.Char8 as C8
import           Data.List (isInfixOf)
import           Network.HTTP.Conduit
import           Text.HandsomeSoup hiding (openUrl)
import           Text.Read (readMaybe)
import           Text.Regex.Posix
import qualified Data.Text as T
import           Text.XML.HXT.Core

url = "http://fjb.kaskus.co.id/product/557117b614088dbf248b458c/ps4-stuff-trade-room-wts-wtb-barter--ori-only-----part-4"

parseNumPage :: String -> Maybe Int
parseNumPage text = 
    let  parsed = text =~ ("Page ([0-9]+) of ([0-9]+)"::String) :: [[String]]
      in case parsed of 
            ((_:_:z:_):_) -> readMaybe z
            _ -> Nothing

extractNumPage :: IOSArrow XmlTree (Maybe Int)
extractNumPage = css ".footer li.page-count" /> getText >>> arr parseNumPage

extractPost = css ".postlist"

extractPostLink = css ".permalink > a" ! "href" >>. filter (\x -> not $ "post_reply" `isInfixOf` x)

extractEntry = xshow $ css ".entry"  >>> indentDoc

fetchUrl :: String -> IO String
fetchUrl url = liftM C8.unpack $ simpleHttp url

processUrl :: String -> IO ()
processUrl u = do 
  contents <- fetchUrl u 
  let page = parseHtml contents
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
  parallel_ $ map processUrl (map (\x-> url ++ "/" ++ show x) [1..n])



fetchNumPage :: IO (Maybe Int)
fetchNumPage = do 
        contents <- fetchUrl url
        let page = parseHtml contents
        (numPage:_) <- runX . script  $ page
        return numPage
    where script page = page >>> extractNumPage