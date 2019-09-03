{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

-- import           Caurakarman.Fetcher.Tokopedia as Toped
-- import qualified Control.Concurrent.STM as S
-- import           System.Environment     (getArgs)
import Caurakarman.Prelude
import           Caurakarman.Scraper.Toped
import           Caurakarman.Types
import qualified Data.Text                 as Text
import           Data.Text.IO              (putStrLn)
import           Text.Regex.PCRE
import Data.ByteString.Char8 (pack)

main :: IO ()
main = do
  Right items <- scrapeToped $ Keyword "gtx 1080 ti"

  print $ filter invalidator items
  where
    invalidator = not . (=~ ("(bukan|not|980|1070|1060)" :: ByteString )) . ( toS :: Text -> ByteString) . Text.toLower . itemName
