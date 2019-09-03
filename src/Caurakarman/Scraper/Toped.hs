module Caurakarman.Scraper.Toped (scrapeToped) where

import           Caurakarman.Prelude
import           Caurakarman.Types
import           Control.Lens
import           Data.Aeson          (Value)
import           Data.Aeson.Lens
import qualified Data.Text           as T
import           Network.HTTP.Client (responseBody)
import           Network.HTTP.Simple (httpJSON, parseRequest)
import           Network.URI         (nullURI, parseURI)


scrapeToped :: Keyword -> IO (Either Text [Item])
scrapeToped (Keyword keyword) = do
    request <- parseRequest $ toS url
    body <- (responseBody <$> httpJSON request) :: IO Value
    case products body of
        Nothing -> return $ Left "error parsing"
        Just product ->
            return . Right . toList $ fmap productToItem product
  where
    url =  mconcat
            [ "https://ace.tokopedia.com/search/product/v3?st=product&q="
            , encoded
            ,"&scheme=https&device=desktop&source=search&ob=3&rows=50&catalog_rows=1&pmin=4000000&page=1&start=0"
            ]

    products = preview $ key "data" . key "products" . _Array
    productToItem json =
        Item
            (json ^. key "name" . _String)
            ( fromMaybe nullURI . parseURI . toS $ json ^. key "url". _String)
            (fromMaybe 0 $ json ^? key "price_int" . _Integer )
    encoded = T.intercalate "+" . T.words $ keyword
