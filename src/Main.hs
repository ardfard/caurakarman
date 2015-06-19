{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Main where

import           Text.XML.HXT.Core

prettyPrint :: IOSArrow XmlTree XmlTree -> IO ()
prettyPrint doc = do
    res <- runX . xshow $ doc >>> indentDoc
    mapM_ putStrLn res

main :: IO ()
main = print "ok"

