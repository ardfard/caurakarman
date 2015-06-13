import Text.Regex.Posix

parseNumPage :: String -> Maybe String
parseNumPage text = 
    let  parsed = text =~ "Page ([0-9]+) of ([0-9]+)" :: [[String]]
      in case parsed of 
            ((x:y:z:_):_) -> Just $ z 
            _ -> Just (show parsed)

main = print $ parseNumPage "Page 1 of 30"