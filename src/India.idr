module India

import System
import System.File

export
parseFileWith :
  (parse : String -> Either String a) ->
  (filename: String) ->
  IO $ Either String a
parseFileWith parse filename = do
  Right text <- readFile filename
    | Left err => pure $ Left $ show err
  pure $ parse text

