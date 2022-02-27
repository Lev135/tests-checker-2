module Test where

import Parser.Prims (Parser)
import Data.Text (Text)
import Text.Megaparsec (parse, MonadParsec (eof), errorBundlePretty)
parseAll :: Parser a -> Text -> Either String a
parseAll p s = case parse (p <* eof) "" s of
    Left  e -> Left $ errorBundlePretty e
    Right a -> return a

