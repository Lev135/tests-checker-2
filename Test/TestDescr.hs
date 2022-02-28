module Test.TestDescr 
    where
import Control.Monad (forM_)
import Parser.Prims (makeErrorMsg, Parser)
import Parser.Layout (pLayout)
import Layout (processLayout)
import Data.Text (pack, Text)
import Data.Bifunctor (Bifunctor(first))
import Text.Megaparsec (parse, MonadParsec (eof), errorBundlePretty)

import Test.Utils ( parseAll )
import Parser.Descr (pDescr)
import Descr (processDescr)

tests :: [(String, String)]
tests 
  = [ (
        "Ужасное сообщение об ошибке (на самом деле не хватает \\n в конце" -- TODO: Fix it
        , "@InpFormat\n  n\n  a[1][1] .. a[1][n]\n  ..\n  a[n][1] .. a[n][n]"
    ), (
        "Та же матрица, но с исправленной ошибкой"
        ,"@InpFormat\n  n\n  a[1][1] .. a[1][n]\n  ..\n  a[n][1] .. a[n][n]\n"
    ), (
        "Та же матрица, но с исправленной ошибкой"
        ,"@InpFormat\n  n\n  a[1][1] .. a[1][n]\n  ..\n  a[n][1] .. a[n][n]\n"
        ++ "@Bounds\n  1 <= n <= 10^6\n  0-10^9 <= a[i][j] <= 10^9\n"
    )
    ]

main :: IO ()
main = do
    forM_ tests $ uncurry helper

helper :: String -> String -> IO ()
helper msg inp = do
    putStrLn msg
    putStrLn "input data"
    mapM_ (\(i, s) -> putStrLn $ show i ++ " |" ++ s) $ zip [1..] inpLines
    putStrLn "output data"
    let outp = readLayout $ pack inp
    case outp of
        Left  e      -> putStrLn e
        Right descrs -> putStrLn $ unlines $ show <$> descrs
    putStrLn "-------------------\n"
    where
        inpLines = lines inp
        readLayout s = do
            lex <- parseAll pDescr s
            first (makeErrorMsg inpLines) $ mapM processDescr lex

