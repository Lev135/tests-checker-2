module Test.TestLayout 
    where
import Control.Monad (forM_)
import Parser.Prims (makeErrorMsg, Parser)
import Parser.Layout (pLayout)
import Layout (processLayout)
import Data.Text (pack, Text)
import Data.Bifunctor (Bifunctor(first))
import Text.Megaparsec (parse, MonadParsec (eof), errorBundlePretty)

import Test.Utils ( parseAll )

tests :: [(String, String)]
tests = [ ("Половина матрицы (нижний левый треугольник). Нули нужны, чтобы в первом цикле догадаться по какому индексу итерироваться"
     , "n\na[1][1] .. a[1][0+1]\n..\na[n][1] .. a[n][0+n]\n")
    , ("Обычная прямоугольная матрица"
     , "n m\na[1][1] .. a[1][m]\n..\na[n][1] .. a[n][m]\n")
    , ("Задача на отрезки массива"
     , "N\na[1] .. a[N]\nQ\nl[1] r[1]\n..\nl[Q] r[Q]\n")
    , ("Очень странный пример. Не знаю, баг это или фича :)"
     , "{a[1] .. a[n] b[1] .. b[m]} .. {a[k] .. a[n] b[k] .. b[m]}\n")
    , ("Более логичный вариант"
     , "{a[1][1] .. a[n][1] b[1][1] .. b[m][1]} .. {a[k][k] .. a[n][k] b[k][k] .. b[m][k]}\n")
    , ("Как же не похвастаться арифметикой. Трапеция"
     , "a[1][1]..a[1][N-1+1]\n..\na[K][K]..a[K][N-K+1]\n")
    , ("Выражение и сначала и в конце"
     , "a[n + 1]..a[m + 2]\n")
    , ("И даже такую лесенку можно сделать, только приходится хитрить. Но зачем это нужно?"
     , "a[1][1-1+1] .. a[1][1-1+n]\n..\na[n][n-1+1] .. a[n][n-1+n]\n")
    , ("Индексы внутри индексов"
     , "c[1] a[1][1] .. a[1][c[1]]\n..\nc[n] a[n][1] .. a[n][c[n]]\n")
    , ("Не получается унифицировать блоки"
     , "{l[1] r[1]}..{l[M] r[N]}\n")
    , ("Не получается унифицировать циклы"
     , "{a[1][1] .. a[1][K]}..{a[N][1] .. a[M][K]}\n")
    , ("Блоки разных типов (слева цикл, справа переменная)"
     , "{a[1][1] .. a[1][K]}..{a[N][1]}\n")
    , ("Пробелы"
     , "{a[1][1]..a[1][K]}..{a[N][1] .. a[N][K]}\n")
    , ("Одинаковые начало и конец цикла"
     , "a[1]..a[1]\n")
    , ("Разные пробелы перед и после .."
     , "a[1] ..a[n]\n"
     )
    , ("", "{a[1+1] a[1+k]}..{a[1+n] a[1 + n]}\n")
    , ("Неравное количество индексов в различных вхождениях переменных"
     , "{a[1] a[2] a}\n"
     )
    ]

main :: IO ()
main = do
    forM_ tests $ uncurry helper

helper :: String -> String -> IO ()
helper msg inp = do
    putStrLn msg
    putStrLn "input data"
    mapM_ (\(i, s) -> putStrLn $ show i ++ ".\t" ++ s) $ zip [1..] inpLines
    putStrLn "output data"
    let outp = readLayout $ pack inp
    case outp of
        Left  e      -> putStrLn e
        Right descrs -> putStrLn $ concatMap show descrs
    putStrLn "-------------------\n"
    where
        inpLines = lines inp
        readLayout s = do
            lex <- parseAll (pLayout $ return ()) s
            first (makeErrorMsg inpLines) $ mapM processLayout lex

