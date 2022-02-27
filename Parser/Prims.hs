{-# LANGUAGE OverloadedStrings #-}
module Parser.Prims (
    Parser,
    sc, scn, lexeme, symbol, pKeyword, pOperator, stringBetween, pInteger,
    stringBetweenL, pIntegerL,
    sepBy2, sepBy2With, one,
    PosSegm, PosI, withPosI,
    makeErrorMsg
)   where


import Text.Megaparsec
    ( MonadParsec(notFollowedBy), Parsec,
      SourcePos(sourceColumn, sourceLine), getSourcePos, unPos,
      manyTill, oneOf )
import Text.Megaparsec.Char ( char, alphaNumChar, space1, string )
import qualified Text.Megaparsec.Char.Lexer as L

import Data.Void (Void)
import Data.Text (Text)
import Utils (Info (Info, iVal, iInfo), WrapInfo)
import Data.Maybe (catMaybes)
import Data.List (nub, sort)
import Control.Applicative ( Alternative(empty, some), (<|>) )
import Control.Monad (void, MonadPlus)

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space
    (void . some $ char ' ' <|> char '\t')
    (L.skipLineComment "//")
    empty

scn :: Parser ()
scn = L.space
    space1
    (L.skipLineComment "//")
    (L.skipBlockCommentNested "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

pKeyword :: Text -> Parser Text
pKeyword keyword = lexeme (string ("@" <> keyword) <* notFollowedBy alphaNumChar)

opSymbols :: [Char]
opSymbols = "+-*/=<>"

pOperator :: Text -> Parser Text
pOperator op = lexeme (string op) <* notFollowedBy (void $ oneOf opSymbols)

pInteger :: Parser Int
pInteger = lexeme L.decimal

stringBetween :: Char -> Parser String
stringBetween ch = lexeme $ char ch *> manyTill L.charLiteral (char ch)

pIntegerL :: Parser Int
pIntegerL = L.decimal

stringBetweenL :: Char -> Parser String
stringBetweenL ch = char ch *> manyTill L.charLiteral (char ch)

one :: Functor f => f a -> f [a]
one = ((:[]) <$>)

sepBy2 :: MonadPlus m => m a -> m sep -> m [(a, sep, a)]
sepBy2 ma msep = ma >>= scan
    where
        scan prev = do
            sep <- msep
            a   <- ma
            others <- scan a <|> pure []
            return $ (prev, sep, a) : others 

sepBy2With :: MonadPlus m => (a -> sep -> a -> b) -> m a -> m sep -> m [b]
sepBy2With g ma msep = ma >>= scan
    where
        scan prev = do
            sep <- msep
            a   <- ma
            others <- scan a <|> pure []
            return $ g prev sep a : others

type PosSegm = (SourcePos, SourcePos)

type PosI = [PosSegm]

withPosI :: Parser a -> Parser (Info PosI a)
withPosI p = do
    begin <- getSourcePos
    a <- p
    end <- getSourcePos
    return $ Info [(begin, end)] a

sLine :: SourcePos -> Int
sLine = unPos . sourceLine
sCol :: SourcePos -> Int
sCol  = unPos . sourceColumn

makeErrorMsg :: Show (e PosI) => [String] -> WrapInfo PosI e  -> String
makeErrorMsg sourceLines Info{iInfo = segms, iVal = e}
    = show e ++ "\n" ++ concatMap (\n ->
                                show (n + 1) ++ ".\t"
                            ++  sourceLines !! n ++ "\n"
                            ++  "\t"
                            ++  mask n ++ "\n") lines
    where
        lines       = nub . sort . concatMap segmLines $ segms
        segmLines (p1, p2) = [sLine p1 - 1 .. (sLine p2 - 1) `min` (length sourceLines - 1)]
        mask n = h '^' '-' segms n
        h bCh iCh segms n = makeMask bCh iCh (lineLen n) (lineSegms segms n)
        lineSegms segms n = catMaybes $ segmInLine n (lineLen n) <$> segms
        lineLen   n = length $ sourceLines !! n

segmInLine :: Int -> Int -> PosSegm -> Maybe (Int, Int)
segmInLine n len (p1, p2)
    | sLine p1 <= n' && n'  <= sLine p2
                = let l = if sLine p1 < n' then 1   else sCol p1
                      r = if sLine p2 > n' then len else sCol p2
                  in Just (l, r)
    | otherwise = Nothing
            where n' = n + 1


makeMask :: Char -> Char -> Int -> [(Int, Int)] -> [Char]
makeMask borderCh innerCh len ixs = flip map [1..len] $ \i ->
        if   any (begEnd i) ixs
        then borderCh
        else if any (containes i) ixs
             then innerCh
             else ' '
    where
        containes i (l, r) = l <= i && i < r
        begEnd    i (l, r) = l == i || i == r - 1

