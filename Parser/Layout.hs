{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
module Parser.Layout (
    pLayout, pVar
)   where


import Text.Megaparsec ((<?>), MonadParsec (try, label, lookAhead), optional, getSourcePos, SourcePos (SourcePos))
import Parser.Prims ( withPosI, PosI, Parser, one, stringBetweenL)
import Layout (LayoutLexItem (..), Var (..), Expr)
import Utils (WrapInfo, Info (Info, iVal))
import Control.Applicative (Alternative (some, many), (<|>))
import Control.Applicative.Combinators (choice)
import Text.Megaparsec.Char (letterChar, char, string, newline)
import Data.Text (unpack, pack)
import Control.Monad (when)
import Data.Maybe (fromMaybe)
import Parser.Ariphm (pExpr)

type LayoutLexPos = WrapInfo PosI LayoutLexItem


pVar :: Parser (Var PosI)
pVar = Var <$> some letterChar <*> many pIndex
    where pIndex = char '[' *> withPosI (pExpr (withPosI pVar)) <* char ']' <?> "var index"

pLayout :: Parser () -> Parser [LayoutLexPos]
pLayout indentP = concat <$> many (pLineOrLoop indentP)

pLineOrLoop :: Parser () -> Parser [LayoutLexPos]
pLineOrLoop indentP = label "line or vertical loop" $ do
    begin <- getSourcePos
    line1 <- pLine indentP
    loopEnd begin line1 <|> return line1
    where
        loopEnd :: SourcePos -> [LayoutLexPos] -> Parser [LayoutLexPos]
        loopEnd begin line1 = try . one $ do
            mid <- getSourcePos
            let b1 = LBlockLex <$> Info [(begin, mid)] line1
            _  <- string ".." <* pNewLine indentP
            b2 <- withPosI $ LBlockLex <$> pLine indentP
            end <- getSourcePos
            return $ Info [(begin, end)] $ LLoopLex b1 "\n" b2

pLine :: Parser () -> Parser [LayoutLexPos]
pLine indentP = pTerms <> one (pNewLine indentP)

pNewLine :: Parser () -> Parser LayoutLexPos
pNewLine indentP = withPosI . label "new line" $ LStringLex "\n" <$ newline <* try indentP

pTerms :: Parser [LayoutLexPos]
pTerms = many (pSpace <|> pTermOrLoop)

pSpace :: Parser LayoutLexPos
pSpace = withPosI . label "space" $ LStringLex " " <$ string " "

pTermOrLoop :: Parser LayoutLexPos
pTermOrLoop = label "term or loop" . withPosI $ do
        term  <- pTerm
        loopEnd term <|> return (iVal term)
    where
        loopEnd term1 = do
            sep'    <- try $ pLoopSep <* string ".."
            sep''   <- pLoopSep
            when (sep' /= sep'') $
                fail $ "Loop separators are unequal: " ++ show sep' ++ " /= " ++ show sep''
            LLoopLex term1 sep' <$> pTerm


pTerm :: Parser LayoutLexPos
pTerm = withPosI . label "term" $ choice [
        LVarLex    <$> pVar,
        LStringLex <$> stringBetweenL '"',
        LStringLex <$> stringBetweenL '\'',
        LBlockLex  <$> (char '{' *> pTerms <* char '}')
    ]

pLoopSep :: Parser String
pLoopSep = label "loop sep" $ unpack <$> choice [
        string " ",
        string ",",
        string ", ",
        string "-",
        string "- ",
        string ":",
        string ": ",
        pack <$> stringBetweenL '"',
        pack <$> stringBetweenL '\''
    ] <|> return ""
