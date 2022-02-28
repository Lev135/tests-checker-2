{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE TupleSections #-}
module Parser.Descr where

import Descr
import Parser.Prims
import Control.Applicative.Combinators (choice)
import Control.Applicative ( Alternative(empty, many), some, (<|>) )
import Layout (LayoutItem, LayoutLexItem)
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Char (space1, char, alphaNumChar, string, newline)
import Parser.Layout (pLayout, pVar)
import Utils (WrapInfo, Info)
import Parser.Ariphm (pCond)
import Text.Megaparsec.Char.Lexer (IndentOpt(IndentMany))
import Text.Megaparsec ((<?>))
import Data.Functor (($>), void)

pDescr :: Parser [WrapInfo  PosI DescrLexItem]
pDescr = many pDescrItem

pDescrItem :: Parser (WrapInfo PosI DescrLexItem)
pDescrItem = withPosI $ L.nonIndented scn $ choice [
        InputFormatLex <$> pInpFormat,
        BoundsLex      <$> pBounds,
        SubTasksLex    <$> (pKeyword "SubTasks"  *> pSubTasks),
        TestsLex       <$> (pKeyword "Tests"     *> pTests)
    ]

pInpFormat :: Parser  [WrapInfo PosI LayoutLexItem]
pInpFormat = do
    scn
    ref <- L.indentLevel
    pKeyword "InpFormat" <* scn
    lvl <- L.indentGuard scn GT ref
    pLayout $ void $ L.indentGuard sc EQ lvl

pBounds :: Parser [Info PosI (Cond PosI)]
pBounds = L.indentBlock scn $
        pKeyword "Bounds" $> IndentMany Nothing return (withPosI $ pCond (withPosI pVar))

pSubTasks = undefined

pTests = undefined
