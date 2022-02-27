{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE TupleSections #-}
module Parser.Descr where

import Descr
import Parser.Prims
import Control.Applicative.Combinators (choice)
import Control.Applicative ( Alternative(empty), some )
import Layout (LayoutItem, LayoutLexItem)
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Char (space1, char, alphaNumChar, string)
import Parser.Layout (pLayout, pVar)
import Utils (WrapInfo, Info)
import Parser.Ariphm (pCond)
import Text.Megaparsec.Char.Lexer (IndentOpt(IndentMany))
import Text.Megaparsec ((<?>))
import Control.Applicative ((<|>))
import Data.Functor (($>))

pDescr :: Parser (DescrLexItem PosI)
pDescr = L.nonIndented scn $ choice [
        InputFormat <$> (pKeyword "InpFormat" *> pInpFormat),
        Bounds      <$> pBounds,
        SubTasks    <$> (pKeyword "SubTasks"  *> pSubTasks),
        Tests       <$> (pKeyword "Tests"     *> pTests)
    ]

pInpFormat :: Parser  [WrapInfo PosI LayoutLexItem]
pInpFormat = L.lineFold (L.space space1 empty empty) pLayout
    
pBounds :: Parser [Info PosI (Cond PosI)]
pBounds =  L.indentBlock scn (
                pKeyword "Bounds" $> IndentMany Nothing return (withPosI $ pCond (withPosI pVar))
            )
        
pBounds' =  L.indentBlock scn x -- (L.IndentMany Nothing return (withPosI $ pCond pVar))
    where 
        x = return $ IndentMany Nothing return (withPosI pItem)

pItemList' = L.nonIndented scn (L.indentBlock scn p)
  where
    p = do
      header <- pItem
      return (L.IndentMany Nothing return pItem)

pItemList :: Parser (String, [String]) -- header and list items
pItemList = L.nonIndented scn (L.indentBlock scn p)
  where
    p = do
      header <- pItem
      return (L.IndentMany Nothing (return . (header, )) pItem)

pItem :: Parser String
pItem = lexeme (some (alphaNumChar <|> char '-')) <?> "list item"



pSubTasks = undefined

pTests = undefined
