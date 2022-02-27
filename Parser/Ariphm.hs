{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
module Parser.Ariphm (
    pExpr, pCond
) where

import Text.Megaparsec ((<?>), MonadParsec (label, try))
import Parser.Prims ( lexeme, pInteger, withPosI, Parser, PosI, pOperator, sepBy2With, symbol, sepBy2 )
import Layout (LayoutLexItem (LVarLex, LStringLex), Var (Var))
import Control.Applicative (Alternative (some, many), (<|>))
import Control.Applicative.Combinators (choice, sepBy1)
import Text.Megaparsec.Char (letterChar, char)
import Control.Monad.Combinators.Expr (Operator (InfixR, InfixL, Prefix), makeExprParser)
import Ariphm ( Expr(..), Op(..), Cond (..), SimpleCond (SimpleCond), Rel (RelLess, RelEq, RelLeq, RelGreater, RelGeq) )
import Data.Text (Text)
import Utils (mapAdjacent)
import Data.Bifunctor (Bifunctor(first))

-- | Парсит арифметическое выражение по заданному парсеру переменных
pExpr :: Parser v -> Parser (Expr v)
pExpr pVar = lexeme . label "expression" $ makeExprParser (lexeme (pTerm pVar)) operators

pTerm :: Parser v -> Parser (Expr v)
pTerm pVar = char '(' *> pExpr pVar <* char ')'
    <|> EConst <$> pInteger
    <|> EVar <$> pVar
    <?> "term"

operators :: [[Operator Parser (Expr v)]]
operators = [
        [ InfixR $ EBinOp OpPow <$ pOperator "^"
        ],
        [ infixLOp OpProd "*"
        , infixLOp OpDiv  "/"
        ],
        [ infixLOp OpSum  "+"
        , infixLOp OpDiff "-"
        ]
    ]
    where
        infixLOp op ch = InfixL $ EBinOp op <$ pOperator ch

-- | Парсит условие по заданному парсеру переменных
pCond :: Parser v -> Parser (Cond v)
pCond pVar = lexeme . label "condition" $ makeExprParser (lexeme (pCondTerm pVar)) condOperators

pCondTerm :: Parser v -> Parser (Cond v)
pCondTerm pVar = char '(' *> pCond pVar <* char ')'
            <|> CSimples <$> (try (pLeqSequence pVar) <|> pGeqSequence pVar)

condOperators :: [[Operator Parser (Cond v)]]
condOperators = [
        [ Prefix $ CNot <$ pOperator "!"
        ],
        [ InfixL $ CAnd <$ pOperator "&&"
        ],
        [ InfixL $ COr  <$ pOperator "||"
        ]
    ]

pLeqSequence :: Parser v -> Parser [SimpleCond v]
pLeqSequence = pRelSequence [(RelLeq, "<="), (RelLess, "<")]

pGeqSequence :: Parser v -> Parser [SimpleCond v]
pGeqSequence = pRelSequence [(RelGeq, ">="), (RelGreater, ">")]

pRelSequence :: [(Rel, Text)] -> Parser v -> Parser [SimpleCond v]
pRelSequence rels pVar = h <$> sepBy2 pEquals (pRel rels)
    where 
        pEquals = first concat . unzip <$> pEqualSeq `sepBy1` symbol ","
        pEqualSeq = do
            exprs <- pExpr pVar `sepBy1` symbol "="
            let eqs = mapAdjacent (SimpleCond RelEq) exprs
            return (eqs, head exprs)
        h triples = 
            let ((fstEqs, _), _, _)  = head triples
            in fstEqs <> foldMap (\((_, ls), rel, (rEqs, rs)) -> rEqs <> makeConds ls rel rs) triples

makeConds :: [Expr v] -> Rel -> [Expr v] -> [SimpleCond v]
makeConds ls rel rs = [SimpleCond rel l r | l <- ls, r <- rs]

pRel :: (Foldable f, Functor f) => f (a, Text) -> Parser a
pRel rels = choice $ uncurry h <$> rels
    where h op smbs = op <$ pOperator smbs
