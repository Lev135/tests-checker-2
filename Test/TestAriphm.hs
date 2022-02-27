{-# LANGUAGE LambdaCase #-}
module Test.TestAriphm ()
    where

import Text.Parsec
    ( char, digit, endOfLine, oneOf, between, eof, many1,
      getPosition,
      parse,
      ParseError,
      SourcePos,
      Parsec,
      char,
      between,
      digit,
      letter,
      many1,
      Parsec,
      (<?>),
      string,
      ParseError,
      try )
import Control.Applicative ((<|>), Alternative (many))
import Text.Parsec.Char ( spaces )
import Control.Monad.Combinators.Expr ( makeExprParser, Operator(InfixL, InfixR) )
import Control.Monad (forM_, forM)
import Utils (concatMapM)
import Data.Foldable (asum)

import Ariphm


type Parser = Parsec String ()

betweenCh :: Char -> Char -> Parser a -> Parser a
betweenCh a b = between (char a) (char b)

lineSpaces :: Parser [Char]
lineSpaces = many $ oneOf " \t"

lineSpaces1 :: Parser [Char]
lineSpaces1 = many1 $ oneOf " \t"

vSpace :: Parser Char
vSpace = endOfLine

numberP :: Parser Int
numberP = read <$> many1 digit

parseAll :: Parser a -> String -> Either ParseError  a
parseAll p = parse (p <* eof) ""

type PosSegm = (SourcePos, SourcePos)

makePos :: SourcePos -> PosSegm
makePos p = (p, p)

data Pos t = Pos {pPos :: [PosSegm], pVal :: t}

instance Functor Pos where
    fmap f (Pos p a) = Pos p $ f a

instance Eq t => Eq (Pos t) where
    p == q = pVal p == pVal q

instance Show t => Show (Pos t) where
    show = show . pVal

withPosP :: Parser t -> Parser (Pos t)
withPosP p = do
    fPos <- getPosition
    x <- p
    lPos <- getPosition
    return $ Pos [(fPos, lPos)] x



newtype VarT = VarT String
    deriving Eq
instance Show VarT where
    show (VarT s) = s

type SExpr = Expr VarT

exprP :: Parser SExpr
exprP = makeExprParser (termP <* lineSpaces) operators <* lineSpaces
        <?> "expression"

termP :: Parser SExpr
termP = between (char '(') (char ')') exprP
    <|> EConst <$> numberP
    <|> EVar . VarT <$> many1 letter
    <?> "term"

operators :: [[Operator Parser SExpr]]
operators = [
        [ InfixR $ EBinOp OpPow <$ char '^' <* lineSpaces
        ],
        [ InfixL $ EBinOp OpProd <$ char '*' <* lineSpaces
        , InfixL $ EBinOp OpDiv  <$ char '/' <* lineSpaces
        ],
        [ InfixL $ EBinOp OpSum  <$ char '+' <* lineSpaces
        , InfixL $ EBinOp OpDiff <$ char '-' <* lineSpaces
        ]
    ]

type SCond = SimpleCond VarT

condP :: Parser SCond
condP =  do
    e <- exprP <* lineSpaces
    rel <- relP <* lineSpaces
    SimpleCond rel e <$> exprP

relP :: Parser Rel
relP = asum $ map h [
        ("<=", RelLeq),
        (">=", RelGeq),
        ("<",  RelLess),
        (">",  RelGreater),
        ("=" , RelEq),
        ("<>", RelNeq)
    ]
    where
        h :: (String, Rel) -> Parser Rel
        h (chars, rel) = rel <$ try (string chars)

-- TESTS

parseCond :: String -> Either ParseError SCond
parseCond = parseAll condP

conds :: [Either ParseError SCond]
conds = parseCond <$> [
        "a <= 2",
        "3 <= b",
        -- "a <= 3 - b",
        "a <= b - 3",
        "a = b + 5",
        "a + 5 = b",
        "a + 2 = b + 3",
        "a - 3 <= 5 + b"
    ]
smplConds :: [OrdCond VarT]
smplConds = concatMap makeOrdCond cs
    where
        Right cs = sequence conds
main :: IO ()
main =
    forM_ conds $ \case
        Right cond -> print $ makeOrdCond cond
        Left e -> print e

testProve :: [String] -> String -> IO ()
testProve boundsS condS = do
    bounds <- concatMapM h boundsS
    cond   <- h condS
    mapM_ print bounds
    print cond
    case mapM (tryProve bounds) cond of
        Just _  -> print "Proved"
        Nothing -> print "Not proved"
    where
        h :: String -> IO [OrdCond VarT]
        h str = case parseAll condP str of
            Left  e -> fail $ unlines [str, show e]
            Right c -> case makeOrdCond c of
                [] -> fail $ unlines [str, show c, "Not a simple cond"]
                oc -> return oc
