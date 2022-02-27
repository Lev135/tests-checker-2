{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
module Ariphm (
    Op(..), Expr (..), Rel(..), Cond(..), SimpleCond(..),
    EvalError(..), eval,
    OrdCond(..), makeOrdCond,
    tryProve
) where

import Utils ( Info(..) )

import Data.Function ( on )
import Data.Foldable ( asum )
import Data.List ( delete, intercalate )
import Data.Maybe ( isNothing, maybeToList ) 

import Control.Applicative ( (<|>) )
import Control.Monad ( guard )

data Op = OpSum | OpDiff | OpDiv | OpProd | OpPow
    deriving Eq

data Expr v
    = EVar     v
    | EConst   Int
    | EBinOp   Op (Expr v) (Expr v)
    deriving Eq

data Rel =  RelLess | RelLeq | RelGreater | RelGeq | RelEq | RelNeq
    deriving Eq

data SimpleCond v = SimpleCond Rel (Expr v) (Expr v)
    deriving Eq

data Cond v
    = CSimples [SimpleCond v]
    | CAnd     (Cond v) (Cond v)
    | COr      (Cond v) (Cond v)
    | CNot     (Cond v)
    deriving Eq

-- Evaluation

opFunc :: Integral a => Op -> a -> a -> a
opFunc OpSum  = (+)
opFunc OpDiff = (-)
opFunc OpProd = (*)
opFunc OpDiv  = div
opFunc OpPow  = (^)

data EvalError = EvalError

eval :: (v -> Maybe Int) -> Expr v -> Either EvalError Int
eval varValue e = case e of
    EVar v -> case varValue v of
        Just n -> Right n
        Nothing -> Left EvalError
    EConst n -> return n
    EBinOp op p p' -> do
        n  <- ev p
        n' <- ev p'
        return $ opFunc op n n'
    where
        ev = eval varValue

-- Ordering of varriables and constants

-- | OrdCond v n v'  <=>  v + n <= v'
data OrdCond v = OrdCond (Maybe v) Int (Maybe v)
    deriving Eq

makeOrdCond :: SimpleCond v -> [OrdCond v]
makeOrdCond (SimpleCond rel e e') = case rel of
        RelLeq -> do
            (mv , n ) <- maybeToList $ simplifyAriphm e
            (mv', n') <- maybeToList $ simplifyAriphm e'
            return $ OrdCond mv (n - n') mv'
        RelEq      -> makeOrdCond (SimpleCond RelLeq e e') <> makeOrdCond (SimpleCond RelLeq e' e)
        RelLess    -> makeOrdCond (SimpleCond RelLeq (plusOne e) e')
        RelGreater -> makeOrdCond (SimpleCond RelLess e' e)
        RelGeq     -> makeOrdCond (SimpleCond RelLeq e' e)
        RelNeq     -> [] -- No way to express alternative now
    where plusOne = EBinOp OpSum (EConst 1)

simplifyAriphm :: Expr v -> Maybe (Maybe v, Int)
simplifyAriphm e = case e of
    EVar   v -> return (Just  v, 0)
    EConst n -> return (Nothing, n)
    EBinOp OpSum ex' ex'' -> do
        (mv' , n' ) <- simplifyAriphm ex'
        (mv'', n'') <- simplifyAriphm ex''
        guard $ isNothing mv' || isNothing mv''
        return (mv' <|> mv'', n' + n'')
    EBinOp OpDiff ex' ex'' -> do
        (mv' , n' ) <- simplifyAriphm ex'
        (mv'', n'') <- simplifyAriphm ex''
        guard $ isNothing mv''
        return (mv', n' - n'')
    EBinOp {} -> Nothing

tryProve :: Eq v => [OrdCond v] -> OrdCond v -> Maybe ()
tryProve bounds (OrdCond mv1 d mv2) = case (mv1, mv2) of
    (Nothing, Nothing) -> guard $ d <= 0
    (Just v1, Nothing) -> asum $ flip map bounds $ \bound ->
                                h10 (delete bound bounds) bound
    (Nothing, Just v2) -> asum $ flip map bounds $ \bound ->
                                h01 (delete bound bounds) bound
    (Just v1, Just v2) -> guard (v1 == v2 && d <= 0)
                    <|> asum (flip map bounds $ \bound ->
                                h10 (delete bound bounds) bound)
    where
        h10 bounds' (OrdCond mv1' d' mv2') = do
                guard $ mv1' == mv1
                -- mv1  + d' <= mv2'
                -- mv2' + ?  <= mv2
                -- mv1  + d  <= mv2
                tryProve bounds' (OrdCond mv2' (d - d') mv2)
        h01 bounds' (OrdCond mv1' d' mv2') = do
                guard $ mv2' == mv2
                -- mv1' + d' <= mv2
                -- mv1  + ?  <= mv1'
                -- mv1  + d  <= mv2
                tryProve bounds' (OrdCond mv1 (d - d') mv1')

-- Show and Eq instances

instance Show Op where
    show OpSum  = "+"
    show OpDiff = "-"
    show OpProd = "*"
    show OpDiv  = "/"
    show OpPow  = "^"

instance Show Rel where
    show RelLess    = "<"
    show RelGreater = ">"
    show RelLeq     = "<="
    show RelGeq     = ">="
    show RelEq      = "="
    show RelNeq     = "<>"
    

instance Show v => Show (Expr v) where
    show (EVar   v)         = show v
    show (EConst n)         = show n
    show (EBinOp op e e')   = "(" ++ show e ++ " " ++ show op ++ " " ++ show e' ++ ")"

instance Show v => Show (SimpleCond v) where
    show (SimpleCond rel e e') = show e ++ " " ++ show rel ++ " " ++ show e'

instance Show v => Show (Cond v) where
    show = bracets . \case
        (CSimples cs) -> intercalate " && " $ show <$> cs
        (CAnd  c1 c2) -> show c1 <> " && " <> show c2 
        (COr   c1 c2) -> show c1 <> " || " <> show c2
        (CNot  c    ) -> "!" ++ show c
        where bracets str = "(" ++ str ++ ")"

instance Show v => Show (OrdCond v) where
    show (OrdCond mv d mv') = h mv ++ " + " ++ show d ++ " <= " ++ h mv'
        where 
            h (Just v) = show v
            h Nothing  = show 0
