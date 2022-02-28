{-# LANGUAGE LiberalTypeSynonyms #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE NamedFieldPuns #-}

module Layout where

import Utils
    ( allEq, guardEither, map2, maybeToEither, 
      Info(Info, iVal, iInfo), WrapInfo )
import qualified Ariphm
import Ariphm hiding (Expr)
import Data.Function (on)

type VarName = String
type StringConst = String
type Expr i = Ariphm.Expr (WrapInfo i Var)

data Var i = Var {
        vName  :: VarName,
        vIndxs :: [WrapInfo i Expr]
    } deriving Eq

data Loop i = Loop {
        lBody     :: WrapInfo i LayoutItem,
        lIdxVar   :: VarName,
        lFirst    :: WrapInfo i Expr,
        lLast     :: WrapInfo i Expr,
        lSepStr   :: StringConst
    } deriving Eq

data LayoutItem i
    = LVar      (Var i)
    | LLoop     (Loop i)
    | LString   StringConst
    | LBlock    [WrapInfo i LayoutItem]
    deriving Eq

data LayoutLexItem i
    = LVarLex       (Var i)
    | LLoopLex      (WrapInfo i LayoutLexItem) String (WrapInfo i LayoutLexItem)
    | LStringLex    StringConst
    | LBlockLex     [WrapInfo i LayoutLexItem]
    deriving (Eq, Show)

data LayoutProcessError i
    = LPE_UnequalBlockTypes
    | LPE_UnequalLength
    | LPE_UnequalNames
    | LPE_UnequalSpaces
    | LPE_UnequalSeparators
    | LPE_UnequalStrings
    | LPE_UnequalOperations
    | LPE_UnifyError           [(WrapInfo i Expr, WrapInfo i Expr)]
    | LPE_IdenticalBeginEnd
    | LPE_UnknownError
    deriving Eq
    
idxVarNameByLvl :: Int -> String
idxVarNameByLvl = (map ('$':) ["i", "j", "k", "l"] !!)

processLayout :: Monoid i => WrapInfo i LayoutLexItem -> Either (WrapInfo i LayoutProcessError) (WrapInfo i LayoutItem)
processLayout = processLayoutImpl 0

processLayoutImpl :: Monoid i => Int -> WrapInfo i LayoutLexItem -> Either (WrapInfo i LayoutProcessError) (WrapInfo i LayoutItem)
processLayoutImpl lvl lex = case iVal lex of
    LVarLex    v -> return . info $ LVar v
    LStringLex s -> return . info $ LString s
    LLoopLex   b1 sep b2 -> do
        b1' <- process' b1
        b2' <- process' b2
        (b, mBounds) <- generalizePart idxVar b1' b2'
        (l, r) <- maybeToEither (info LPE_IdenticalBeginEnd) mBounds
        return . info . LLoop $ Loop b idxVarName l r sep
    LBlockLex  b -> info . LBlock <$> mapM (processLayoutImpl lvl) b 
    where
        info = Info $ iInfo lex
        process' = processLayoutImpl (lvl + 1)
        idxVarName = idxVarNameByLvl lvl
        idxVar = Var idxVarName []

generalizeVars :: Monoid i => Var i -> WrapInfo i Var -> WrapInfo i Var
    -> Either (WrapInfo i LayoutProcessError) (WrapInfo i Var, Maybe (WrapInfo i Expr, WrapInfo i Expr))
generalizeVars idxVar (Info i1 (Var name1 idxs1)) (Info i2 (Var name2 idxs2))
    = do
        guardEither (info LPE_UnequalNames) $ name1 == name2
        msIdxs <- maybeToEither (info LPE_UnequalLength) $ map2 (generalizeExpr idxVar) idxs1 idxs2
        (idxs, idxsMIs) <- unzip <$> sequence msIdxs
        mIs <- makeUnifyError $ allEq idxsMIs
        return (info $ Var name1 idxs, mIs)
    where
        info = Info $ i1 <> i2

generalizeExpr :: Monoid i => Var i -> WrapInfo i Expr -> WrapInfo i Expr
    -> Either (WrapInfo i LayoutProcessError) (WrapInfo i Expr, Maybe (WrapInfo i Expr, WrapInfo i Expr))
generalizeExpr idxVar e1 e2 = case (iVal e1, iVal e2) of
    (EVar v1, EVar v2) ->
        do
            (var, mIs) <- generalizeVars idxVar v1 v2
            return (EVar <$> info var, mIs)
        <> foundIdx
    (EConst c1, EConst c2) ->
        if c1 == c2
            then return (info $ EConst c1, Nothing)
            else foundIdx
    (EBinOp op1 a1 b1, EBinOp op2 a2 b2) ->
        do
            guardEither (info LPE_UnknownError) $ op1 == op2 && op1 `elem` [OpSum, OpDiff]
            (Info _ a, aMIs) <- generalizeExpr idxVar (Info i1 a1) (Info i2 a2)
            (Info _ b, bMIs) <- generalizeExpr idxVar (Info i1 b1) (Info i2 b2)
            mIs <- makeUnifyError $ allEq [aMIs, bMIs]
            return (info $ EBinOp op1 a b, mIs)
        <> foundIdx
    _ -> foundIdx
    where
        i1 = iInfo e1
        i2 = iInfo e2
        info = Info $ i1 <> i2
        var = info $ EVar $ info  idxVar
        foundIdx = return (var, Just (e1, e2))

generalizePart :: Monoid i => Var i -> WrapInfo i LayoutItem -> WrapInfo i LayoutItem
    -> Either (WrapInfo i LayoutProcessError) (WrapInfo i LayoutItem, Maybe (WrapInfo i Expr, WrapInfo i Expr))
generalizePart idxVar lay1 lay2 = case (iVal lay1, iVal lay2) of
    (LVar v1, LVar v2) -> do
        (v, mIs) <- generalizeVars idxVar (Info i1 v1) (Info i2 v2)
        return (LVar <$> v, mIs)
    (LLoop l1, LLoop l2) -> do
        guardEither (info LPE_UnequalSeparators) $ lSepStr l1 == lSepStr l2
        guardEither (info LPE_UnknownError) $ lIdxVar l1 == lIdxVar l2
        (body, bodyMIs) <- generalizePart idxVar (lBody l1) (lBody  l2)
        (fIdx, fIdxMIs) <- generalizeExpr idxVar (lFirst l1) (lFirst l2)
        (lIdx, lIdxMIs) <- generalizeExpr idxVar (lLast  l1) (lLast  l2)
        mIs <- makeUnifyError $ allEq [bodyMIs, fIdxMIs, lIdxMIs]
        let l = Loop body (lIdxVar l1) fIdx lIdx (lSepStr l1)
        return (info $ LLoop l, mIs)
    (LString s1, LString s2) -> do
        guardEither (info LPE_UnequalStrings) $ s1 == s2
        return (info $ LString s1, Nothing)
    (LBlock b1, LBlock b2) -> do
        msBlock   <- maybeToEither (info LPE_UnequalLength) $ map2 (generalizePart idxVar) b1 b2
        (b, bMIs) <- unzip <$> sequence msBlock
        mIs       <- makeUnifyError . allEq $ bMIs
        return (info $ LBlock b, mIs)
    _ -> Left $ info LPE_UnequalBlockTypes
    where
        i1 = iInfo lay1
        i2 = iInfo lay2
        info = Info $ i1 <> i2

makeUnifyError :: Monoid i => Either [(WrapInfo i Expr, WrapInfo i Expr)] a -> Either (WrapInfo i LayoutProcessError) a
makeUnifyError (Right val)  = Right val
makeUnifyError (Left  tmp) = Left $ Info (p1 <> p2) (LPE_UnifyError tmp)
    where
        p1 = mconcat $ map (iInfo . fst) tmp
        p2 = mconcat $ map (iInfo . snd) tmp

-- Show

instance Show (Var i) where
    show v = vName v ++ concatMap h (vIndxs v)
        where h idx = "[" ++ show idx ++ "]"

instance Show (Loop i) where
    show Loop{lBody, lIdxVar, lFirst, lLast, lSepStr}
        = "<" ++ show lBody ++ "|"++ show lSepStr ++ "|" 
            ++ lIdxVar ++ "=" ++ show lFirst ++ ".." ++ show lLast ++ ">"

instance Show (LayoutItem i) where
    show (LVar    v)  = show v
    show (LLoop   l)  = show l
    show (LString s)  = show s
    show (LBlock  b)  = "{" ++ concatMap show b ++ "}"

instance Show (LayoutProcessError i) where
    show LPE_UnequalBlockTypes       = "Block types are unequal"
    show LPE_UnequalLength           = "Block length/index count are unequal"
    show LPE_UnequalNames            = "Variable names are unequal"
    show LPE_UnequalSpaces           = "Spaces are unequal"
    show LPE_UnequalSeparators       = "Loop separators are unequal"
    show LPE_UnequalStrings          = "String constants are unequal"
    show LPE_UnequalOperations       = "Binary operations are unequal"
    show (LPE_UnifyError exprs)      = "Unable to unify these changes:"
                                            ++ concatMap ((" " ++) . show) exprs
    show LPE_IdenticalBeginEnd       = "First and last blocks of loop are identical"
    show LPE_UnknownError            = "UNKNOWN_ERROR this msg shouldn't be shown"
