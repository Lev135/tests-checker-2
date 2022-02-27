module Utils (
    maybeToEither, guardEither, concatMapM, allEq, map2, concatM,
    Info (..), WrapInfo (..),
    unionWithA, mapAdjacent
) where

import Data.List (group, nub, sort)
import Data.Maybe (catMaybes)
import Control.Monad ((>=>))
import qualified Data.Map as M
import qualified Data.Map.Merge.Strict as M
import Data.Function (on)

-- | Given a 'Maybe', convert it to an 'Either', providing a suitable
--   value for the 'Left' should the value be 'Nothing'.
--
-- > \a b -> maybeToEither a (Just b) == Right b
-- > \a -> maybeToEither a Nothing == Left a
maybeToEither :: a -> Maybe b -> Either a b
maybeToEither a (Just b) = Right b
maybeToEither a Nothing = Left a

guardEither :: e -> Bool -> Either e ()
guardEither _ True  = Right ()
guardEither e False = Left  e


-- | A version of 'concatMap' that works with a monadic predicate.
concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM op = foldr f (pure [])
    where f x xs = do x <- op x; if null x then xs else do xs <- xs; pure $ x++xs

-- |Compose a list of monadic actions into one action.  Composes using
-- ('>=>') - that is, the output of each action is fed to the input of
-- the one after it in the list.
concatM :: (Monad m) => [a -> m a] -> (a -> m a)
concatM = foldr (>=>) return

-- | 
-- allEq [Just 1, Nothing, Just 1] == Just Just 1
-- allEq [Nothing, Nothing] == Just Nothing
-- allEq [Just 1, Just 2] == Nothing 
allEq :: Eq a => [Maybe a] -> Either [a] (Maybe a)
allEq = h . nub . catMaybes
    where
        h []  = Right Nothing
        h [x] = Right $ Just x
        h xs  = Left xs

-- | Just a map, if sizes of lists are equal
map2 :: (a -> b -> c) -> [a] -> [b] -> Maybe [c]
map2 _ [] []             = Just []
map2 g (a : as) (b : bs) = (g a b :) <$> map2 g as bs
map2 _ _ _               = Nothing

data Info i a = Info { iInfo :: i, iVal :: a}

instance Show a => Show (Info i a) where
    show = show . iVal
instance Eq a => Eq (Info i a) where
    (==) = (==) `on` iVal
instance Functor (Info i) where
    fmap f (Info i a) = Info i $ f a

($$) :: (a -> b) -> Info i a -> b
f $$ pa = f $ iVal pa

type WrapInfo i t = Info i (t i)


-- | unite Maps with side effects
unionWithA :: (Ord k, Applicative m) => (a -> a -> m a) -> M.Map k a -> M.Map k a -> m (M.Map k a)
unionWithA g = M.mergeA hMiss hMiss hMatched
    where
        hMiss       = M.mapMissing $ const id
        hMatched    = M.zipWithAMatched $ const g


-- | This function combines every pair of neighbour elements
-- in a list with a certain function.
mapAdjacent :: (a -> a -> b) -> [a] -> [b]
mapAdjacent f xs = zipWith f xs (tail xs)
