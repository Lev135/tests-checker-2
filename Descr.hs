{-# LANGUAGE LiberalTypeSynonyms #-}
module Descr where
import Layout (LayoutLexItem, Var, LayoutItem, LayoutProcessError, processLayout)
import qualified Ariphm
import Utils (WrapInfo, Info (iVal))
import Data.Text ( Text )
import Data.Functor (($>))

type Cond i = Ariphm.Cond (WrapInfo i Var)

data DescrItem i
    = InputFormat   [WrapInfo i LayoutItem]
    | Bounds        [WrapInfo i Cond]
    | SubTasks      [WrapInfo i SubTask]
    | Tests         [WrapInfo i Test]
    deriving Show

data DescrLexItem i
    = InputFormatLex   [WrapInfo i LayoutLexItem]
    | BoundsLex        [WrapInfo i Cond]
    | SubTasksLex      [WrapInfo i SubTask]
    | TestsLex         [WrapInfo i Test]
    deriving Show

processDescr :: Monoid i => WrapInfo i DescrLexItem -> Either (WrapInfo i LayoutProcessError) (WrapInfo i DescrItem)
processDescr lDescr = case iVal lDescr of
  InputFormatLex inpFL -> do
    inpFs <- mapM processLayout inpFL
    let inpF = InputFormat inpFs
    return $ lDescr $> inpF
  BoundsLex   a -> return $ lDescr $> Bounds   a 
  SubTasksLex a -> return $ lDescr $> SubTasks a
  TestsLex    a -> return $ lDescr $> Tests    a

data SubTask i = SubTask {
        stNumber    :: Int,
        stName      :: Text,
        stBounds    :: [Cond i]
    }
    deriving Show

data Test i
    = File
    | Generate [Cond i]
    deriving Show
