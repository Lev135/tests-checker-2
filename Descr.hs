{-# LANGUAGE LiberalTypeSynonyms #-}
module Descr where
import Layout (LayoutLexItem, Var)
import qualified Ariphm
import Utils (WrapInfo)
import Data.Text ( Text )

type Cond i = Ariphm.Cond (WrapInfo i Var)

data DescrLexItem i
    = InputFormat   [WrapInfo i LayoutLexItem]
    | Bounds        [WrapInfo i Cond]
    | SubTasks      [WrapInfo i SubTask]
    | Tests         [WrapInfo i Test]
    deriving Show
    
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
