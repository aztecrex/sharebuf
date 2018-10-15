module CRDT where
---

import Data.Text (Text)
import qualified Data.Text as T (singleton, concat)
import Data.Vector as V

data Cell where
    Start :: Cell
    End :: Cell
    Atom :: Rational -> Char -> Cell
    deriving (Eq, Show)

uid :: Cell -> Rational
uid Start = 0
uid End = 1
uid (Atom p _) = p

text :: Cell -> Text
text Start = ""
text End = ""
text (Atom _ v) = T.singleton v

makeCell :: Cell -> Cell -> Char -> Cell
makeCell before after c = Atom ((uid before + uid after) / 2) c

firstc :: Vector Cell -> Cell
firstc cs = if V.null cs then End else V.head cs

lastc :: Vector Cell -> Cell
lastc cs = if V.null cs then Start else V.last cs

stitch :: Vector Cell -> Vector Cell -> Char -> (Vector Cell, Cell)
stitch left right new =
    let newc = makeCell (lastc left) (firstc right) new
    in (left <> V.singleton newc <> right, newc)

type Buffer = V.Vector Cell

insert :: Buffer -> Int -> Char -> (Buffer, Cell)
insert cs i c = stitch (V.take i cs) (V.drop i cs) c

insert' :: Buffer -> Int -> Char -> Buffer
insert' cs i c = fst $ insert cs i c

emit :: Buffer -> Text
emit cs = T.concat . toList . fmap text $ cs


