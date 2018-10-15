module CRDT where
---

import Data.Text (Text)
import qualified Data.Text as T (singleton, concat)
import Data.Vector as V

data Cell where
    Cell :: Rational -> Char -> Cell
    deriving (Eq, Show)

start :: Rational
start  = 0

end :: Rational
end = 1

uid :: Cell -> Rational
uid (Cell p _) = p

text :: Cell -> Text
text (Cell _ v) = T.singleton v

makeCell :: Rational -> Rational -> Char -> Cell
makeCell before after c = Cell ((before + after) / 2) c

firstc :: Vector Cell -> Rational
firstc cs = if V.null cs then end else uid (V.head cs)

lastc :: Vector Cell -> Rational
lastc cs = if V.null cs then start else uid (V.last cs)

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


