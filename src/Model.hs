{-# LANGUAGE ViewPatterns #-}
module Model where

import Prelude hiding (lookup, floor, abs)
import Data.Functor
import Control.Monad
-- import Data.Monoid
import Data.Foldable
import qualified Data.List as List
import qualified Data.Map.Strict as Map hiding (size)
import Data.Map.Strict (Map, (!))
-- import Data.Set (Set)
-- import qualified Data.Set as Set
import Numeric.Natural
import Control.Arrow (Kleisli(..))
import Data.Monoid.Endomorphism

--- ## Model ## ---

type Nat = Natural

data Point = Point {_row :: Int, _col :: Int} deriving (Show, Eq, Ord)

--                                 pos   width  height
data Furniture     = Furniture     Point Int    Int    deriving (Eq, Ord)
data NormFurniture = NormFurniture Point Nat    Nat    deriving (Show, Eq, Ord)

data Grid = Grid (Nat, Nat) (Map Point Bool) deriving (Eq, Ord)

instance Show Furniture where
  show (Furniture (Point row col) width height) =
    "Furniture {row:" ++ show row ++ ", col:" ++ show col ++ ", width:" ++ show width ++ ", height:" ++ show height ++ "}"

instance Show Grid where
  show = showGrid

--                 Floor     Furniture
--                 Structure      id
data State = State Grid      (Map Nat Furniture)

--- Grid to matrix ---

tabulateList2 :: Nat -> Nat -> (Nat -> Nat -> a) -> [[a]]
tabulateList2 width height f = (\row -> (\col -> f row col) <$> [0..(width - 1)]) <$> [0..(height - 1)]

-- Assumes that for each index in (width, height), a value is set.
gridList :: Grid -> [[Bool]]
gridList (Grid (width, height) mat) = tabulateList2 width height (\row col -> mat ! Point (fromIntegral row) (fromIntegral col))

--- Addition, construction ---

-- Lens possibility
setPoint :: Point -> (Grid -> Maybe Grid)
setPoint p (Grid dim grid) =
            case Map.lookup p grid of
              Nothing -> Nothing
              Just True -> Nothing
              Just False -> Just $ Grid dim (Map.insert p True grid)

-- Moves the origin to the upper left, width and height are non-negative (Nats).
normalize :: Furniture -> NormFurniture
normalize (Furniture (Point row  col ) width  height) =
           NormFurniture (Point row' col') width' height'
           where
               (row', height') = normNegative row height
               (col', width')  = normNegative col width



furniturePoints :: Furniture -> [Point]
furniturePoints (normalize -> (NormFurniture (Point row col) width height)) =
                 join $ tabulateList2 width height (\drow dcol -> Point (row + fromIntegral drow) (col + fromIntegral dcol))


addFurnitureGrid :: Furniture -> (Grid -> Maybe Grid)
addFurnitureGrid furniture = applyAll setPoint (furniturePoints furniture)

constructFloor :: Foldable f => f Furniture -> Grid -> Maybe Grid
constructFloor = applyAll addFurnitureGrid

stateGrid :: State -> Maybe Grid
stateGrid (State floor fs) = constructFloor (Map.elems fs) floor

-- addFurniture :: Furniture -> State -> Maybe State
-- addFurniture furniture (State grid set) = Set.insert furniture set

--- Transforms ---

rotateClockwise :: Furniture -> Furniture
rotateClockwise (Furniture p w h) = Furniture p (-h) w

rotateCounterClockwise :: Furniture -> Furniture
rotateCounterClockwise (Furniture p w h) = Furniture p h (-w)

translate :: Int -> Int -> Furniture -> Furniture
translate drow dcol (Furniture (Point row col) w h) = Furniture (Point (row + drow) (col + dcol)) w h

moveUp, moveDown, moveLeft, moveRight :: Furniture -> Furniture
moveUp    = translate (-1) 0
moveDown  = translate 1    0
moveLeft  = translate 0    (-1)
moveRight = translate 0    1

transforms :: [Furniture -> Furniture]
transforms = [moveUp, moveDown, moveLeft, moveRight, rotateClockwise, rotateCounterClockwise]

allTransforms :: Furniture -> [Furniture]
allTransforms furniture = ($ furniture) <$> transforms
--                         ^ apply with furniture for each element in transforms.

--- ## Everything else ## ---


--- Numeric ---

abs :: Int -> Nat
abs n | n < 0     = fromIntegral (-n)
      | otherwise = fromIntegral n

-- subtracts w from origin if it's negative, otherwise, untouched, and abs'es w.
-- normNegative 5 3    = (5,3)
-- normNegative 5 (-3) = (2,3)
-- normNegative 3 (-5) = (-2, 5)
normNegative :: Int -> Int -> (Int, Nat)
normNegative origin w | w < 0     = (origin + w, abs w)
                      | otherwise = (origin    , abs w)

--- Kleisli monoid fun ---

endoM :: Monad m => (a -> m a) -> Endomorphism (Kleisli m) a
endoM = Endomorphism . Kleisli

runEndoM :: Monad m => Endomorphism (Kleisli m) a -> (a -> m a)
runEndoM = runKleisli . getEndomorphism

-- apply the operation staring from the b for each a in the list.
applyAll :: (Monad m, Foldable f) => (a -> (b -> m b)) -> f a -> b -> m b
applyAll f list = runEndoM $ foldMap (endoM . f) list

--- Show grid ---

mkString :: (a -> String) -> String -> [a] -> String
mkString f sep list = List.intercalate sep (f <$> list)

showMat :: (a -> String) -> String -> [[a]] -> String
showMat f sep grid = unlines (mkString f sep <$> grid)

showGrid :: Grid -> String
showGrid grid = '\n' : showMat (\b -> if b then "X" else "O") "" mat
                where mat = gridList grid

--- Ghci Testing ---

emptyGrid :: Nat -> Nat -> Grid
emptyGrid width height = Grid (width,height) (Map.fromList $ rectangle width height)

initial :: State
initial = State (emptyGrid 5 5) Map.empty

rectangle :: Nat -> Nat -> [(Point, Bool)]
rectangle width height = do
                         row <- [0..height - 1]
                         col <- [0..width - 1]
                         return (Point (fromIntegral row) (fromIntegral col), False)
                         --             Nat -> Int         Nat -> Int

printState :: State -> IO ()
printState = print . stateGrid

main :: IO ()
main = printState initial
