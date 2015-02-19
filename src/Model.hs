{-# LANGUAGE ViewPatterns #-}
module Model where

import Prelude hiding (lookup, floor, abs, length)
import Data.Functor
import Control.Monad
import Data.Monoid
import Data.Foldable
import Data.Maybe (mapMaybe)
import Data.Function (on)
import Control.Lens.Lens
import Control.Lens.Setter
import qualified Data.List as List
import qualified Data.Map.Strict as Map hiding (size)
import Data.Map.Strict (Map)
-- import Data.Set (Set)
import qualified Data.Set as Set
import Numeric.Natural
import Control.Arrow (Kleisli(..))
import Data.Monoid.Endomorphism
import Data.Graph.AStar

----------------------------------------------------------------------
---------------------------- Model -----------------------------------
----------------------------------------------------------------------

type Nat = Natural

data Point = Point {_row :: Int, _col :: Int} deriving (Show, Eq, Ord)

data Block = FloorBlock | WallBlock | FurnitureBlock deriving (Show, Eq, Ord)
data Move = MoveUp | MoveDown | MoveLeft | MoveRight | RotateClockwise | RotateCounterClockwise deriving (Show, Eq, Ord)

--                                 pos   width  height
data Furniture     = Furniture     Point Int    Int    deriving (Eq, Ord)
data NormFurniture = NormFurniture Point Nat    Nat    deriving (Show, Eq, Ord)

data Grid = Grid { ungrid :: Map Point Block } deriving (Eq, Ord)

--                 Floor     Furniture
--                 Structure      id
data State = State Grid      (Map Nat Furniture) (Maybe Move) Grid deriving (Eq, Ord)

----------------------------------------------------------------------
-------------------- Model typeclass instances -----------------------
----------------------------------------------------------------------

instance Monoid Point where
    mappend (Point x y) (Point x' y') = Point (x + x') (y + y')
    mempty = Point 0 0

instance Monoid Grid where
    mappend (Grid a) (Grid b) = Grid $ Map.union a b
    mempty = Grid Map.empty

instance Show Furniture where
  show (Furniture (Point row col) width height) =
    "Furniture {row:" ++ show row ++ ", col:" ++ show col ++ ", width:" ++ show width ++ ", height:" ++ show height ++ "}"

instance Show Grid where
  show = showGrid

instance Show State where
  show (State _ furniture move filledGrid) = showMove move ++ "\n" ++ show filledGrid

showMove :: Maybe Move -> String
showMove Nothing = ""
showMove (Just move) = "Move: " ++ show move


--- Lenses ---


originLens :: Lens' Furniture Point
originLens f (Furniture p w h) = (\p' -> Furniture p' w h) <$> f p

----------------------------------------------------------------------
----------------------- Grid to matrix -------------------------------
----------------------------------------------------------------------

tabulateList2 :: Nat -> Nat -> (Nat -> Nat -> a) -> [[a]]
tabulateList2 width height _ | width == 0 || height == 0 = []
tabulateList2 width height f = (\row -> (\col -> f row col) <$> [0..(width - 1)]) <$> [0..(height - 1)]

-- Does not assume that for each index in (width, height), a value is set.
-- For all points in [0..max row,0..max col], if defined -> Just, otherwise -> None.
-- Takes the largest rectangle that covers the whole floor.
gridList :: Map Point a -> [[Maybe a]]
gridList grid = tabulateList2 width height (\row col -> Map.lookup (Point (fromNat row) (fromNat col)) grid)
                      where width, height :: Nat
                            width  = toNat $ maxCol + 1  -- (+ 1) because the indexes start from 0.
                            height = toNat $ maxRow + 1
                            Point _ maxCol  = maximumBy (compare `on` _col) positions
                            Point maxRow _  = maximumBy (compare `on` _row) positions
                            positions = Map.keys grid


----------------------------------------------------------------------
------------------- Addition & construction --------------------------
----------------------------------------------------------------------

-- Lens possibility
setPoint :: Point -> (Grid -> Maybe Grid)
setPoint p (Grid grid) =
            case Map.lookup p grid of
              Nothing -> Nothing
              Just WallBlock -> Nothing
              Just FurnitureBlock -> Nothing
              Just FloorBlock -> Just $ Grid (Map.insert p FurnitureBlock grid)

-- Moves the origin to the upper left, width and height are non-negative (Nats).
normalize :: Furniture -> NormFurniture
normalize (Furniture (Point row  col ) width  height) =
           NormFurniture (Point row' col') width' height'
           where
               (row', height') = normNegative row height
               (col', width')  = normNegative col width

fromNormFurniture :: NormFurniture -> Furniture
fromNormFurniture (NormFurniture origin width height) = Furniture origin (fromNat width) (fromNat height)


furniturePoints :: Furniture -> [Point]
furniturePoints (normalize -> (NormFurniture (Point row col) width height)) =
                 join $ tabulateList2 width height (\drow dcol -> Point (row + fromNat drow) (col + fromNat dcol))


fillFurniture :: Furniture -> (Grid -> Maybe Grid)
fillFurniture furniture = applyAll setPoint (furniturePoints furniture)

constructFloor :: Foldable f => f Furniture -> Grid -> Maybe Grid
constructFloor = applyAll fillFurniture

stateFloor :: State -> Maybe State
stateFloor (State floor fs move _) = State floor fs move <$> constructFloor (Map.elems fs) floor

addFurniture :: Furniture -> Nat -> State -> Maybe State
addFurniture furniture id' (State grid fs move filledGrid) = stateFloor $ State grid (Map.insert id' furniture fs) move filledGrid

----------------------------------------------------------------------
------------------------- Transforms ---------------------------------
----------------------------------------------------------------------

rotateClockwise :: Furniture -> Furniture
rotateClockwise (Furniture p w h) = Furniture p (-h) w

rotateCounterClockwise :: Furniture -> Furniture
rotateCounterClockwise (Furniture p w h) = Furniture p h (-w)

movePoint :: Int -> Int -> Point -> Point
movePoint drow dcol (Point row col) = Point (max (row + drow) 0) (max (col + dcol) 0)

movef :: (Point -> Point) -> (Furniture -> Furniture)
movef = over originLens

moveUp, moveDown, moveLeft, moveRight :: Point -> Point
moveUp    = movePoint (-1) 0
moveDown  = movePoint 1    0
moveLeft  = movePoint 0    (-1)
moveRight = movePoint 0    1

movefUp, movefDown, movefLeft, movefRight :: Furniture -> Furniture
movefUp    = movef moveUp
movefDown  = movef moveDown
movefLeft  = movef moveLeft
movefRight = movef moveRight

transforms :: [(Furniture -> Furniture, Move)]
transforms = [(movefUp, MoveUp), (movefDown, MoveDown), (movefLeft, MoveLeft), (movefRight, MoveRight), (rotateClockwise, RotateClockwise), (rotateCounterClockwise, RotateCounterClockwise)]

tryTransform :: (Furniture -> Furniture, Move) -> Nat -> State -> Maybe State
tryTransform (f,move) id' state@(State _ fs _ _) =
         do
           furniture <- Map.lookup id' fs
           (State floor fs' _ grid) <- addFurniture (f furniture) id' state
           return $ State floor fs' (Just move) grid

tryTransforms :: Nat -> State -> [State]
tryTransforms id' state = mapMaybe (\f -> tryTransform f id' state) transforms

allTransforms :: State -> [State]
allTransforms state@(State _ fs _ _) =
         do
           id' <- Map.keys fs
           tryTransforms id' state

----------------------------------------------------------------------
------------------------------ AI ------------------------------------
----------------------------------------------------------------------

bestPath :: State -> State -> Maybe [State]
bestPath goal start = (start:) <$> aStar (Set.fromList . allTransforms) (const . const 1) (heuristic goal) (isGoal goal) start

heuristic :: State -> State -> Double
heuristic (State _ goalFurniture _ _) (State _ currentFurniture _ _) = getSum $ foldMap Sum $ mergeWith furnitureDistance currentFurniture goalFurniture
                                                                                      -- merge by key with distance.

isGoal :: State -> State -> Bool
isGoal (State _ goalFurniture _ _) (State _ currentFurniture _ _) = getAll $ foldMap All $ mergeWith equalFurniture currentFurniture goalFurniture

furnitureDistance :: Furniture -> Furniture -> Double
furnitureDistance (normalize -> (NormFurniture p1 _ _)) (normalize -> (NormFurniture p2 _ _)) = distance p1 p2

equalFurniture :: Furniture -> Furniture -> Bool
equalFurniture (normalize -> (NormFurniture p1 w1 h1)) (normalize -> (NormFurniture p2 w2 h2)) = p1 == p2 && w1 == w2 && h1 == h2

--- ## Everything else ## ---

----------------------------------------------------------------------
---------------------- Set Manipulation ------------------------------
----------------------------------------------------------------------

-- Requires that all the keys are equal for both maps.
mergeWith :: Ord k => (a -> b -> c) -> Map k a -> Map k b -> Map k c
mergeWith f t1 t2 = Map.mergeWithKey (\_ x1 x2 -> Just $ f x1 x2) (const Map.empty) (const Map.empty) t1 t2

----------------------------------------------------------------------
------------------------- Numeric ------------------------------------
----------------------------------------------------------------------

abs :: Int -> Nat
abs n | n < 0     = toNat (-n)
      | otherwise = toNat n

-- subtracts w from origin if it's negative, otherwise, untouched, and abs'es w.
-- normNegative 5 3    = (5,3)
-- normNegative 5 (-3) = (2,3)
-- normNegative 3 (-5) = (-2, 5)
normNegative :: Int -> Int -> (Int, Nat)
normNegative origin w | w < 0     = (origin + w, abs w)
                      | otherwise = (origin    , abs w)

distance :: Point -> Point -> Double
distance (Point x y) (Point x' y') = sqrt $ (toDouble x - toDouble x')^(2 :: Int) + (toDouble y - toDouble y')^(2 :: Int)

toNat :: Int -> Nat
toNat = fromIntegral

fromNat :: Nat -> Int
fromNat = fromIntegral

toDouble :: Int -> Double
toDouble = fromIntegral

----------------------------------------------------------------------
-------------------- Kleisli monoid fun ------------------------------
----------------------------------------------------------------------

endoM :: Monad m => (a -> m a) -> Endomorphism (Kleisli m) a
endoM = Endomorphism . Kleisli

runEndoM :: Monad m => Endomorphism (Kleisli m) a -> (a -> m a)
runEndoM = runKleisli . getEndomorphism

-- apply the operation staring from the b for each a in the list.
applyAll :: (Monad m, Foldable f) => (a -> (b -> m b)) -> f a -> b -> m b
applyAll f list = runEndoM $ foldMap (endoM . f) list

----------------------------------------------------------------------
----------------------- Show grid ------------------------------------
----------------------------------------------------------------------

mkString :: (a -> String) -> String -> [a] -> String
mkString f sep list = List.intercalate sep (f <$> list)

showMat :: (a -> String) -> String -> [[a]] -> String
showMat f sep grid = unlines (mkString f sep <$> grid)

showGrid :: Grid -> String
showGrid (Grid grid) = '\n' : showMat showBlock "" mat
                    where mat = gridList grid
                          showBlock Nothing               = " "
                          showBlock (Just FloorBlock)     = "-"
                          showBlock (Just WallBlock)      = "█"
                          showBlock (Just FurnitureBlock) = "X"
