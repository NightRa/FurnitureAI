{-# LANGUAGE ViewPatterns #-}
module Model where

import Prelude hiding (lookup, floor, abs, length)
import Data.Functor
import Control.Monad
import Data.Monoid
import Data.Foldable
import Data.Maybe (isJust, mapMaybe, fromJust)
import Data.Function (on)
import qualified Data.List as List
import qualified Data.Map.Strict as Map hiding (size)
import Data.Map.Strict (Map)
-- import Data.Set (Set)
import qualified Data.Set as Set
import Numeric.Natural
import Control.Arrow (Kleisli(..))
import Data.Monoid.Endomorphism
import Data.Graph.AStar

--- ## Model ## ---

type Nat = Natural

data Point = Point {_row :: Int, _col :: Int} deriving (Show, Eq, Ord)

data Block = FloorBlock | WallBlock | FurnitureBlock deriving (Show, Eq, Ord)
data Move = MoveUp | MoveDown | MoveLeft | MoveRight | RotateClockwise | RotateCounterClockwise deriving (Show, Eq, Ord)

--                                 pos   width  height
data Furniture     = Furniture     Point Int    Int    deriving (Eq, Ord)
data NormFurniture = NormFurniture Point Nat    Nat    deriving (Show, Eq, Ord)

data Grid = Grid (Map Point Block) deriving (Eq, Ord)

--                 Floor     Furniture
--                 Structure      id
data State = State Grid      (Map Nat Furniture) (Maybe Move) Grid deriving (Eq, Ord)

--- Model typeclass instances ---

instance Monoid Point where
    mappend (Point x y) (Point x' y') = Point (x + x') (y + y')
    mempty = Point 0 0

instance Monoid Grid where
    mappend = combineFloors
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

--- Grid to matrix ---

tabulateList2 :: Nat -> Nat -> (Nat -> Nat -> a) -> [[a]]
tabulateList2 width height f = (\row -> (\col -> f row col) <$> [0..(width - 1)]) <$> [0..(height - 1)]

-- Assumes that for each index in (width, height), a value is set.
gridList :: Grid -> [[Maybe Block]]
gridList (Grid grid) = tabulateList2 width height (\row col -> Map.lookup (Point (fromIntegral row) (fromIntegral col)) grid)
                      where width, height :: Nat
                            width  = fromIntegral maxCol + 1  -- (+ 1) because the indexes start from 0.
                            height = fromIntegral maxRow + 1
                            Point _ maxCol  = maximumBy (compare `on` _col) positions
                            Point maxRow _  = maximumBy (compare `on` _row) positions
                            positions = Map.keys grid

--- Addition, construction ---

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



furniturePoints :: Furniture -> [Point]
furniturePoints (normalize -> (NormFurniture (Point row col) width height)) =
                 join $ tabulateList2 width height (\drow dcol -> Point (row + fromIntegral drow) (col + fromIntegral dcol))


fillFurniture :: Furniture -> (Grid -> Maybe Grid)
fillFurniture furniture = applyAll setPoint (furniturePoints furniture)

constructFloor :: Foldable f => f Furniture -> Grid -> Maybe Grid
constructFloor = applyAll fillFurniture

stateFloor :: State -> Maybe State
stateFloor state@(State floor fs move _) = State floor fs move <$> constructFloor (Map.elems fs) floor

addFurniture :: Furniture -> Nat -> State -> Maybe State
addFurniture furniture id' (State grid fs move filledGrid) = stateFloor $ State grid (Map.insert id' furniture fs) move filledGrid

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

transforms :: [(Furniture -> Furniture, Move)]
transforms = [(moveUp, MoveUp), (moveDown, MoveDown), (moveLeft, MoveLeft), (moveRight, MoveRight), (rotateClockwise, RotateClockwise), (rotateCounterClockwise, RotateCounterClockwise)]

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

--- ## AI ## ---

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

--- Set manipulation ---

-- Requires that all the keys are equal for both maps.
mergeWith :: Ord k => (a -> b -> c) -> Map k a -> Map k b -> Map k c
mergeWith f t1 t2 = Map.mergeWithKey (\_ x1 x2 -> Just $ f x1 x2) (const Map.empty) (const Map.empty) t1 t2

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

distance :: Point -> Point -> Double
distance (Point x y) (Point x' y') = sqrt $ (fromIntegral x - fromIntegral x')^(2 :: Int) + (fromIntegral y - fromIntegral y')^(2 :: Int)

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
showGrid grid = '\n' : showMat showBlock "" mat
                where mat = gridList grid
                      showBlock Nothing               = " "
                      showBlock (Just FloorBlock)     = "-"
                      showBlock (Just WallBlock)      = "█"
                      showBlock (Just FurnitureBlock) = "X"


--- Ghci Testing ---

emptyGrid :: Nat -> Nat -> Grid
emptyGrid width height = Grid (Map.fromList $ rectangle width height (Point 0 0) (const FloorBlock))

tempFilledGrid :: Grid
tempFilledGrid = error "stateFloor evaluates filledGrid"

believeValidState :: (Grid -> State) -> State
believeValidState f = fromJust . stateFloor $ f tempFilledGrid

initialGrid :: Grid
initialGrid = room $ Room 5 5 (Point 0 0)

initial :: State
initial = State initialGrid (Map.fromList [(1,Furniture (Point 4 1) 2 2),(2, Furniture (Point 4 4) 2 2)]) Nothing initialGrid

goalState :: State
goalState = believeValidState $ State initialGrid (Map.fromList [(1,Furniture (Point 1 4) 2 2),(2, Furniture (Point 1 1) 2 2)]) Nothing

rectangle :: Nat -> Nat -> Point -> (Point -> Block) -> [(Point, Block)]
rectangle width height origin f =
    do
      row <- [0..height - 1]
      col <- [0..width - 1]
      let relative = Point (fromIntegral row) (fromIntegral col)
      return (origin <> relative, f relative)
      --             Nat -> Int         Nat -> Int

data Room = Room Nat Nat Point
data Orientation = Vertical | Horizontal
            --               origin length
data Door = Door Orientation Point  Nat
room :: Room -> Grid
room (Room width height origin) = Grid $ Map.fromList $ rectangle (width + 2) (height + 2) origin block
                                                        where block p = if isWall p then WallBlock else FloorBlock
                                                              isWall (Point row col) = row == 0 || row == (fromIntegral width + 1) || col == 0 || col == (fromIntegral height + 1)

door :: Door -> Grid
door (Door Vertical origin length) = Grid $ Map.fromList $ rectangle 1 length origin (const FloorBlock)
door (Door Horizontal origin length) = Grid $ Map.fromList $ rectangle length 1 origin (const FloorBlock)

combineFloors :: Grid -> Grid -> Grid
combineFloors (Grid a) (Grid b) = Grid $ Map.union a b

createFloor :: [Room] -> [Door] -> Grid
createFloor rooms doors = foldMap door doors <> foldMap room rooms

format :: Maybe [State] -> String
format (Just list) = "Solution length: " ++ show (List.length list) ++ "\n" ++ mkString show "\n\n" list
format Nothing = "No path!"

swapFloor :: Grid
swapFloor = createFloor [Room 10 10 (Point 0 0), Room 10 10 (Point 0 11)] [Door Vertical (Point 4 11) 3]

swapFurniture :: [(Nat, Furniture)]
swapFurniture = [(1,Furniture (Point 4 8) 3 3), (2, Furniture (Point 4 12) 3 3)]

swapFurnitureGoal :: [(Nat, Furniture)]
swapFurnitureGoal = [(2,Furniture (Point 4 8) 3 3), (1, Furniture (Point 4 12) 3 3)]

swapInitial :: State
swapInitial = believeValidState $ State swapFloor (Map.fromList swapFurniture) Nothing

swapGoal :: State
swapGoal = believeValidState $ State swapFloor (Map.fromList swapFurnitureGoal) Nothing


