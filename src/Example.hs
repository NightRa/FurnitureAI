module Example where

import Model
import Creation

import Data.Foldable
import Data.Monoid

import Prelude hiding (length)
import Data.Maybe (fromJust)
import qualified Data.Map.Strict as Map
import qualified Data.List as List


--- Ghci Testing ---

emptyGrid :: Nat -> Nat -> Grid
emptyGrid width height = Grid (Map.fromList $ rectangle width height (Point 0 0) (const FloorBlock))

tempFilledGrid :: Grid
tempFilledGrid = error "stateFloor evaluates filledGrid"

believeValidState :: (Grid -> State) -> State
believeValidState f = fromJust . stateFloor $ f tempFilledGrid

initialGrid :: Grid
initialGrid = roomGrid $ Room (Point 0 0) 5 5

initial :: State
initial = State initialGrid (Map.fromList [(1,Furniture (Point 4 1) 2 2),(2, Furniture (Point 4 4) 2 2)]) Nothing initialGrid

goalState :: State
goalState = believeValidState $ State initialGrid (Map.fromList [(1,Furniture (Point 1 4) 2 2),(2, Furniture (Point 1 1) 2 2)]) Nothing


createFloor :: [Room] -> [Door] -> Grid
createFloor rooms doors = foldMap doorGrid doors <> foldMap roomGrid rooms

format :: Maybe [State] -> String
format (Just list) = "Solution length: " ++ show (List.length list) ++ "\n" ++ mkString show "\n\n" list
format Nothing = "No path!"

swapFloor :: Grid
swapFloor = createFloor [Room (Point 0 0) 10 10, Room (Point 0 11) 10 10] [Door Vertical (Point 4 11) 3]

swapFurniture :: [(Nat, Furniture)]
swapFurniture = [(1,Furniture (Point 4 8) 3 3), (2, Furniture (Point 4 12) 3 3)]

swapFurnitureGoal :: [(Nat, Furniture)]
swapFurnitureGoal = [(2,Furniture (Point 4 8) 3 3), (1, Furniture (Point 4 12) 3 3)]

swapInitial :: State
swapInitial = believeValidState $ State swapFloor (Map.fromList swapFurniture) Nothing

swapGoal :: State
swapGoal = believeValidState $ State swapFloor (Map.fromList swapFurnitureGoal) Nothing
