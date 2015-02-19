{-# LANGUAGE ScopedTypeVariables #-}
module Creation where

--- Imports ---

import Model

import Data.Functor
import Data.Monoid
import Data.Foldable hiding (all)
import Control.Applicative
import Control.Monad
import Control.Lens
import Control.Lens.Lens
import Control.Lens.Iso
import Control.Lens.Prism
import Prelude hiding (abs)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Debug.Trace

--- Datatypes ---

data Stage = SelectRooms | SelectDoors | SelectFurnitureStart | SelectFurnitureGoal | EndStage
data Task = SelectRect | SelectVertical | SelectHorizontal
data Step = Origin | Destination

data BlockErr = ValidBlock Block | InvalidBlock deriving Eq
type GridErr = Map Point BlockErr
                                 --   pointer                            origin pointer
data Selection = OriginSelection Grid Point  | DestinationSelection Grid Point  Point
-- data State = State Stage Task Selection

data Room = Room Point Nat Nat
data Orientation = Vertical | Horizontal
            --               origin length
data Door = Door Orientation Point  Nat



--- Optics ---

pointer :: Lens' Selection Point
pointer f (OriginSelection g p)         = OriginSelection g         <$> f p
pointer f (DestinationSelection g po p) = DestinationSelection g po <$> f p

{-# ANN selectionGrid "HLint: ignore Avoid lambda" #-}
selectionGrid :: Lens' Selection Grid
selectionGrid f (OriginSelection g p)         = (\g' -> OriginSelection g' p)         <$> f g
selectionGrid f (DestinationSelection g po p) = (\g' -> DestinationSelection g' po p) <$> f g

blockErrIso :: Iso' BlockErr (Maybe Block)
blockErrIso = iso to from
              where to InvalidBlock   = Nothing
                    to (ValidBlock b) = Just b
                    from Nothing      = InvalidBlock
                    from (Just b)     = ValidBlock b

blockErr :: Prism' BlockErr Block
blockErr = blockErrIso . _Just

--- Objects to positions ---

rectangle :: Nat -> Nat -> Point -> (Point -> Block) -> [(Point, Block)]
rectangle width height _      _ | width == 0 || height == 0 = []
rectangle width height origin f =
    do
      row <- [0..height - 1]
      col <- [0..width - 1]
      let relative = Point (fromNat row) (fromNat col)
      return (origin <> relative, f relative)
      --             Nat -> Int         Nat -> Int

roomGrid :: Room -> Grid
roomGrid r = Grid $ Map.fromList $ roomBlocks r

roomBlocks :: Room -> [(Point, Block)]
roomBlocks (Room origin width height) = rectangle width height origin block
                                        where block p = if isWall p then WallBlock else FloorBlock
                                              isWall (Point row col) = row == 0 || (row == fromNat height - 1) || col == 0 || col == (fromNat width - 1)

doorGrid :: Door -> Grid
doorGrid = Grid . Map.fromList . doorBlocks

doorBlocks :: Door -> [(Point,Block)]
doorBlocks (Door Vertical origin length)   = rectangle 1 length origin (const FloorBlock)
doorBlocks (Door Horizontal origin length) = rectangle length 1 origin (const FloorBlock)

--- GridErr manipulation: marking points ---

--             is valid position?                         block type
markPoint :: ((Point -> Maybe Block) -> Point -> Bool) -> Block ->   Point -> GridErr -> GridErr
markPoint cond block p grid = if cond ((`Map.lookup` grid) >=> preview blockErr) p
                              then Map.insert p (ValidBlock block) grid
                              else Map.insert p InvalidBlock grid

markWallBlock :: Point -> GridErr -> GridErr
markWallBlock = markPoint isValidWall WallBlock

markFurnitureBlock :: ID -> Point -> GridErr -> GridErr
markFurnitureBlock id = markPoint isValidFurniture (FurnitureBlock (Just id))

markDoorBlock :: Point -> GridErr -> GridErr
markDoorBlock = markPoint isValidDoor FloorBlock

isValidWall :: (Point -> Maybe Block) -> Point -> Bool
isValidWall _ _ = True

isValidFurniture :: (Point -> Maybe Block) -> Point -> Bool
isValidFurniture grid p = grid p == Just FloorBlock

isValidDoor :: (Point -> Maybe Block) -> Point -> Bool
isValidDoor grid p = all (\b -> b == Just WallBlock || b == Just FloorBlock) (grid <$> neighbours p) && grid p == Just WallBlock

neighbours :: Point -> [Point]
neighbours (Point row col) =
    -- filter (\(Point row' col') -> row' >= 0 && col' >= 0) $
    (\drow dcol -> Point (row + drow) (col + dcol)) <$> [-1..1] <*> [-1..1]


--- Marking objects ---

markRoom :: Room -> Grid -> GridErr
markRoom room grid = review gridErr' (roomGrid room <> grid)

markDoor :: Door -> Grid -> GridErr
markDoor door grid = alaf Endo foldMap markDoorBlock doorPoints (review gridErr' grid)
                            where doorPoints = fst <$> doorBlocks door

markFurniture :: ID -> Furniture -> Grid -> GridErr
markFurniture id furniture grid = alaf Endo foldMap (markFurnitureBlock id) (furniturePoints furniture) (review gridErr' grid)

--- Creation of objects from 2 points ---

furnitureFromPoints :: Point -> Point -> NormFurniture
furnitureFromPoints origin@(Point row1 col1) (Point row2 col2) = ajust $ normalize $ Furniture origin (col2 - col1) (row2 - row1)
        where ajust (NormFurniture origin width height) = NormFurniture origin (width + 1) (height + 1)

roomFromPoints :: Point -> Point -> Room
roomFromPoints p1 p2 = fromFurniture (furnitureFromPoints p1 p2)
                       where fromFurniture (NormFurniture origin width height) = Room origin width height

doorFromPoints :: Orientation -> Point -> Point -> Door
doorFromPoints o@Vertical   p1@(Point row1 _   ) p2@(Point row2 _   ) = Door o origin height
                where origin = if row1 < row2 then p1 else p2
                      height = abs (row2 - row1) + 1

doorFromPoints o@Horizontal p1@(Point _    col1) p2@(Point _    col2) = Door o origin width
                where origin = if col1 < col2 then p1 else p2
                      width = abs (col2 - col1) + 1

--- Validation ---

gridErr' ::  Prism' GridErr Grid
gridErr' = prism' (\(Grid grid) -> review blockErr <$> grid) (\gridErr -> Grid <$> traverse (preview blockErr) gridErr)

