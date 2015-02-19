{-# LANGUAGE ForeignFunctionInterface, ExplicitForAll, ScopedTypeVariables #-}
module Main where

import Model
import Example
import Creation

import Control.Lens
import Control.Monad.Trans.Maybe
import Control.Arrow
import Data.Functor
import Data.Composition

import Data.Char
import Data.Maybe
import Data.Array hiding (index)
import qualified Data.Map.Strict as Map

import Data.IORef
import System.IO
import System.Console.ANSI
import System.Exit
import Foreign.C.Types

import Debug.Trace

----------------------------------------------------------------------
------------------------------- FFI ----------------------------------
----------------------------------------------------------------------

foreign import ccall unsafe "conio.h getch"
  c_getch :: IO CInt

getHiddenChar :: IO Char
getHiddenChar = fmap (chr.fromEnum) c_getch

----------------------------------------------------------------------
--------------------------- Datatypes --------------------------------
----------------------------------------------------------------------

data Action = MoveCursor Direction | Confirm | Cancel
data Direction = UpDir | DownDir | LeftDir | RightDir

main' :: IO ()
main' = do
        setTitle "Furniture AI"
        hideCursor
        showState' 0 states
        frame <- newIORef (0 :: Nat)
        loop frame size
       where
        result = fromJust $ bestPath swapGoal swapInitial
        size = fromIntegral $ length result
        states :: Array Nat State
        states = listArray (0,size - 1) result
        loop :: IORef Nat -> Nat -> IO ()
        loop ref size = do
                 c <- getHiddenChar
                 handleAction size ref c
                 index <- readIORef ref
                 showState' index states
                 loop ref size
        -- putStrLn (format $ bestPath swapGoal swapInitial)

showState' :: Nat -> Array Nat State -> IO ()
showState' frame arr = do
                       clearScreen
                       setCursorPosition 0 0
                       putStrLn $ "Result frame " ++ show frame
                       print $ arr ! frame

back :: Nat -> Nat
back 0 = 0
back n = n - 1

next :: Nat -> Nat -> Nat
next size n | n == size - 1 = n
next _    n                 = n + 1

handleAction :: Nat -> IORef Nat -> Char -> IO ()
handleAction _    ref 'l' = modifyIORef' ref back
handleAction size ref 'r' = modifyIORef' ref (next size)
handleAction _    _   'q' = exitSuccess
handleAction _    _   _   = return ()

readAction :: IO Action
readAction = getHiddenChar >>= (demand . action)
             where demand :: Maybe Action -> IO Action
                   demand Nothing  = readAction
                   demand (Just a) = return a

action :: Char -> Maybe Action
action 'w'  = Just $ MoveCursor UpDir
action 'a'  = Just $ MoveCursor LeftDir
action 's'  = Just $ MoveCursor DownDir
action 'd'  = Just $ MoveCursor RightDir
action '\r' = Just Confirm
action '\n' = Just Confirm
action 'z'  = Just Cancel
action  _   = Nothing

showGridErr :: GridErr -> String
showGridErr grid = '\n' : showMat showBlock "" mat
                where mat = gridList grid
                      showBlock Nothing = " "
                      showBlock (Just InvalidBlock)  = "!"
                      showBlock (Just (ValidBlock b)) = show b

printGridErr :: String -> (GridErr,Point) -> IO ()
printGridErr header (grid,Point row col) = do
                                            clearScreen
                                            setCursorPosition 0 0
                                            putStrLn header
                                            putStrLn $ showGridErr grid
                                            setCursorPosition (row + 2) col

----------------------------------------------------------------------
------------------- Generic selection functions ----------------------
----------------------------------------------------------------------

selectPoint :: String -> (Point -> GridErr) -> (Point -> Maybe s') -> (Direction -> Point -> Point) -> Point -> IO (Point,Maybe s')
selectPoint header showState confirm move p = do
                                               printGridErr header (showState p, p)
                                               act <- readAction
                                               case act of
                                                 MoveCursor dir -> selectPoint header showState confirm move (move dir p)
                                                 Confirm        -> case confirm p of
                                                                     Nothing -> selectPoint header showState confirm move p
                                                                     (Just s') -> return (p,Just s')
                                                 Cancel         -> return (p,Nothing)

selectOrigin :: (Point -> GridErr -> GridErr) -> ((Point -> Maybe Block) -> Point -> Bool) -> String -> (Direction -> Point -> Point) -> Grid -> Point -> IO (Point,Maybe Point)
selectOrigin mark isValid header move g@(Grid grid) = selectPoint header showState confirm move
                                                      where showState :: Point -> GridErr
                                                            showState p = mark p (review gridErr' g)
                                                            confirm :: Point -> Maybe Point
                                                            confirm p = if isValid (`Map.lookup` grid) p then Just p else Nothing


selectDestination :: forall a. (Point -> Grid -> (GridErr, a)) -> String -> (Direction -> Point -> Point) -> Grid -> Point -> IO (Point,Maybe (Grid,a))
selectDestination mark header move grid = selectPoint header showState confirm move
                                          where showState :: Point -> GridErr
                                                showState p = fst $ mark p grid
                                                confirm :: Point -> Maybe (Grid, a)
                                                confirm p = (\grid' -> (grid',a)) <$> preview gridErr' gridErr
                                                            where (gridErr,a) = mark p grid


selectDestinationWithOrigin' :: forall a. (Point -> Point -> a) -> (a -> Grid -> GridErr) -> String -> (Direction -> Point -> Point) -> Point -> Grid -> Point -> IO (Point,Maybe (Grid,a))
selectDestinationWithOrigin' fromPoints mark header move origin = selectDestination mark' header move
                where mark' :: Point -> Grid -> (GridErr, a)
                      mark' dest grid = (mark obj grid, obj)
                                         where obj :: a
                                               obj = fromPoints origin dest

selectDestinationWithOrigin :: (Point -> Point -> a) -> (a -> Grid -> GridErr) -> String -> (Direction -> Point -> Point) -> Point -> Grid -> Point -> IO (Point,Maybe Grid)
selectDestinationWithOrigin = compose7 (second (fmap fst) <$>) selectDestinationWithOrigin'


-- selectObject' :: (Grid -> Point -> IO (Point,Maybe Point)) -> (Point -> Grid -> Point -> IO (Point,Maybe Grid)) -> Grid -> Point -> IO (Point,Grid, Maybe a)

selectObject :: (Grid -> Point -> IO (Point,Maybe Point)) -> (Point -> Grid -> Point -> IO (Point,Maybe Grid)) -> Grid -> Point -> IO (Point,Grid)
selectObject selectOrigin' selectDestination' grid start =
            second (fromMaybe grid) <$> compose (flip selectOrigin') (\p origin -> selectDestination' origin grid p) start grid

-- I should use some monad transformer here, just not sure which exactly.
compose :: (Monad m) => (a -> b -> m (a, Maybe c)) -> (a -> c -> m (a, Maybe d)) -> (a -> b -> m (a, Maybe d))
compose st1 st2 a b = st1 a b >>= \(a',mbb) -> case mbb of
                                           Nothing -> return (a',Nothing)
                                           (Just b') -> st2 a' b'

----------------------------------------------------------------------
------------------------ Movement functions --------------------------
----------------------------------------------------------------------

move1D :: Orientation -> Direction -> Point -> Point
move1D Vertical   UpDir    = moveUp
move1D Vertical   DownDir  = moveDown
move1D Horizontal RightDir = moveRight
move1D Horizontal LeftDir  = moveLeft
move1D _          _        = id

move2D :: Direction -> Point -> Point
move2D UpDir    = moveUp
move2D DownDir  = moveDown
move2D LeftDir  = moveLeft
move2D RightDir = moveRight


----------------------------------------------------------------------
----------------------- Selection functions --------------------------
----------------------------------------------------------------------

selectRoomOrigin :: Grid -> Point -> IO (Point,Maybe Point)
selectRoomOrigin = selectOrigin markWallBlock isValidWall "Select the room's origin" move2D

selectRoomDestination :: Point -> Grid -> Point -> IO (Point,Maybe Grid)
selectRoomDestination = selectDestinationWithOrigin roomFromPoints markRoom "Select the room's second point" move2D

                   -- cursor starting position
selectRoom :: Grid -> Point -> IO (Point, Grid)
selectRoom = selectObject selectRoomOrigin selectRoomDestination

selectFurnitureOrigin :: ID -> Grid -> Point -> IO (Point, Maybe Point)
selectFurnitureOrigin id = selectOrigin (markFurnitureBlock id) isValidFurniture "Select the furniture's origin" move2D

selectFurnitureDestination :: ID -> Point -> Grid -> Point -> IO (Point, Maybe (Grid,Furniture))
selectFurnitureDestination id = selectDestinationWithOrigin' (fromNormFurniture .: furnitureFromPoints) (markFurniture id) "Select the furniture's second point" move2D

selectFurniture :: ID -> Grid -> Point -> IO (Point,Grid, Maybe Furniture)
selectFurniture id grid start = (flatr . second (maybe (grid,Nothing) (\(grid',f') -> (grid',Just f')))) <$> compose (flip (selectFurnitureOrigin id)) (\p origin -> selectFurnitureDestination id origin grid p) start grid
                             where flatr :: (a,(b,c)) -> (a,b,c)
                                   flatr (a,(b,c)) = (a,b,c)

selectDoorOrigin :: Grid -> Point -> IO (Point,Maybe Point)
selectDoorOrigin = selectOrigin markDoorBlock isValidDoor "Select the door's origin" move2D

selectDoorDestination :: Orientation -> Point -> Grid -> Point -> IO (Point,Maybe Grid)
selectDoorDestination orientation = selectDestinationWithOrigin (doorFromPoints orientation) markDoor "Select the door's second point" (move1D orientation)

selectDoor :: Orientation -> Grid -> Point -> IO (Point,Grid)
selectDoor orientation = selectObject selectDoorOrigin (selectDoorDestination orientation)

selectStep :: (Grid -> Point -> IO (Point,Grid)) -> UIState -> IO UIState
selectStep select (p, State grid furnitures _ _, id) = f <$> select grid p
                                                       where f :: (Point,Grid) -> (Point,State,ID)
                                                             f (p',grid') = (p', State grid' furnitures Nothing grid', id)

selectRoomStep :: UIState -> IO UIState
selectRoomStep = selectStep selectRoom

selectDoorStep :: Orientation -> UIState -> IO UIState
selectDoorStep o = selectStep (selectDoor o)

selectFurnitureStep :: UIState -> IO UIState
selectFurnitureStep (p, s@(State grid furnitures _ grid'), id) = f <$> selectFurniture id grid' p
                                                         where f :: (Point,Grid,Maybe Furniture) -> (Point,State,ID)
                                                               f (p', grid'', Nothing) = (p', s, id)
                                                               f (p', grid'', Just furniture) = (p', State grid (Map.insert id furniture furnitures) Nothing grid'', id + 1)

moveFurniture :: ID -> UIState -> IO UIState
moveFurniture id uis@(_, s@(State grid furnitures _ grid'), currentId) =
    case Map.lookup id furnitures of
    Nothing -> return uis
    Just (Furniture oldOrigin width height) ->
        package <$> selectDestination (\origin grid'' -> moveFurniture origin grid'') "Select the destination furniture position" move2D gridWOFurniture oldOrigin
            where moveFurniture :: Point -> Grid -> (GridErr,Furniture)
                  moveFurniture origin grid'' = (markFurniture id moved grid'', moved)
                    where moved :: Furniture
                          moved = Furniture origin width height
                  gridWOFurniture :: Grid
                  gridWOFurniture = filledGridOf $ fromJust $ stateFloor (State grid (Map.delete id furnitures) Nothing (Grid Map.empty))
                  filledGridOf :: State -> Grid
                  filledGridOf (State _ _ _ filledGrid) = filledGrid
                  package :: (Point, Maybe (Grid,Furniture)) -> UIState
                  package (p', Nothing) =  (p', s, currentId)
                  package (p', Just (grid'', furniture)) = (p', State grid (Map.insert id furniture furnitures) Nothing grid'', currentId)


--- Main ---

type UIState = (Point,State,ID)
type UIStep = UIState -> IO UIState

main :: IO ()
main = do
         let s0 :: UIState
             s0 = (Point 0 0, State (Grid Map.empty) Map.empty Nothing (Grid Map.empty), 0)

         s1 <- selectRoomStep s0
         s2 <- selectRoomStep s1
         s3 <- selectDoorStep Vertical s2
         s4 <- selectFurnitureStep s3
         s5 <- selectFurnitureStep s4
         s6 <- moveFurniture 0 s5
         putStrLn "\r\n\r\n\r\n\r\n\r\n\r\n\r\n\r\n"
         print s6
         return ()

