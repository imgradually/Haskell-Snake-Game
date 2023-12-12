{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module Snake
  ( initGame
  , step
  , turn1, turn2
  , Game(..)
  , Direction(..)
  , food, dead, winner, score1, score2, snake1, snake2
  , height, width
  , pauseGame
  ) where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Maybe()

import Control.Lens
    ( (&), (^.), use, (%=), (%~), (.=), (.~), makeLenses )
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import Control.Monad.Extra (orM)
import Data.Sequence (Seq(..), (<|))
import qualified Data.Sequence as S
import Linear.V2 (V2(..), _x, _y)
import System.Random (Random(..), newStdGen)


-- import Debug.Trace
-- this is a method to print the value in terminal to debug 
-- temp <- use snake
-- trace (show "") $ return ()

-- Types

data Game = Game
  { _snake1  :: Snake        -- ^ snake as a sequence of points in N2
  , _dir1    :: Direction    -- ^ direction
  , _score1  :: Int          -- ^ score
  , _locked1 :: Bool         -- ^ lock to disallow duplicate turns between time steps
  , _snake2  :: Snake        -- ^ snake as a sequence of points in N2
  , _dir2    :: Direction    -- ^ direction
  , _score2  :: Int          -- ^ score
  , _locked2 :: Bool         -- ^ lock to disallow duplicate turns between time steps
  , _food    :: Coord        -- ^ location of the food
  , _foods   :: Stream Coord -- ^ infinite list of random next food locations
  , _dead    :: Bool         -- ^ game over flag
  , _paused  :: Bool         -- ^ paused flag
  , _winner  :: Int          -- ^ 1 if winner is P1, 2 if winner is P2
  } deriving (Show)

type Coord = V2 Int

type Snake = Seq Coord

data Stream a = a :| Stream a
  deriving (Show)

data Direction
  = North
  | South
  | East
  | West
  deriving (Eq, Show)

makeLenses ''Game

-- Constants

height, width :: Int
height = 40
width = 40

-- Functions

-- | Step forward in time
step :: Game -> Game
step s = flip execState s . runMaybeT $ do

  -- Make sure the game isn't paused or over
  MaybeT $ guard . not <$> orM [use paused, use dead]

  -- Unlock from last directional turn
  MaybeT . fmap Just $ locked1 .= False
  MaybeT . fmap Just $ locked2 .= False

  -- die (moved into boundary), eat (moved into food), or move (move into space)
  die <|> eatFood1 <|> eatFood2 <|> MaybeT (Just <$> modify move)

-- | Possibly die if next head position is in snake
die :: MaybeT (State Game) ()
die = do
  MaybeT . fmap guard $ do
    snakeCrash1 <- get
      >>=
        (\ headValue
          -> (||) <$> (elem headValue <$> use snake1)
                <*> (elem headValue <$> use snake2))
          . nextHead1
    snakeCrash2 <- get
      >>=
        (\ headValue
          -> (||) <$> (elem headValue <$> use snake1)
                <*> (elem headValue <$> use snake2))
          . nextHead2
    wallCrash1  <- checkNextHeadOOB1 <$> get
    wallCrash2  <- checkNextHeadOOB2 <$> get
    return (snakeCrash1 || snakeCrash2|| wallCrash1 || wallCrash2)
  
  s1 <- use score1
  s2 <- use score2

  if s1 >= s2
    then MaybeT . fmap Just $ winner .= 1
    else MaybeT . fmap Just $ winner .= 2

  MaybeT . fmap Just $ dead .= True

-- | Possibly eat food if next head position is food
eatFood1 :: MaybeT (State Game) ()
eatFood1 = do
  MaybeT . fmap guard $ (==) <$> (nextHead1 <$> get) <*> use food
  MaybeT . fmap Just $ do
    score1 %= (+ 10)
    get >>= \g -> snake1 %= (nextHead1 g <|)
    nextFood

eatFood2 :: MaybeT (State Game) ()
eatFood2 = do
  MaybeT . fmap guard $ (==) <$> (nextHead2 <$> get) <*> use food
  MaybeT . fmap Just $ do
    score2 %= (+ 10)
    get >>= \g -> snake2 %= (nextHead2 g <|)
    nextFood

-- | Set a valid next food coordinate
nextFood :: State Game ()
nextFood = do
  (f :| fs) <- use foods
  foods .= fs

  isInS1 <- elem f <$> use snake1
  isInS2 <- elem f <$> use snake2

  if isInS1 || isInS2
    then nextFood
    else food .= f
        

-- | Move snake along in a marquee fashion
move :: Game -> Game
move g@Game { _snake1 = (s1 :|> _), _snake2 = (s2 :|> _)  } =
   g & snake1 .~ (nextHead1 g <| s1) & snake2 .~ (nextHead2 g <| s2)
move _                             = error "Snakes can't be empty!"

-- | Pause the game
pauseGame :: Game -> Game
pauseGame g = g & if g ^. paused then paused .~ False else paused .~ True

-- | Get next head position of the snake
nextHead1 :: Game -> Coord
nextHead1 Game { _dir1 = d, _snake1 = (a :<| _) }
  | d == North = a & _y %~ (\y -> (y + 1) `mod` height)
  | d == South = a & _y %~ (\y -> (y - 1) `mod` height)
  | d == East  = a & _x %~ (\x -> (x + 1) `mod` width)
  | d == West  = a & _x %~ (\x -> (x - 1) `mod` width)
nextHead1 _ = error "Snakes can't be empty!"

nextHead2 :: Game -> Coord
nextHead2 Game { _dir2 = d, _snake2 = (a :<| _) }
  | d == North = a & _y %~ (\y -> (y + 1) `mod` height)
  | d == South = a & _y %~ (\y -> (y - 1) `mod` height)
  | d == East  = a & _x %~ (\x -> (x + 1) `mod` width)
  | d == West  = a & _x %~ (\x -> (x - 1) `mod` width)
nextHead2 _ = error "Snakes can't be empty!"

-- | Check if next position moves out of bound
checkNextHeadOOB1 :: Game -> Bool
checkNextHeadOOB1 Game { _dir1 = d, _snake1 = ((V2 x y) :<| _) }
  | d == North = y == height-1
  | d == South = y == 0  
  | d == East  = x == width-1
  | d == West  = x == 0
checkNextHeadOOB1 _ = error "Snakes can't be empty!"

checkNextHeadOOB2 :: Game -> Bool
checkNextHeadOOB2 Game { _dir2 = d, _snake2 = ((V2 x y) :<| _) }
  | d == North = y == height-1
  | d == South = y == 0  
  | d == East  = x == width-1
  | d == West  = x == 0
checkNextHeadOOB2 _ = error "Snakes can't be empty!"
-- | Turn game direction (only turns orthogonally)
--
-- Implicitly unpauses yet locks game
turn1 :: Direction -> Game -> Game
turn1 d g = if g ^. locked1
  then g
  else g & dir1 %~ turnDir d & paused .~ False & locked1 .~ True

turn2 :: Direction -> Game -> Game
turn2 d g = if g ^. locked2
  then g
  else g & dir2 %~ turnDir d & paused .~ False & locked2 .~ True

turnDir :: Direction -> Direction -> Direction
turnDir n c | c `elem` [North, South] && n `elem` [East, West] = n
            | c `elem` [East, West] && n `elem` [North, South] = n
            | otherwise = c

-- | Initialize a paused game with random food location
initGame :: IO Game
initGame = do
  (f :| fs) <-
    fromList . randomRs (V2 0 0, V2 (width - 1) (height - 1)) <$> newStdGen
  let x1 = width `div` 2
      y1 = height `div` 4 
      x2 = width `div` 2
      y2 = height `div` 4 * 3
      g  = Game
        { _snake1  = S.singleton (V2 x1 y1)
        , _score1  = 0
        , _dir1    = North
        , _locked1 = False
        , _snake2  = S.singleton (V2 x2 y2)
        , _score2  = 0
        , _dir2    = South
        , _locked2 = False        
        , _food    = f
        , _foods   = fs
        , _paused  = True
        , _dead    = False
        , _winner  = 0
        }
  return $ execState nextFood g

fromList :: [a] -> Stream a
fromList = foldr (:|) (error "Streams must be infinite")


    


