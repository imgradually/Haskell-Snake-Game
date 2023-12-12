{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module Snake
  ( initGame
  , step
  , turn1, turn2
  , Game(..)
  , Direction(..)
  , food, dead, winner, score1, score2, snake1, snake2, freezer, dead1, dead2, p2mode
  , height, width
  , pauseGame
  , applyDeadEffect
  -- , applyFreezeEffect
  ) where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Maybe()

import Control.Lens
    ( (&), (^.), use, (%=), (%~), (.=), (.~), makeLenses )
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import Control.Monad.Extra (orM, andM)
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
  , _dead1   :: Bool         -- ^ snake 1 is dead
  , _snake2  :: Snake        -- ^ snake as a sequence of points in N2
  , _dir2    :: Direction    -- ^ direction
  , _score2  :: Int          -- ^ score
  , _locked2 :: Bool         -- ^ lock to disallow duplicate turns between time steps
  , _dead2   :: Bool         -- ^ snake 2 is dead
  , _food    :: Coord        -- ^ location of the food
  , _freezer :: Coord        -- ^ location of the freezer
  , _foods   :: Stream Coord -- ^ infinite list of random next food locations
  , _freeze1 :: Int          -- freeze duration for snake 1
  , _freeze2 :: Int          -- freeze duration for snake 2
  , _dead    :: Bool         -- ^ game over flag
  , _paused  :: Bool         -- ^ paused flag
  , _winner  :: String          -- ^ 1 if winner is P1, 2 if winner is P2
  , _p2mode  :: Bool         -- double player mode flag
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
  MaybeT $ guard . not <$> orM [use paused, andM [use dead1, use dead2] ]

  -- Unlock from last directional turn
  MaybeT . fmap Just $ locked1 .= False
  MaybeT . fmap Just $ locked2 .= False

  -- die (moved into boundary), eat (moved into food), or move (move into space)
  die <|> eatFood1 <|> eatFood2 <|> eatFreezer1 <|> eatFreezer2 <|> MaybeT (Just <$> modify move)

-- | Possibly die if next head position is in snake
die :: MaybeT (State Game) ()
die = do
  MaybeT $ guard . not <$> orM [use dead1, use dead2]
  die1 <|> die2

die1 :: MaybeT (State Game) ()
die1 = do
  MaybeT . fmap guard $ do
    snakeCrash1 <- get
      >>=
        (\ headValue
          -> (||) <$> (elem headValue <$> use snake1)
                <*> (elem headValue <$> use snake2))
          . nextHead1
    wallCrash1  <- checkNextHeadOOB1 <$> get
    return (snakeCrash1 ||  wallCrash1)
  
  MaybeT . fmap Just $ winner .= "WINNER is\nPLAYER red"
  MaybeT . fmap Just $ dead1 .= True
  MaybeT . fmap Just $ dead .= True

die2 :: MaybeT (State Game) ()
die2 = do
  MaybeT . fmap guard $ do
    snakeCrash2 <- get
      >>=
        (\ headValue
          -> (||) <$> (elem headValue <$> use snake1)
                <*> (elem headValue <$> use snake2))
          . nextHead2
    wallCrash2  <- checkNextHeadOOB2 <$> get
    return (snakeCrash2 ||  wallCrash2)
  
  MaybeT . fmap Just $ winner .= "WINNER is\nPLAYER blue"
  MaybeT . fmap Just $ dead2 .= True
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

-- | Possibly eat freezer if next head position is food
eatFreezer1 :: MaybeT (State Game) ()
eatFreezer1 = do
  MaybeT . fmap guard $ (==) <$> (nextHead1 <$> get) <*> use freezer
  MaybeT . fmap Just $ do
    freeze2 %= (+ 10)
    nextFreezer

eatFreezer2 :: MaybeT (State Game) ()
eatFreezer2 = do
  MaybeT . fmap guard $ (==) <$> (nextHead2 <$> get) <*> use freezer
  MaybeT . fmap Just $ do
    freeze1 %= (+ 10)
    nextFreezer

-- | Set a valid next food coordinate
nextFood :: State Game ()
nextFood = do
  (f :| fs) <- use foods
  foods .= fs

  isInS1 <- elem f <$> use snake1
  isInS2 <- elem f <$> use snake2
  isInfreezer <- (==) f <$> use freezer

  if isInS1 || isInS2 || isInfreezer
    then nextFood
    else food .= f

nextFreezer :: State Game ()
nextFreezer = do
  (f :| fs) <- use foods
  foods .= fs

  isInS1 <- elem f <$> use snake1
  isInS2 <- elem f <$> use snake2
  isInfood <- (==) f <$> use food

  if isInS1 || isInS2 || isInfood
    then nextFreezer
    else freezer .= f

nextFoodAndFreezer :: State Game ()
nextFoodAndFreezer = do
  nextFood
  nextFreezer

-- | Move snake along in a marquee fashion
move :: Game -> Game
move g = move1 $ move2 g
move _                             = error "Snakes can't be empty!"

move1 :: Game -> Game
move1 g@Game { _snake1 = (s1 :|> _), _freeze1 = f1, _dead1 = d1} =
  if d1
    then g
    else if f1 > 0
      then g & (freeze1 .~ f1 - 1)
      else g & (snake1 .~ (nextHead1 g <| s1))
move1 _                            = error "Snakes can't be empty!"

move2 :: Game -> Game
move2 g@Game { _snake2 = (s2 :|> _), _freeze2 = f2, _dead2 = d2} =
  if d2
    then g
    else if f2 > 0
      then g & (freeze2 .~ f2 - 1)
      else g & (snake2 .~ (nextHead2 g <| s2))
move2 _                            = error "Snakes can't be empty!"

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
initGame :: Bool -> IO Game
initGame p2 = do
  (f :| (ff :| fs)) <-
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
        , _dead1   = False
        , _snake2  = S.singleton (V2 x2 y2)
        , _score2  = 0
        , _dir2    = South
        , _locked2 = False        
        , _dead2   = not p2
        , _food    = f
        , _freezer = ff
        , _foods   = fs
        , _freeze1  = 0
        , _freeze2  = 0
        , _paused  = True
        , _dead    = False
        , _winner  = "TIE GAME"
        , _p2mode  = p2
        }
  return $ execState nextFoodAndFreezer g

fromList :: [a] -> Stream a
fromList = foldr (:|) (error "Streams must be infinite")

-- applyFreezeEffect :: Int -> Game -> Game
-- applyFreezeEffect 1 g = g & freeze1 .~ 10
-- applyFreezeEffect 2 g = g & freeze2 .~ 10

applyDeadEffect :: Int -> Game -> Game
applyDeadEffect 1 g = g & dead1 .~ True
applyDeadEffect 2 g = g & dead2 .~ True

