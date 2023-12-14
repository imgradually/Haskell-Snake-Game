{-# LANGUAGE OverloadedStrings #-}
module UI where

import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Class (modify, put, get)
import Control.Concurrent (threadDelay, forkIO)
import Data.Maybe()

import Snake
import Start (start)

import Brick
  -- ( App(..), AttrMap, BrickEvent(..), EventM, Widget
  -- , customMain, neverShowCursor
  -- , halt
  -- , hLimit, vBox, hBox
  -- , padRight, padTop, padAll, Padding(..)
  -- , withBorderStyle
  -- , str
  -- , attrMap, withAttr, emptyWidget, AttrName, on, fg
  -- , (<+>)
  -- )
import Brick.AttrMap (attrName)
import Brick.BChan (newBChan, writeBChan)
import Brick.Widgets.Border (border, borderWithLabel, hBorderWithLabel, vBorder)
import Brick.Widgets.Border.Style (unicodeBold)
import Brick.Widgets.Center
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import Control.Lens ((^.),(&))
import qualified Graphics.Vty as V
import Graphics.Vty.CrossPlatform (mkVty)
import qualified Data.Sequence as S
import Linear.V2 (V2(..))

-- Types

-- | Ticks mark passing of time
--
-- This is our custom event that will be constantly fed into the app.
data Tick = Tick

-- | Named resources
--
-- Not currently used, but will be easier to refactor
-- if we call this "Name" now.
type Name = ()

data Cell = Snake1 | Snake2 | Food | Freezer | Empty | SnakeHead1 | SnakeHead2

-- App definition

app :: App Game Tick Name
app = App { appDraw = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return ()
          , appAttrMap = const theMap
          }
-- >>> [x | x <- [1..10], even x]
-- <interactive>:8655:2-27: warning: [-Wtype-defaults]
--     • Defaulting the following constraints to type ‘Integer’
--         (Show a0) arising from a use of ‘print’ at <interactive>:8655:2-27
--         (Integral a0) arising from a use of ‘it’ at <interactive>:8655:2-27
--     • In a stmt of an interactive GHCi command: print it
-- [2,4,6,8,10]
--

main :: IO ()
main = do
  p2 <- start
  chan <- newBChan 10
  _ <- forkIO $ forever $ do
    writeBChan chan Tick
    threadDelay 100000 -- decides how fast your game moves
  g <- initGame p2
  let builder = mkVty V.defaultConfig
  initialVty <- builder
  void $ customMain initialVty builder (Just chan) app g

-- Handling events

handleEvent :: BrickEvent Name Tick -> EventM Name Game ()
handleEvent (AppEvent Tick)                       = modify step
handleEvent (VtyEvent (V.EvKey V.KUp []))         = modify $ turn1 North
handleEvent (VtyEvent (V.EvKey V.KDown []))       = modify $ turn1 South
handleEvent (VtyEvent (V.EvKey V.KRight []))      = modify $ turn1 East 
handleEvent (VtyEvent (V.EvKey V.KLeft []))       = modify $ turn1 West 
handleEvent (VtyEvent (V.EvKey (V.KChar 'w') [])) = modify $ turn2 North
handleEvent (VtyEvent (V.EvKey (V.KChar 's') [])) = modify $ turn2 South
handleEvent (VtyEvent (V.EvKey (V.KChar 'd') [])) = modify $ turn2 East 
handleEvent (VtyEvent (V.EvKey (V.KChar 'a') [])) = modify $ turn2 West 
handleEvent (VtyEvent (V.EvKey (V.KChar 'r') [])) = do {g <- get; g' <- liftIO $ initGame $ g ^. p2mode; put g'; return ()}
handleEvent (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt
handleEvent (VtyEvent (V.EvKey V.KEsc []))        = halt
handleEvent (VtyEvent (V.EvKey (V.KChar 'p') [])) = modify pauseGame
handleEvent (VtyEvent (V.EvKey (V.KChar '.') [])) = modify $ applyReverseEffect 1
handleEvent (VtyEvent (V.EvKey (V.KChar 'g') [])) = modify $ applyReverseEffect 2
handleEvent (VtyEvent (V.EvKey (V.KChar 'P') [])) = modify pauseGame
handleEvent (VtyEvent (V.EvKey (V.KChar 'G') [])) = modify $ applyReverseEffect 2
handleEvent (VtyEvent (V.EvKey (V.KChar 'W') [])) = modify $ turn2 North
handleEvent (VtyEvent (V.EvKey (V.KChar 'S') [])) = modify $ turn2 South
handleEvent (VtyEvent (V.EvKey (V.KChar 'D') [])) = modify $ turn2 East 
handleEvent (VtyEvent (V.EvKey (V.KChar 'A') [])) = modify $ turn2 West 
handleEvent (VtyEvent (V.EvKey (V.KChar 'R') [])) = do {g <- get; g' <- liftIO $ initGame $ g ^. p2mode; put g'; return ()}
handleEvent (VtyEvent (V.EvKey (V.KChar 'Q') [])) = halt
handleEvent _                                     = return ()

-- Drawing

drawUI :: Game -> [Widget Name]
drawUI g =
  if g ^. p2mode then
    [ C.center $ padRight (Pad 2)  ((drawHelp g) <=> (drawTip g) <=> padLeft (Pad 7) (drawStats g)) <+> drawGrid g]
  else
    [ C.center $ padRight (Pad 2)  ((drawHelp g) <=> (drawTip g) <=> padLeft (Pad 5) (drawStats g)) <+> drawGrid g]

drawStats :: Game -> Widget Name
drawStats g = if g ^. p2mode then 
  hLimit 11
  $ vBox [ drawScore (g ^. score1) "p1Score"
         , drawScore (g ^. score2) "p2Score"
         , padTop (Pad 2) $ drawGameOver (g ^. dead) (g ^. paused) (g ^. winner)
         ]
  else
    hLimit 11
  $ vBox [ drawScore (g ^. score1) "Score"
         , padTop (Pad 2) $ drawGameOver (g ^. dead) (g ^. paused) (g ^. winner)
         ]


drawScore :: Int -> String -> Widget Name
drawScore n s = withBorderStyle BS.unicodeRounded
  $ B.borderWithLabel (str s)
  $ C.hCenter
  $ padAll 1
  $ str $ show n

drawGameOver :: Bool -> Bool -> String -> Widget Name
drawGameOver isDead isPause winnerStr =
  if isDead
     then withAttr gameOverAttr $ C.hCenter $ str $ winnerStr
     else if isPause
      then withAttr gameOverAttr $ C.hCenter $ str $ "GAME PAUSED\nPRESS A KEY\nTO CONTINUE"
      else emptyWidget

drawGrid :: Game -> Widget Name
drawGrid g = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "Snake")
  $ vBox rows
  where
    rows         = [hBox $ cellsInRow r | r <- [height-1,height-2..0]]
    cellsInRow y = [drawCoord (V2 x y) | x <- [0..width-1]]
    drawCoord    = drawCell . cellAt
    cellAt c
      | c `elem` g ^. snake1 && False == g ^. dead1 && isItemFirst c (g ^. snake1) = SnakeHead1
      | c `elem` g ^. snake1 && False == g ^. dead1                                = Snake1
      | c `elem` g ^. snake2 && False == g ^. dead2 && isItemFirst c (g ^. snake2) = SnakeHead2
      | c `elem` g ^. snake2 && False == g ^. dead2                                = Snake2
      | c == g ^. food                                                             = Food
      | c == g ^. freezer && False == g ^. dead1 && False == g ^. dead2            = Freezer
      | otherwise                                                                  = Empty

drawHelp :: Game -> Widget()
drawHelp g = if g ^. p2mode then
    [ "Player1 Move    : ↑←↓→"
    , "Player2 Move    : WASD"
    , "Player1 Reverse : ."
    , "Player2 Reverse : g"
    , "Quit            : Q"
    , "Restart         : R"
    , "Pause           : P"
    ]
    & unlines
    & str
    & borderWithLabel (str " Help ")
    & withBorderStyle unicodeBold
    & setAvailableSize (100, 50)
  else
    [ "Move        : ↑←↓→"
    , "Reverse     : ."
    , "Quit        : Q"
    , "Restart     : R"
    , "Pause       : P"
    ]
    & unlines
    & str
    & borderWithLabel (str " Help ")
    & withBorderStyle unicodeBold
    & setAvailableSize (100, 50)

drawTip :: Game -> Widget()
drawTip g = if g ^. p2mode then
      [ "RED DOT IS PLAYER 1"
      , "BLUE DOT IS PLAYER 2"
      , "YELLOW DOT IS FREEZER "
      , "GREEN DOT IS FOOD"
    ]
    & unlines
    & str
    & borderWithLabel (str " Tip ")
    & withBorderStyle unicodeBold
    & setAvailableSize (100, 50)
  else
    [ "BLUE DOT IS PLAYER"
    , "GREEN DOT IS FOOD"
    ]
    & unlines
    & str
    & borderWithLabel (str " Tip ")
    & withBorderStyle unicodeBold
    & setAvailableSize (100, 50)

drawCell :: Cell -> Widget Name
drawCell SnakeHead1 = withAttr snakeAttr1 cw1
drawCell SnakeHead2 = withAttr snakeAttr2 cw1
drawCell Snake1 = withAttr snakeAttr1 cw
drawCell Snake2 = withAttr snakeAttr2 cw
drawCell Food   = withAttr foodAttr cw
drawCell Freezer  = withAttr freezerAttr cw
drawCell Empty  = withAttr emptyAttr cw

cw :: Widget Name
cw = str "  "

cw1 :: Widget Name
cw1 = str "**"

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (snakeAttr1, V.blue `on` V.blue)
  , (snakeAttr2, V.red `on` V.red)
  , (snakeheadAttr1, V.blue `on` V.blue)
  , (snakeheadAttr2, V.red `on` V.red)
  , (foodAttr, V.green `on` V.green)
  , (freezerAttr, V.yellow `on` V.yellow)
  , (gameOverAttr, fg V.red `V.withStyle` V.bold)
  ]

gameOverAttr :: AttrName
gameOverAttr = attrName "gameOver"

snakeAttr1, snakeAttr2, foodAttr, emptyAttr, freezerAttr, snakeheadAttr1, snakeheadAttr2 :: AttrName
snakeAttr1 = attrName "snakeAttr1"
snakeAttr2 = attrName "snakeAttr2"
snakeheadAttr1 = attrName "snakeheadAttr1"
snakeheadAttr2 = attrName "snakeheadAttr2"
foodAttr  = attrName "foodAttr"
freezerAttr = attrName "freezerAttr"
emptyAttr = attrName "emptyAttr"

