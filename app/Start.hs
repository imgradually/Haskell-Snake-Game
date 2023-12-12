module Start
  ( start
  ) where

import System.Exit (exitSuccess)
import Control.Monad (when)

import Brick
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Graphics.Vty as V

app :: App (Maybe Bool) e ()
app = App
  { appDraw         = const [ui]
  , appHandleEvent  = handleEvent
  , appStartEvent   = pure ()
  , appAttrMap      = const $ attrMap V.defAttr []
  , appChooseCursor = neverShowCursor
  }

ui :: Widget ()
ui = C.center 
  $ vBox [ C.hCenter $ str "WELCOME TO THE SNAKE"
    , C.hCenter $ padTop (Pad 1) $ str "press 1 to play in single player mode\npress 2 to play in double player mode"]

handleEvent :: BrickEvent () e -> EventM () (Maybe Bool) ()
handleEvent (VtyEvent (V.EvKey V.KEnter      _)) = do {put $ Just False; halt}
handleEvent (VtyEvent (V.EvKey V.KEsc        _)) = halt
handleEvent (VtyEvent (V.EvKey (V.KChar 'q') _)) = halt
handleEvent (VtyEvent (V.EvKey (V.KChar 'Q') _)) = halt
handleEvent (VtyEvent (V.EvKey (V.KChar '1') _)) = do {put $ Just False; halt}
handleEvent (VtyEvent (V.EvKey (V.KChar '2') _)) = do {put $ Just True; halt}
handleEvent _ = pure ()

start :: IO Bool
start = defaultMain app Nothing >>= maybe exitSuccess return