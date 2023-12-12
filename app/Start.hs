module Start
  ( start
  ) where

import System.Exit (exitSuccess)
-- import Control.Monad (when)

import Brick
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Graphics.Vty as V

app :: App (Maybe Int) e ()
app = App
  { appDraw         = const [ui]
  , appHandleEvent  = handleEvent
  , appStartEvent   = pure ()
  , appAttrMap      = const $ attrMap V.defAttr []
  , appChooseCursor = neverShowCursor
  }

ui :: Widget ()
ui = C.center $ str "press ENTER to start"

handleEvent :: BrickEvent () e -> EventM () (Maybe Int) ()
handleEvent (VtyEvent (V.EvKey V.KEsc        _)) = halt
handleEvent (VtyEvent (V.EvKey (V.KChar 'q') _)) = halt
handleEvent (VtyEvent (V.EvKey (V.KChar 'Q') _)) = halt
handleEvent (VtyEvent (V.EvKey V.KEnter      _)) = do {put $ Just 1; halt}
handleEvent _ = pure ()

start :: IO Int
start = defaultMain app Nothing >>= maybe exitSuccess return