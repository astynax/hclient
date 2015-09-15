{-# LANGUAGE LambdaCase #-}

module GUI (runGUI) where

import           Control.Monad    (void)
import           Data.Foldable    (forM_)
import           Data.IORef       (newIORef, readIORef, writeIORef)
import           HTk.Toplevel.HTk

import           Types


runGUI :: String -> Maybe String -> UI a
runGUI title intro initialState process = do
  tk <- initHTk [ text title
                , minSize (300, 150)]

  refState <- newIORef initialState

  entry <- newEntry tk [] :: IO (Entry String)
  (outFrame, out) <- newOutput tk

  (escaped, _) <- bind entry
                  [WishEvent [Control] $ KeyPress $ Just $ KeySym "d"]

  (entered, _) <- bindSimple entry
                  $ KeyPress $ Just $ KeySym "Return"

  pack entry    [ Fill X ]
  pack outFrame [ Fill Both
                , Expand On ]

  void $ spawnEvent $ forever
    $  escaped >>> destroy tk
    +> entered >>> do
      cmd <- getValue entry :: IO String
      st <- readIORef refState
      (st', mbEntry, actions) <- process st cmd
      forM_ mbEntry (void . (entry #) . value)
      forM_ actions $ \case
        Write msg -> write out msg
        Clear     -> clear out
        Quit      -> destroy tk
      writeIORef refState st'

  forM_ intro (write out)

  setFocus entry

  finishHTk

  where
    newOutput :: Container a => a -> IO (Frame, Output)
    newOutput cont = do
      f  <- newFrame     cont []
      sb <- newScrollBar f    []
      ed <- newEditor    f    [ scrollbar Vertical sb
                              , wrap WordWrap
                              , width 10
                              , disable ]
      pack ed [ Side AtLeft
              , Fill Both
              , Expand On ]
      pack sb [ Side AtRight
              , Fill Y ]
      return (f, Output { write = enabling ed
                                  . appendText ed
                        , clear = enabling ed
                                  $ deleteTextRange ed
                                  ((0, 0) :: Position) EndOfText
                        })


enabling :: HasEnable a => a -> IO b -> IO ()
enabling w action = void $
  configure w [enable]
  >> action
  >> configure w [disable]
