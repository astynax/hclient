{-# LANGUAGE LambdaCase #-}

module GUI (runGUI) where

import           Control.Monad    (void)
import           Data.Foldable    (forM_)
import           Data.IORef       (newIORef, readIORef, writeIORef)
import           HTk.Toplevel.HTk

import           Types


runGUI :: String -> Maybe String -> UI IO a
runGUI title intro initialState ctl = do
  main <- initHTk [ text title
                  , minSize (300, 150)]
  let exit = destroy main

  refState <- newIORef initialState

  entry           <- newEntry main [] :: IO (Entry String)
  (outFrame, out) <- newOutput main

  pack entry    [ Fill X ]
  pack outFrame [ Fill Both
                , Expand On ]

  onCtrlD  <- hotkey entry [Control] "d"
  onReturn <- hotkey entry []        "Return"

  void $ spawnEvent $ forever

    $  onCtrlD  >>> exit

    +> onReturn >>> do
      cmd <- getValue entry :: IO String

      oldState <- readIORef refState
      Result mbNewState mbEntry actions <- runController ctl oldState cmd

      forM_ mbEntry (void . (entry #) . value)

      forM_ actions $ \case
        Write msg -> writeTo out msg
        Clear     -> clearOutput out
        Quit      -> exit

      forM_ mbNewState (writeIORef refState)

  forM_ intro (writeTo out)

  setFocus entry

  finishHTk

  where
    hotkey w mods key =
      fst <$> bind w [WishEvent mods $ KeyPress $ Just $ KeySym key]

    newOutput :: Container a => a -> IO (Frame, Output)
    newOutput cont = do
      frame  <- newFrame     cont  []
      sb     <- newScrollBar frame []
      ed     <- newEditor    frame [ scrollbar Vertical sb
                                   , wrap WordWrap
                                   , width 10
                                   , disable ]
      pack ed [ Side AtLeft
              , Fill Both
              , Expand On ]
      pack sb [ Side AtRight
              , Fill Y ]

      return ( frame
             , Output { writeTo = enabling ed
                                  . appendText ed
                      , clearOutput = enabling ed
                                      $ deleteTextRange ed
                                      ((0, 0) :: Position) EndOfText
                      })


-- | Temporarily enables the widget until the completion of action
enabling :: HasEnable a => a -> IO b -> IO ()
enabling w action = void $
  configure w [enable]
  >> action
  >> configure w [disable]
