{-# LANGUAGE LambdaCase #-}

module GUI (runGUI) where

import           Control.Monad    (void)
import           Data.IORef       (newIORef, readIORef, writeIORef)
import           HTk.Toplevel.HTk hiding (EndOfText)
import qualified HTk.Toplevel.HTk as H

import           Types


runGUI :: String -> [Action a] -> UI a
runGUI title setup ctl = do
  main <- initHTk [ text title
                  , minSize (300, 100) ]

  refState <- newIORef =<< initialize ctl

  let exit = readIORef refState >>= finalize ctl >> destroy main

  entry           <- newEntry main [] :: IO (Entry String)
  (outFrame, out) <- newOutput main

  pack entry    [ Fill X ]
  pack outFrame [ Fill Both
                , Expand On ]

  let perform = mapM_ $ \case
        Write msg   -> writeTo out msg
        ClearOutput -> clearOutput out
        SetInput i  -> void $ entry # value i
        SetState s  -> writeIORef refState s
        ScrollTo t  -> out `scrollTo` t
        Quit        -> exit

  onCtrlD  <- hotkey entry [Control] "d"
  onReturn <- hotkey entry []        "Return"

  void $ spawnEvent $ forever

    $  onCtrlD  >>> exit

    +> onReturn >>> do
      cmd <- getValue entry :: IO String

      oldState <- readIORef refState
      actions <- communicate ctl oldState cmd

      perform actions

  perform setup

  setFocus entry

  finishHTk

  where
    hotkey w mods key =
      fst <$> bind w [WishEvent mods $ KeyPress $ Just $ KeySym key]

    newOutput :: Container a => a -> IO (Frame, Output (IO ()))
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
                                      ((0, 0) :: Position) H.EndOfText
                      , scrollTo = moveto Vertical ed
                                   . \case
                                     BeginOfText -> 0
                                     EndOfText   -> 1
                      })


-- | Temporarily enables the widget until the completion of action
enabling :: HasEnable a => a -> IO b -> IO ()
enabling w action = void $
  configure w [enable]
  >> action
  >> configure w [disable]
