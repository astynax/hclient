{-# LANGUAGE LambdaCase #-}

module TUI (runTUI) where

import           Control.Monad (void, foldM)
import           Types


type TUIAction a = a -> IO a
type TUIOutput a = Output (TUIAction a)


runTUI :: [Action a] -> UI a
runTUI setup ctl = do
  void $ perform setup
  loop =<< initialize ctl
  where
    loop = undefined
    perform = undefined
    {-
    perform state = mapM_ $ \case
      Write msg   -> writeTo out msg
      ClearOutput -> clearOutput out
      SetInput i  -> void $ entry # value i
      SetState s  -> writeIORef refState s
      ScrollTo t  -> out `scrollTo` t
      Quit        -> exit
    -}
