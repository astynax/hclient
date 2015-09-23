{-# LANGUAGE LambdaCase #-}

module Main where

import           TUI  -- GUI
import           Types


main :: IO ()
main = runTUI (writeLn "Echo service is ready!") echo


echo :: (Monad m) => Controller m Int
echo = Controller { initialize  = return 1
                  , finalize    = const $ return ()
                  , communicate = (return .) . communicate' }
  where
    communicate' cnt = \case
      ""   -> showHelp

      ":?" -> showHelp

      ":q" -> quit

      ":r" -> clear <> clearInput

      msg  -> setState (cnt + 1)
              <> clearInput
              <> write (show cnt ++ ": " ++ msg)

    showHelp = write
               $ unlines [ "Use:"
                         , " \":?\" to show this help"
                         , " \":q\" to quit (Ctrl+D works same way)"
                         , " \":r\" to reset state"
                         , " <msg> to see an echo" ]
    clearInput = setInput ""
