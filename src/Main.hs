module Main where

import           GUI
import           Types


main :: IO ()
main = runGUI "echo" (writeLn "Echo service is ready!\n") 1 echo


echo :: (Monad m) => Controller m Int
echo = Controller $ \cnt msg -> return $
  case msg of
    ""   -> showHelp

    ":?" -> showHelp

    ":q" -> quit

    ":r" -> clear <> clearInput

    _    -> setState (cnt + 1)
            <> clearInput
            <> write (show cnt ++ ": " ++ msg ++ "\n")
  where
    showHelp = write
               $ unlines [ "Use:"
                         , " \":?\" to show this help"
                         , " \":q\" to quit (Ctrl+D works same way)"
                         , " \":r\" to reset state"
                         , " <msg> to see an echo" ]
    clearInput = setInput ""
