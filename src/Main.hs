module Main where

import           GUI
import           Types


main :: IO ()
main = runGUI "echo" (Just "Echo service is ready!\n\n") 1 echo


echo :: (Monad m) => Controller m Int
echo = Controller $ \cnt msg ->
  case msg of
    ""   -> return $ write
            $ unlines [ "Use:"
                      , " :q    - to quit (Ctrl+D works same way)"
                      , " :r    - to reset state"
                      , " <msg> - to see an echo" ]
    ":q" -> quit
    ":r" -> clear
    _    -> return
            $ withState (cnt + 1)
            $ consuming
            $ write $ show cnt ++ ": " ++ msg ++ "\n"
