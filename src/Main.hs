module Main where

import           GUI
import           Types


main :: IO ()
main = runGUI "echo" (Just "Echo service is ready!\n\n") (1 :: Int) echo


echo :: (Show a, Num a) => Controller a
echo state msg =
  let (update, action) = case msg of
        ":q" -> (id,      Quit)
        ":r" -> (const 1, Clear)
        _    -> ((+ 1),   Write $ show state ++ ": " ++ msg ++ "\n")
  in  return (update state, Just "", [action])
