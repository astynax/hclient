module Main where

import           GUI
import           Types


main :: IO ()
main = runGUI (Just "Echo service ready!\n\n") (1 :: Int) echo


echo :: (Show a, Num a) => Controller a
echo s msg =
  let (f, action) = case msg of
        ":q" -> (id,      Quit)
        ":r" -> (const 1, Clear)
        _    -> ((+ 1),   Write $ show s ++ ": " ++ msg ++ "\n")
  in  return (f s, Just "", [action])
