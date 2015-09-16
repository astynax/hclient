module Types (
  Output(..), Input, UI
  , Action(..), Controller(..), Result
  , (<>)
  , quit, clear, setInput, setState
  , write, writeLn
  ) where

import           Data.Monoid ((<>))


data Output = Output { writeTo     :: String -> IO ()
                     , clearOutput :: IO () }

type UI a = a -> Controller IO a -> IO ()


data Action a = Write String
              | ClearOutput
              | SetInput String
              | SetState a
              | Quit

type Input = String

type Result a = [Action a]


newtype Monad m => Controller m a =
  Controller { runController ::
                  a                -- previous state
                  -> Input         -- current input
                  -> m (Result a)  -- result
             }

quit, clear :: Result a
quit     = [Quit]
clear    = [ClearOutput]

write, writeLn, setInput :: String -> Result a
write    = (:[]) . Write
writeLn  = (:[]) . Write . (++ "\n")
setInput = (:[]) . SetInput

setState :: a -> Result a
setState = (:[]) . SetState
