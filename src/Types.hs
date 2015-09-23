module Types (
  Output(..), Input, UI
  , Action(..), Controller(..), Result, ScrollTarget(..)
  , (<>)
  , quit, clear, setInput, setState
  , write, writeLn
  , scrollToBegin, scrollToEnd
  ) where

import           Data.Monoid ((<>))


data Output t = Output { writeTo     :: String -> t
                       , clearOutput :: t
                       , scrollTo    :: ScrollTarget -> t }

type UI a = Controller IO a -> IO ()

data ScrollTarget = BeginOfText | EndOfText

data Action a = Write String
              | ClearOutput
              | SetInput String
              | SetState a
              | ScrollTo ScrollTarget
              | Quit

type Input = String

type Result a = [Action a]


data Monad m => Controller m a =
  Controller { initialize  :: m a
             , finalize    :: a -> m ()
             , communicate :: a             -- previous state
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

scrollToBegin, scrollToEnd :: Result a
scrollToBegin = [ScrollTo BeginOfText]
scrollToEnd   = [ScrollTo EndOfText]
