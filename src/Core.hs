module Core (
  Output(..), Input, UI
  , Action(..), Controller(..), Result, ScrollTarget(..)
  , (<>)
  , quit, clear, setInput, setState
  , write, writeLn
  , scrollToBegin, scrollToEnd
  , State(finished, ctlState, uiState, uiInput)
  , state, perform, finish
  , modUIState, modCtlState
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


-- | Pure State
data State a b = State { finished :: Bool
                       , uiInput  :: Maybe String
                       , uiState  :: a
                       , ctlState :: b
                       }

-- | "Words" of the controller eDSL

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


-- | Functions, helping to work with the pure state

state :: a -> b -> State a b
state = State False Nothing

perform :: Output (State a b -> State a b) -> [Action b] -> State a b -> State a b
perform out = flip (foldl apply)
  where
    apply s _ | finished s = s
    apply s Quit           = finish s
    apply s act            =
      case act of
        Write msg   -> writeTo out msg
        ClearOutput -> clearOutput out
        ScrollTo t  -> out `scrollTo` t
        SetState x  -> \s' -> s' { ctlState = x }
        SetInput x  -> \s' -> s' { uiInput = Just x }
        Quit        -> error "This case should't be reached!"
      $ s

modUIState :: (a -> a) -> State a b -> State a b
modUIState f s = s { uiState = f (uiState s) }

modCtlState :: (b -> b) -> State a b -> State a b
modCtlState f s = s { ctlState = f (ctlState s) }

finish :: State a b -> State a b
finish s = s { finished = True }
