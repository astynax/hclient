module Types where

data Output = Output { write :: String -> IO ()
                     , clear :: IO () }

data Action = Write String
            | Clear
            | Quit

type Input = String

type Controller a =
  a                    -- initial state
  -> Input             -- current input
  -> IO ( a            -- new state
        , Maybe Input  -- new value for cmdline. Wouldn't change if Nothing
        , [Action])    -- list of actions to apply to output

type UI a = a -> Controller a -> IO ()
