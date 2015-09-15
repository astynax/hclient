module Types where


data Output = Output { writeTo     :: String -> IO ()
                     , clearOutput :: IO () }

data Action = Write String
            | Clear
            | Quit

type Input = String

data Result a =
  Result
  (Maybe a)      -- new state, if changed
  (Maybe Input)  -- new value for cmdline. Wouldn't change if Nothing
  [Action]       -- list of actions to apply to output


newtype Monad m => Controller m a =
  Controller { runController ::
                  a                -- previous state
                  -> Input         -- current input
                  -> m (Result a)  -- result
             }

type UI m a = a -> Controller m a -> m ()


quit, clear :: Monad m => m (Result a)
quit  = return $ Result Nothing Nothing [Quit]
clear = return $ Result Nothing Nothing [Clear]

consuming :: Result a -> Result a
consuming (Result s _ as) = Result s (Just "") as

withState :: a -> Result a -> Result a
withState s (Result _ i as) = Result (Just s) i as

write :: String -> Result a
write s = Result Nothing Nothing [Write s]
