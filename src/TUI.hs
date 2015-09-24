{-# LANGUAGE LambdaCase #-}

module TUI (runTUI) where

import           System.IO       (hFlush, stdout)
import           System.IO.Error (catchIOError, isEOFError)

import           Core


type TUIState a = State String a


tuiOutput :: Output (TUIState a -> TUIState a)
tuiOutput = Output { writeTo     = \msg -> modUIState (++ msg)
                   , clearOutput =         modUIState (const [])
                   , scrollTo    = const id -- TUI can't scroll
                   }


runTUI :: Show a => [Action a] -> UI a
runTUI setup ctl =
  loop . perform tuiOutput setup . state "" =<< initialize ctl
  where
    loop s
      | finished s = finalize ctl (ctlState s)
                     >> putStrLn "\nBye!"
      | otherwise = do
        putStr (uiState s)

        let s' = modUIState (const []) s

        mbInput <- getInput

        loop =<< maybe
          (return $ finish s')
          (\input -> do
              actions <- communicate ctl (ctlState s') input
              return $ perform tuiOutput actions s'
          )
          mbInput


getInput :: IO (Maybe String)
getInput = catchIOError
           (putStr "> " >> hFlush stdout >> Just <$> getLine)
           (\e -> if isEOFError e
                  then return Nothing
                  else ioError e)
