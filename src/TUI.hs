{-# LANGUAGE LambdaCase #-}

module TUI (runTUI) where

import           System.IO       (hFlush, stdout)
import           System.IO.Error (catchIOError, isEOFError)

import           Core


data TUIState  a = Run { buffer   :: String
                       , ctlState :: a
                       }
                 | Stop a -- final state of Controller


tuiOutput :: Output (TUIState a -> TUIState a)
tuiOutput = Output { writeTo     = \msg s -> s { buffer = buffer s ++ msg }
                   , clearOutput = \    s -> s { buffer = [] }
                   , scrollTo    = \_   s -> s -- TUI can't scroll
                   }


runTUI :: Show a => [Action a] -> UI a
runTUI setup ctl =
  loop . perform setup . Run "" =<< initialize ctl
  where
    loop (Stop s) = finalize ctl s >> putStrLn "\nBye!"
    loop s        = do
      putStr (buffer s)

      let s' = s { buffer = [] }

      mbInput <- getInput

      loop =<< maybe
        (return $ Stop $ ctlState s')
        (\input -> do
            actions <- communicate ctl (ctlState s') input
            return $ perform actions s'
        )
        mbInput

    perform = flip (foldl apply)
      where
        apply s@(Stop _) _    = s
        apply s          Quit = Stop (ctlState s)
        apply s          act  =
          case act of
            Write msg   -> writeTo tuiOutput msg
            ClearOutput -> clearOutput tuiOutput
            ScrollTo t  -> tuiOutput `scrollTo` t
            SetState x  -> \s' -> s' { ctlState = x }
            SetInput _  -> id -- TUI can't set the input
            Quit        -> error "This case should't be reached!"
          $ s

getInput :: IO (Maybe String)
getInput = catchIOError
           (putStr "> " >> hFlush stdout >> Just <$> getLine)
           (\e -> if isEOFError e
                  then return Nothing
                  else ioError e)
