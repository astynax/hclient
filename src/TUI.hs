{-# LANGUAGE LambdaCase #-}

module TUI (runTUI) where

import           Control.Monad (void, foldM)
import           Types


type TUIOutput = Output ([String] -> IO [String])
type TUIState a = (Bool, [String], a)


runTUI :: [Action a] -> UI a
runTUI setup ctl = do
  state <- initialize ctl

  let out = Output { writeTo     = \x s -> return $ s ++ [x]
                   , clearOutput = \_   -> return []
                   , scrollTo    = \_ s -> return s }

  void $ perform out (False, [], state) setup >>= loop ctl out

  where
    loop :: Controller IO a -> TUIOutput -> TUIState a -> IO (TUIState a)
    loop c _   state@(True, _, a) = finalize c a >> return state
    loop c out state@(_,    s, a) =
      mapM_ putStrLn s
      >> getLine
      >>= communicate c a
      >>= perform out state
      >>= loop c out

    perform out = foldM (apply out)

    apply :: Output (a -> IO a) -> (Bool, a, b) -> Action b -> IO (Bool, a, b)
    apply _ state@(True, _, _) _    = return state
    apply _       (_,    s, b) Quit = return (True, s, b)
    apply out     (_,    s, b) act  =
      case act of
        Write msg   -> with s b $ writeTo out msg
        ClearOutput -> with s b $ clearOutput out
        SetInput _  -> keep s b
        SetState b' -> keep s b'
        ScrollTo t  -> with s b $ out `scrollTo` t
        _           -> error "This case should't be reached!"

    with s b f = fmap (\s' -> (False, s', b)) (f s)
    keep = (return .) . (\s b -> (False, s, b))
