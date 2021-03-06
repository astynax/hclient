module Main where

import           Control.Monad       (void)
import qualified Data.Text           as T
import           Options.Applicative
import           System.Process      (readCreateProcessWithExitCode, shell)
import           System.Exit         (ExitCode(ExitSuccess))

import           HTk.Toplevel.HTk    (text, size, configure)

import           UI.Dialogui
import           UI.Dialogui.HTk
import           UI.Dialogui.TUI


data Config = Config Bool String String

type Template = String -> String


main :: IO ()
main = cli >>= \(ui, tpl) -> ui [] (controller tpl)


controller :: Template -> Controller IO ()
controller format =
  voidController { communicate = \_ input -> do
                      (code, out, err) <- readCreateProcessWithExitCode
                                          (shell $ format input) []
                      let msg = if code == ExitSuccess
                                then out
                                else err
                      return $ clear <> write msg <> scrollToBegin
                 }


cli :: IO ([Action ()] -> UI (), Template)
cli = do
  Config useTUI repl cmdLine <- execParser options

  let ui =
        if useTUI
        then runTUI
        else runGUIWith
             $ defaultOptions { prepareWindow = set [ text "hClient" ]
                              , prepareOutput = set [ size (80, 25) ] }

      template =
        if null repl
        then \x -> cmdLine ++ ' ' : x
        else \x -> replace repl x cmdLine

  return (ui, template)

  where
    options = info (helper <*> config)
              ( fullDesc
                <> progDesc "Execs the command with args interactively"
                <> header "hCLIent - GUI for CLI-commands!" )
    config = Config
             <$> flag False True
             ( short 'T'
               <> long "tui"
               <> help "use Text User Interface instead of GUI (Tk)" )
             <*> strOption
             ( short 'I'
               <> metavar "STR"
               <> help ("substring of CMDLINE which will be replaced " ++
                        "with the user input (in GUI)")
               <> value "" )
             <*> argument str
             ( metavar "CMDLINE" )

    replace sub repl src =
      T.unpack $ T.replace (T.pack sub) (T.pack repl) (T.pack src)

    set = (void .) . flip configure
