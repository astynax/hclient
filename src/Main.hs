module Main where

import qualified Data.Text           as T
import           Options.Applicative
import           System.Process      (readCreateProcessWithExitCode, shell)
import           System.Exit         (ExitCode(ExitSuccess))

import           GUI
import           Types


data Config = Config String String

type Template = String -> String


main :: IO ()
main = cli >>= runGUI "hCLIent" [] () . controller


controller :: Template -> Controller IO ()
controller format = Controller $ \_ input -> do
  (code, out, err) <- readCreateProcessWithExitCode (shell $ format input) []
  let msg = if code == ExitSuccess
            then out
            else err
  return $ clear <> write msg <> scrollToBegin


cli :: IO Template
cli = do
  Config repl cmdLine <- execParser options

  return $ if null repl
           then \x -> cmdLine ++ ' ' : x
           else \x -> replace repl x cmdLine

  where
    options = info (helper <*> config)
              ( fullDesc
                <> progDesc "Execs the command with args interactively"
                <> header "hCLIent - GUI for CLI-commands!" )
    config = Config
             <$> strOption
             ( short 'I'
               <> metavar "STR"
               <> help ("substring of CMDLINE which will be replaced " ++
                        "with the user input (in GUI)")
               <> value "" )
             <*> argument str
             ( metavar "CMDLINE" )

    replace sub repl src =
      T.unpack $ T.replace (T.pack sub) (T.pack repl) (T.pack src)
