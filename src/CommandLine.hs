module CommandLine (Config(..),exec) where

import Options.Applicative

data Config = Config
  { seeE :: Bool
  , seeB :: Bool
  , seeA :: Bool
  , seeX :: Bool
  , seeI :: Bool
  , keepSandBoxes :: Bool
  , materializeAll :: Bool
  , args :: [FilePath]
  } deriving Show

exec :: IO Config
exec = customExecParser
  (prefs (showHelpOnError <> showHelpOnEmpty))
  (info (subCommands <**> helper)
    ( fullDesc <> header "jenga: A build system" ))

subCommands :: Parser Config
subCommands = hsubparser
  (command "build"
    (info buildCommand
      (progDesc "Bring a build up to date")))

buildCommand :: Parser Config
buildCommand = Config <$> e <*> b <*> a <*> x <*> i <*> k <*> m <*> args
  where
    e = switch (short 'e' <> help "Log steps for elaboration of targets and artifacts")
    b = switch (short 'b' <> help "Log steps for bringing a build up to date")
    a = switch (short 'a' <> help "Log execution of user build commands")
    x = switch (short 'x' <> help "Log execution of externally run commands")
    i = switch (short 'i' <> help "Log execution of internal file system access")
    k = switch (short 'k' <> long "keep-sandboxes"
                <> help "Keep all sandboxes when build completes")
    m = switch (short 'm' <> long "materialize-all"
                <> help "Materialize all targets; not just declared artifacts")
    args = many (strArgument (metavar "DIRS"
                              <> help "directories containing build rules"))
