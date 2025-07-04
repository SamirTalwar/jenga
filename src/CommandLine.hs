module CommandLine (LogMode(..),Config(..),BuildMode(..),exec) where

import Options.Applicative

data LogMode = LogQuiet | LogNormal | LogVerbose

data Config = Config
  { logMode :: LogMode
  , seeX :: Bool
  , seeI :: Bool
  , cacheParentOverride :: Maybe String -- Nothing: means use default in $HOME
  , keepSandBoxes :: Bool
  , materializeAll :: Bool
  , reverseDepsOrder :: Bool -- experiment for concurent jengas
  , args :: [FilePath]
  , buildMode :: BuildMode
  }

data BuildMode = ModeBuild | ModeListTargets | ModeListRules

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
buildCommand = Config
  <$>
  (
    flag' LogVerbose
    (short 'v' <> help "Log build-targets checked")
    <|>
    flag' LogQuiet
    (short 'q' <> help "Build quietly, except for errors")
    <|>
    pure LogNormal -- see user actions + counts
  )
  <*> switch (short 'x' <> help "Log execution of externally run commands")
  <*> switch (short 'i' <> help "Log execution of internal file system access")
  <*> (
  Just <$> strOption
    (short 'c' <> long "cache" <> metavar "DIR"
     <> help "Use .cache/jenga in DIR instead of $HOME"
    )
    <|> pure Nothing
  )
  <*>
  switch (short 'k' <> long "keep-sandboxes"
          <> help "Keep all sandboxes when build completes")
  <*>
  switch (short 'm' <> long "materialize-all"
          <> help "Materialize all targets; not just declared artifacts")
  <*>
  switch (long "reverse"
          <> help "Build dependencies in reverse order; experiment for concurrent jenga")
  <*>
  many (strArgument (metavar "DIRS"
                      <> help "directories containing build rules"))
  <*> (
  flag' ModeListTargets
    (short 'l'
      <> long "list-targets"
      <> help "List all buildable targets")
    <|>
    flag' ModeListRules
    (short 'r'
      <> long "list-rules"
      <> help "List all rules (building scanner dependecies if necessary)")
    <|>
    pure ModeBuild
  )
