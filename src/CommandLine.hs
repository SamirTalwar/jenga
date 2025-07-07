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
  , reverseDepsOrder :: Bool -- experiment for concurrent jengas
  , buildMode :: BuildMode
  , args :: [FilePath]
  }

data BuildMode
  = ModeListTargets
  | ModeListRules
  | ModeBuild
  | ModeBuildAndRun FilePath [FilePath]

exec :: IO Config
exec = customExecParser
  (prefs (showHelpOnError <> showHelpOnEmpty))
  (info (subCommands <**> helper)
    ( fullDesc <> header "jenga: A build system" ))

subCommands :: Parser Config
subCommands =
  hsubparser
  (command "list-targets"
    (info listTargets
      (progDesc "List all targets")))
  <|>
  hsubparser
  (command "list-rules"
    (info listRules
      (progDesc "List all build rules")))
  <|>
  hsubparser
  (command "build"
    (info buildCommand
      (progDesc "Bring a build up to date")))
  <|>
  hsubparser
  (command "run"
    (info runCommand
      (progDesc "Build and run a single executable target")))


listTargets :: Parser Config
listTargets = sharedOptions
  <*> pure ModeListTargets
  <*>
  many (strArgument (metavar "DIRS"
                      <> help "directories containing build rules"))

listRules :: Parser Config
listRules = sharedOptions
  <*> pure ModeListRules
  <*>
  many (strArgument (metavar "DIRS"
                      <> help "directories containing build rules"))

buildCommand :: Parser Config
buildCommand = sharedOptions
  <*> pure ModeBuild
  <*>
  many (strArgument (metavar "DIRS"
                      <> help "directories containing build rules"))

runCommand :: Parser Config
runCommand = sharedOptions
  <*> (
  ModeBuildAndRun <$>
    strArgument (metavar "EXE-TARGET"
                 <> help "target to build and execute")
    <*>
    many (strArgument (metavar "EXE-ARG"
                       <> help "arguent to pass to target executable"))
  )
  <*>
  pure []


sharedOptions :: Parser (BuildMode -> [FilePath] -> Config)
sharedOptions = Config
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
  -- TODO: simpler to just always materialize all targets build?
  switch (short 'm' <> long "materialize-all"
          <> help "Materialize all targets; not just declared artifacts")
  <*>
  switch (long "reverse"
          <> help "Build dependencies in reverse order; experiment for concurrent jenga")
