module CommandLine
  ( LogMode(..),Config(..),BuildMode(..), CacheDirSpec(..)
  , exec
  ) where

import Options.Applicative

data LogMode = LogQuiet | LogActions | LogVerbose

data Config = Config
  { logMode :: LogMode
  , seeX :: Bool
  , seeI :: Bool
  , cacheDirSpec :: CacheDirSpec
  , keepSandBoxes :: Bool
  , reverseDepsOrder :: Bool -- experiment for concurrent jengas
  , buildMode :: BuildMode
  , args :: [FilePath]
  }

data CacheDirSpec = CacheDirDefault | CacheDirChosen String | CacheDirTemp

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
listTargets = sharedOptions LogQuiet
  <*> pure ModeListTargets
  <*>
  many (strArgument (metavar "DIRS"
                      <> help "directories containing build rules"))

listRules :: Parser Config
listRules = sharedOptions LogQuiet
  <*> pure ModeListRules
  <*>
  many (strArgument (metavar "DIRS"
                      <> help "directories containing build rules"))

buildCommand :: Parser Config
buildCommand = sharedOptions LogActions
  <*>
  (
    flag' ModeListTargets
    (short 't'
      <> long "list-targets"
      <> help "List buildable targets")
    <|>
    flag' ModeListRules
    (short 'r'
      <> long "list-rules"
      <> help "List generated rules")
    <|>
    pure ModeBuild
  )
  <*>
  many (strArgument (metavar "DIRS"
                      <> help "directories containing build rules"))

runCommand :: Parser Config
runCommand = sharedOptions LogQuiet
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


sharedOptions :: LogMode -> Parser (BuildMode -> [FilePath] -> Config)
sharedOptions defaultLogMode = Config
  <$>
  (
    flag' LogVerbose
    (short 'v' <> help "Log build-targets checked")
    <|>
    flag' LogActions
    (short 'a' <> help "Build showing actions run")
    <|>
    flag' LogQuiet
    (short 'q' <> help "Build quietly, except for errors")
    <|>
    pure defaultLogMode
  )
  <*> switch (short 'x' <> help "Log execution of externally run commands")
  <*> switch (short 'i' <> help "Log execution of internal file system access")
  <*>
  (
    CacheDirChosen <$> strOption
    (short 'c' <> long "cache" <> metavar "DIR"
     <> help "Use .cache/jenga in DIR instead of $HOME"
    )
    <|>
    flag' CacheDirTemp
    (short 'f' <> long "temporary-cache"
     <> help "Build using temporary cache to force build actions")
    <|> pure CacheDirDefault
  )
  <*>
  switch (short 'k' <> long "keep-sandboxes"
          <> help "Keep sandboxes when build completes")
  <*>
  switch (long "reverse"
          <> help "Reverse dependencies ordering; dev experiment!")
