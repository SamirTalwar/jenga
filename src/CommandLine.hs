module CommandLine (Config(..),Mode(..),exec) where

import Options.Applicative

-- TODO: maybe simplify loging verbosity: -v (A + count info) -vv (EBX) -vvv (I)
data Config = Config
  { seeE :: Bool
  , seeB :: Bool
  , seeA :: Bool
  , seeX :: Bool
  , seeI :: Bool
  , cacheParentOverride :: Maybe String -- Nothing means use default in $HOME
  , keepSandBoxes :: Bool
  , materializeAll :: Bool
  , reverseDepsOrder :: Bool -- experiment for concurent jengas
  , args :: [FilePath]
  , mode :: Mode
  } deriving Show

data Mode = ModeBuild | ModeListTargets | ModeListRules deriving Show

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

-- TODO: allow multiple setting of flags: -a -a
buildCommand :: Parser Config
buildCommand =
  Config <$> e <*> b <*> a <*> x <*> i <*> c <*> k <*> m <*> r <*> args <*> mode
  where
    -- TODO: consider removing the less useful short names
    e = switch (short 'e' <> help "Log steps for elaboration of targets and artifacts")
    b = switch (short 'b' <> help "Log steps for bringing a build up to date")
    a = switch (short 'a' <> help "Log execution of user build commands")
    x = switch (short 'x' <> help "Log execution of externally run commands")
    i = switch (short 'i' <> help "Log execution of internal file system access")

    c =
      Just <$> strOption
      (short 'c' <> long "cache" <> metavar "DIR"
        <> help "Use .cache/jenga in DIR instead of $HOME"
      )
      <|>
      pure Nothing

    k = switch (short 'k' <> long "keep-sandboxes"
                <> help "Keep all sandboxes when build completes")
    m = switch (short 'm' <> long "materialize-all"
                <> help "Materialize all targets; not just declared artifacts")

    r = switch (long "reverse"
                <> help "Build dependencies in reverse order; experiment for concurrent jenga")

    args = many (strArgument (metavar "DIRS"
                              <> help "directories containing build rules"))

    mode =
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
