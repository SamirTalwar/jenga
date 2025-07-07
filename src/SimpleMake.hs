module SimpleMake (elab) where

import Data.List.Split (splitOn)
import Data.Set ((\\))
import Data.Set qualified as Set
import ElabC qualified (macroC)
import Interface (G(..),Rule(..),Action(..),D(..),Key(..))
import Par4 (Position,Par,parse,position,skip,alts,many,some,sat,lit)
import StdBuildUtils ((</>),dirKey,baseKey)
import Text.Printf (printf)

dispatch :: String -> Key -> G()
dispatch = \case
  "CC" -> ElabC.macroC
  name -> error (printf "unknown macro name: %s" name)

-- TODO: consider passing the "dir" context as read-info in the G monad

elab :: Key -> G ()
elab config  = do
  -- _generateAllFileRule -- TODO: is there a better way?
  elabRuleFile config
  where

    elabRuleFile :: Key -> G ()
    elabRuleFile config  = do
      s <- GReadKey config
      let clauses = Par4.parse gram s
      sequence_ [ GArtifact (makeKey key) | key <- selectArtifacts clauses ]
      mapM_ elabClause clauses

    elabClause :: Clause -> G ()
    elabClause = \case
      ClauseTrip x -> elabTrip x
      ClauseMacro m -> elabMacro m
      ClauseInclude filename -> elabRuleFile (makeKey filename)

    elabMacro :: Macro -> G ()
    elabMacro = \case
      Macro{name,arg} -> dispatch name (makeKey arg)

    elabTrip :: Trip -> G ()
    elabTrip Trip{pos,targets,deps,action} = do
      GRule $ Rule
        { tag = printf "rule@%s" (show pos)
        , targets = map makeKey targets
        , depcom = do sequence_ [ makeDep targets dep | dep <- deps ];  pure (Bash action)
        }

    makeDep targets = \case
      DepPlain file -> DNeed (makeKey file)
      DepScanner file -> do
        let key = makeKey file
        contents <- DReadKey key
        let deps = filterDepsFor targets contents
        sequence_ [ DNeed (makeKey dep) | dep <- deps ]
      DepOpt file -> do
        let key = makeKey file
        b <- DExistsKey key
        if b then DNeed key else pure ()

    makeKey :: String -> Key
    makeKey basename = Key (dirKey config </> basename)

    -- special rule so user rules can access the list of all file names
    _generateAllFileRule =  do
      let dir = dirKey config
      allFiles <- map Key <$> GGlob dir
      GRule (Rule { tag = printf "glob-%s" (show dir)
                  , targets = [ makeKey allFilesName ]
                  , depcom = pure (Bash (printf "echo -n '%s' > %s"
                                         (unlines (map baseKey allFiles))
                                         allFilesName)) })

    allFilesName = "all.files"

    selectArtifacts :: [Clause] -> [String]
    selectArtifacts clauses = do
      -- Only considering clauses which define rule-triples...
      -- Any target which isn't a deps is considered an artifcat
      let targets = [ key | ClauseTrip (Trip{targets}) <- clauses, key <- targets ]
      let deps = [ keyOfDep dep
                 | ClauseTrip (Trip{deps}) <- clauses, dep <- deps ]
                 ++ [ allFilesName ]
      Set.toList (Set.fromList targets \\ Set.fromList deps)


-- TODO: maybe cleaner to use par4 parser to parse the scanner deps!!
filterDepsFor :: [String] -> String -> [String]
filterDepsFor targets contents = do
  let
    -- TODO: simplify/inline this code
    parse1 :: String -> Maybe ([String],[String])
    parse1 line =
      case splitOn ":" line of
        [left,right] -> Just (words left, words right)
        _ -> Nothing
    parse :: String -> [String]
    parse line =
      case (parse1 line) of
        Nothing -> [] -- comment or garbage we dont understand
        Just (as,bs) -> if any (`elem` targets) as then bs else []
  [ dep | line <- lines contents, dep <- parse line ]


data Clause = ClauseTrip Trip | ClauseMacro Macro | ClauseInclude String

data Macro = Macro { name :: String, arg :: String } -- TODO: multi arg macro

data Trip = Trip
  { pos :: Position
  , targets :: [String]
  , deps :: [Dep]
  , action :: String
  }

data Dep
  = DepPlain String     -- key
  | DepScanner String   -- @key
  | DepOpt String       -- ?key

keyOfDep :: Dep -> String
keyOfDep = \case DepPlain k -> k; DepScanner k -> k; DepOpt k -> k


-- grammar for traditional "make-style" triples, spread over two lines.
-- Extended to allow scanner deps of the form "@file"
-- And also simple macro calls: "Name(Arg)"

gram :: Par [Clause]
gram = start
  where
    start = do
      skip $ alts [space,nl,commentToEol]
      many clause

    clause = do
      pos <- position
      x <- identifier
      if x == "include" then includeClause else
        alts [triple pos x, macroCall pos x]

    includeClause = do
      fileName <- identifier
      skip space
      alts [nl,commentToEol]
      skip $ alts [nl,commentToEol]
      pure (ClauseInclude fileName)

    macroCall _pos name = do
      lit '('
      skip space
      arg <- identifier
      lit ')'
      skip space
      alts [nl,commentToEol]
      skip $ alts [nl,commentToEol]
      pure (ClauseMacro (Macro {name,arg}))

    triple pos target1 = do
      moreTargets <- many identifier
      let targets = target1 : moreTargets
      lit ':'
      skip space
      deps <- many dep
      alts [nl,commentToEol]
      space -- at least one space char to begin the action
      skip space
      -- TODO: support comments in action body
      action <- singleAcionLine
      alts [nl,commentToEol]
      skip $ alts [nl,commentToEol]
      pure (ClauseTrip (Trip {pos,targets,deps,action}))

    dep = do
      alts [ do lit '@'; x <- identifier; pure (DepScanner x)
           , do lit '?'; x <- identifier; pure (DepOpt x)
           , DepPlain <$> identifier ]

    identifier = do
      res <- some identifierChar
      skip space -- post skip space
      pure res

    identifierChar = sat (not . specialChar)

    specialChar = (`elem` " :#()\n")

    singleAcionLine = do
      trimTrailingSpace <$> some actionChar

    actionChar = sat $ \case -- anything upto a comment or NL
      '#' -> False
      '\n' -> False
      _ -> True

    trimTrailingSpace = reverse . dropWhile (==' ') . reverse

    space = lit ' '
    nl = lit '\n'

    commentToEol = do
      lit '#'
      skip notNL
      lit '\n'

    notNL = do
      _ <- sat (\case '\n' -> False; _ -> True)
      pure ()
