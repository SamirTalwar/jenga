module MakeStyle (elaborate) where

import Data.List.Split (splitOn)
import ElabC qualified (macroC)
import Interface (G(..),Rule(..),Action(..),D(..),Key(..))
import Par4 (Position,Par,parse,position,skip,alts,many,some,sat,lit)
import StdBuildUtils ((</>),dirKey,baseKey)
import Text.Printf (printf)

elaborate :: Key -> G ()
elaborate config  = do
  allFilesRule
  elabRuleFile config
  where

    elabRuleFile :: Key -> G ()
    elabRuleFile config  = do
      s <- GReadKey config
      let clauses = Par4.parse gram s
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
    elabTrip Trip{pos,targets,deps,command} = do
      GRule $ Rule
        { tag = printf "rule@%s" (show pos)
        , hidden = False
        , targets = map makeKey targets
        , depcom = do sequence_ [ makeDep targets dep | dep <- deps ];  pure (bash command)
        }

    bash :: String -> Action
    bash command = Action { hidden = False, command }

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

    -- hidden rule so user-rules can access the list of file names
    allFilesName = "all.files"
    allFilesRule =  do
      let dir = dirKey config
      allFiles <- map Key <$> GGlob dir
      GRule (Rule { tag = printf "glob-%s" (show dir)
                  , hidden = True
                  , targets = [ makeKey allFilesName ]
                  , depcom = pure (Action
                                    { hidden = True
                                    , command = printf "echo -n '%s' > %s"
                                                (unlines (map baseKey allFiles))
                                                allFilesName
                                    })})


filterDepsFor :: [String] -> String -> [String]
filterDepsFor targets contents = do
  let
    parseDepsLine :: String -> [String]
    parseDepsLine line =
      case splitOn ":" line of
        -- If a 'deps' line contains a colon,
        -- we regard names on the right as the list of deps,
        -- but only take them if we target a name listed on the left.
        [left,right] -> do
          if any (`elem` targets) (words left) then words right else []
        _ -> do
          -- No colon: we take all the deps
          words line

  [ dep | line <- lines contents, dep <- parseDepsLine line ]


dispatch :: String -> Key -> G()
dispatch = \case
  "CC" -> ElabC.macroC
  name -> error (printf "unknown macro name: %s" name)

-- TODO: remove support for macros
data Clause = ClauseTrip Trip | ClauseMacro Macro | ClauseInclude String

data Macro = Macro { name :: String, arg :: String } -- TODO: multi arg macro

data Trip = Trip
  { pos :: Position
  , targets :: [String]
  , deps :: [Dep]
  , command :: String
  }

data Dep
  = DepPlain String     -- key
  | DepScanner String   -- @key
  | DepOpt String       -- ?key

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
      colon
      deps <- many dep
      alts [colon,newlineAndIndent]
      -- TODO: support comments in action body
      command <- singleCommandLine
      alts [nl,commentToEol]
      skip $ alts [nl,commentToEol]
      pure (ClauseTrip (Trip {pos,targets,deps,command}))

    -- traditional make syntax
    newlineAndIndent = do
      alts [nl,commentToEol]
      space -- at least one space char to begin the action
      skip space

    -- syntax to allow a rule on a single line
    colon = do
      lit ':'
      skip space

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

    singleCommandLine = do
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
