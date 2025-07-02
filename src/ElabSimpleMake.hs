module ElabSimpleMake (elab) where

import Data.List.Split (splitOn)
import Data.Set ((\\))
import Data.Set qualified as Set
import ElabC qualified (macroC)
import Interface (G(..),Rule(..),Action(..),D(..),Key(..))
import Par4 (Position,Par,parse,position,skip,alts,many,some,sat,lit)
import StdBuildUtils ((</>),dirKey)
import Text.Printf (printf)

dispatch :: String -> Key -> G()
dispatch = \case
  "CC" -> ElabC.macroC
  name -> error (printf "unknown macro name: %s" name)

elab :: Key -> G ()
elab config  = do
  s <- GReadKey config
  let clauses = Par4.parse gram s
  sequence_ [ GArtifact (makeKey key) | key <- selectArtifacts clauses ]
  mapM_ elabClause clauses
    where
      elabClause :: Clause -> G ()
      elabClause = \case
        ClauseTrip x -> elabTrip x
        ClauseMacro m -> elabMacro m

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
        Dep file -> DNeed (makeKey file)
        Scanner file -> do
          let key = makeKey file
          contents <- DReadKey key
          let deps = filterDepsFor targets contents
          sequence_ [ DNeed (makeKey dep) | dep <- deps ]

      makeKey :: String -> Key
      makeKey basename = Key (dirKey config </> basename)


selectArtifacts :: [Clause] -> [String]
selectArtifacts clauses = do
  -- Only considering clauses which define rule-triples...
  -- Any target which isn't a deps is considered an artifcat
  let targets = [ key | ClauseTrip (Trip{targets}) <- clauses, key <- targets ]
  let deps = [ case dep of Dep k -> k; Scanner k -> k
             | ClauseTrip (Trip{deps}) <- clauses, dep <- deps ]
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


data Clause = ClauseTrip Trip | ClauseMacro Macro

data Macro = Macro { name :: String, arg :: String } -- TODO: multi arg macro

data Trip = Trip
  { pos :: Position
  , targets :: [String]
  , deps :: [Dep]
  , action :: String
  }

data Dep = Dep String | Scanner String


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
      alts [triple pos x, macroCall pos x]

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
      alts [ do lit '@'; x <- identifier; pure (Scanner x)
           , Dep <$> identifier ]

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
