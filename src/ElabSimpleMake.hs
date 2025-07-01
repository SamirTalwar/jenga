module ElabSimpleMake (elab) where

import Data.Set ((\\))
import Data.Set qualified as Set
import Interface (G(..),Rule(..),Action(..),D(..),Key(..))
import Par4 (Position,Par,parse,position,skip,alts,many,some,sat,lit)
import StdBuildUtils ((</>),dirKey)
import Text.Printf (printf)

elab :: Key -> G ()
elab config  = do
  s <- GReadKey config
  let trips = parse gram s
  -- All targets which aren't also deps are considered to be artifcats
  let targets = [ key | Trip{targets} <- trips, key <- targets ]
  let deps = [ key | Trip{deps} <- trips, key <- deps ]
  let artifacts = Set.toList (Set.fromList targets \\ Set.fromList deps)
  sequence_ [ GArtifact (makeKey key) | key <- artifacts ]
  mapM_ elabTrip trips
    where
      elabTrip :: Trip -> G ()
      elabTrip Trip{pos,targets,deps,action} = do
        GRule $ Rule
          { tag = printf "rule@%s" (show pos)
          , targets = map makeKey targets
          , depcom = do sequence_ [ DNeed (makeKey dep) | dep <- deps ];  pure (Bash action)
          }

      makeKey :: String -> Key
      makeKey basename = Key (dirKey config </> basename)

data Trip = Trip
  { pos :: Position
  , targets :: [String]
  , deps :: [String]
  , action :: String
  }

-- grammar for traditional "make-style" triples, spread over two lines
gram :: Par [Trip]
gram = start
  where
    start = do
      skip $ alts [space,nl,commentToEol]
      many trip

    trip = do
      pos <- position
      targets <- many identifier
      lit ':'
      skip space
      deps <- many identifier
      alts [nl,commentToEol]
      space -- at least one space char to begin the action
      skip space
      action <- singleAcionLine
      alts [nl,commentToEol]
      skip $ alts [nl,commentToEol]
      pure $ Trip {pos,targets,deps,action}

    identifier = do
      res <- some identifierChar
      skip space -- post skip space
      pure res

    identifierChar = sat $ \case -- anything but the four special chars
      ' ' -> False
      ':' -> False
      '#' -> False
      '\n' -> False
      _ -> True

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
