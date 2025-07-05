module P (P(..),runP,parallel) where

import Control.Monad (ap,liftM)

import Data.HMap (HMap)
import Data.HMap qualified as HMap (empty,createKey,lookup,insert)

instance Functor P where fmap = liftM
instance Applicative P where pure = PRet; (<*>) = ap
instance Monad P where (>>=) = PBind

parallel :: [P a] -> P [a]
parallel = \case
  [] -> pure []
  [p] -> (\x->[x]) <$> p
  p:ps -> (\(x,xs) -> x:xs) <$> Par p (parallel ps)

data P a where
  PRet :: a -> P a
  PBind :: P a -> (a -> P b) -> P b
  PIO :: IO a -> P a
  NonPar :: P a -> P b -> P (a,b)
  Par :: P a -> P b -> P (a,b)
  Yield :: P ()

runP :: P a -> IO a
runP p = loop p state0 k0
  where
    k0 :: PState r -> r -> IO r
    k0 _s@PState{active,blocked} a =
      case (active,blocked) of
        ([],[]) -> pure a
        _ -> error "unexpected jobs remain"
        -- _ -> next _s -- ?

    state0 = PState { active = [], blocked = [], hmap = HMap.empty }

    loop :: P a -> PState r -> (PState r -> a -> IO r) -> IO r
    loop p s k = case p of
      PRet x -> do
        k s x
      PBind p g -> do
        loop p s $ \s a -> loop (g a) s k
      PIO io -> do
        x <- io
        k s x

      NonPar pa pb -> do
        loop pa s $ \s a -> loop pb s $ \s b -> k s (a,b)

      Par a b -> do
        keyA <- HMap.createKey
        keyB <- HMap.createKey
        let
          kA s a = do
            let PState{hmap} = s
            case HMap.lookup keyB hmap of
              Just b -> k s (a,b)
              Nothing -> next s { hmap = HMap.insert keyA a hmap }
        let
          kB s b = do
            let PState{hmap} = s
            case HMap.lookup keyA hmap of
              Just a -> k s (a,b)
              Nothing -> next s { hmap = HMap.insert keyB b hmap }

        loop a s { active = PJob b kB : active s } kA

      Yield -> do
        let PState{blocked} = s
        let me = PJob (pure ()) k
        next s { blocked = me : blocked }

    next :: PState r -> IO r
    next s@PState{active,blocked} =
      case active of
        j:active -> resume j s { active }
        [] -> case blocked of
          [] -> error "next: no more jobs"
          blocked ->
            next s { active = reverse blocked, blocked = [] }

    resume :: PJob r -> PState r -> IO r
    resume (PJob p k) s = loop p s k

data PState r = PState
  { active :: [PJob r]
  , blocked :: [PJob r]
  , hmap :: HMap
  }

data PJob r where
  PJob :: P a -> (PState r -> a -> IO r) -> PJob r
