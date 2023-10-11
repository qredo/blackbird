{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ImportQualifiedPost #-}
module Blackbird.Interpreter.Policy
  ( interpret
  , residuate
  , prove
  , satisfiable
  , satisfies
  , signatures
  , stallers
  , verify
  , verifyWitness
  ) where

import Control.Arrow (left, Arrow ((&&&)))
import Control.Exception qualified as E
import Control.Lens (preview, _Left, (#), (&), (.~), (<&>), (^.))
import Control.Monad.State.Strict
import Data.Bool (bool)
import Data.Default
import Data.Either (partitionEithers)
import Data.Foldable (fold)
import Data.HashMap.Strict qualified as HM
import Data.Map (Map, fromSet)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Vector qualified as B
import Data.Void
import Data.Word (Word64)
import Text.Parser.Combinators (eof)
import Text.Parser.Token (whiteSpace)
import Text.Trifecta.Result
import Unsafe.Coerce (unsafeCoerce)

import Blackbird.Builtin
import Blackbird.Builtin.Type as Type (policy)
import Blackbird.Console.Command (checkAndCompile)
import Blackbird.Constraint.Env
import Blackbird.Core.Compiler
import Blackbird.Core.Optimizer
import Blackbird.Diagnostic
import Blackbird.Interpreter as Interp hiding (Address)
import Blackbird.Native.Policy (Address (..), Policy (..))
import Blackbird.Native.Witness (Witness (..))
import Blackbird.Parser.Policy as Parser (policy)
import Blackbird.Parser.Trifecta
import Blackbird.Parser.Type
import Blackbird.Pretty
import Blackbird.Pretty.Type (prettyType)
import Blackbird.Syntax.G as G (Func (Con), G (App), LambdaForm (..), Ref (Local))
import Blackbird.Syntax.Global as Global
import Blackbird.Syntax.Id
import Blackbird.Syntax.ModuleName
import Blackbird.Syntax.Sort (Sorted (Sorted))
import Blackbird.Syntax.Term as Term
import Blackbird.Unification.Meta

signatures :: Policy -> Set Address
signatures = \case
  Policy_Signature s -> Set.singleton s
  Policy_All ps -> foldMap signatures ps
  Policy_Any _ ps -> foldMap signatures ps

satisfies :: (Address -> Bool) -> Policy -> Bool
satisfies predicate = fix $ \go -> \case
  Policy_Signature s -> predicate s
  Policy_All ps -> all go ps
  Policy_Any k ps -> run k ps
    where
      run 0 _ = True
      run _ [] = False
      run n (p:ps') = run (bool id pred (go p) n) ps'

prove :: Set Address -> Policy -> Maybe (Witness, Set Address)
prove signers = preview _Left . residuate (`Set.member` signers)

verifyWitness :: Set Address -> Witness -> Policy -> Bool
verifyWitness signers = fix $ \go -> curry $ \case
  (Witness_PrecheckedSignature, Policy_Signature s) -> s `Set.member` signers
  (Witness_All ws, Policy_All ps) -> and $ zipWith go ws ps
  (Witness_Any ws, Policy_Any k ps) -> and
    [ fromIntegral (length ws) == k
    , and $ Map.intersectionWith go ws $ Map.fromAscList $ zip [0..] ps
    ]
  _ -> False

residuate :: (Address -> Bool) -> Policy -> Either (Witness, Set Address) Policy
residuate predicate = fix $ \go -> \case
  po@(Policy_Signature s)
    | predicate s -> Left (Witness_PrecheckedSignature, Set.singleton s)
    | otherwise -> Right po
  Policy_All ps -> case partitionEithers (fmap go ps) of
    (ws, []) -> Left $ gather Witness_All ws
    (_, ps') -> Right $ Policy_All ps'
  Policy_Any k ps -> run k id id $ zip [0..] ps
    where
      run 0 accW _    _  = Left $ gather Witness_Any $ Map.fromAscList $ accW []
      run n _    accP [] = Right $ Policy_Any n $ accP []
      run n accW accP ((i,p):ips) = case go p of
        Left w   -> run (pred n) (accW . ((i,w) :)) accP           ips
        Right p' -> run n        accW               (accP . (p':)) ips
  where
    gather f = (f . fmap fst) &&& foldMap snd

stallers :: Policy -> Maybe (Set Address)
stallers = \case
  Policy_Signature s -> Just $ Set.singleton s
  Policy_All ps -> fold <$> traverse stallers ps
  Policy_Any k ps' ->
    if k > fromIntegral (length ps)
      then Nothing
      else Just $ Map.keysSet $ Map.filter (>= threshold) substallCounts
    where
      ps = mapMaybe stallers ps'
      substallCounts :: Map Address Word64
      substallCounts = Map.unionsWith (+) $ fromSet (const 1) <$> ps
      threshold = fromIntegral (length ps) - k + 1

--- >>> verify mempty $ Policy_All []
-- True
--- >>> verify mempty $ Policy_Any 1 []
-- False
--- >>> :set -XOverloadedStrings
--- >>> verify (Set.fromList ["A"]) $ Policy_Any 2 [Policy_Signature "A", Policy_Signature "B"]
-- False
--- >>> verify (Set.fromList ["A", "B"]) $ Policy_Any 1 [Policy_Signature "A"]
-- True
--- >>> verify (Set.fromList ["A", "B"]) $ Policy_Any 2 [Policy_Signature "A"]
-- False
--- >>> verify (Set.fromList ["A", "B"]) $ Policy_Any 2 [Policy_Signature "A", Policy_Signature "B"]
-- True
verify :: Set Address -> Policy -> Bool
verify = satisfies . flip Set.member

-- >>> :set -XOverloadedStrings
-- >>> satisfiable $ Policy_Any 1 [Policy_Signature "A"]
-- True
--- >>> satisfiable $ Policy_Any 1 []
-- False
--- >>> satisfiable $ Policy_Any 1 [Policy_Signature "A", Policy_Signature "B"]
-- True

-- Assumes all sub policies are simultaneously satisfiable
-- Since there's no negation operation we can do this by treating all signatures as given
satisfiable :: Policy -> Bool
satisfiable = satisfies $ const True

interpret :: String -> IO (Either (Doc AnsiStyle) Policy)
interpret s = fmap join $ evalStateT (parsingS Parser.policy evalBody s) parserState

parsingS :: StateT s Parser a -> (a -> IO b)
        -> String -> StateT s IO (Either (Doc AnsiStyle) b)
parsingS p k s = StateT $ \st ->
  let p' = runStateT p st
  in case parseString (whiteSpace *> p' <* eof) mempty s of
      Success (a, st') -> k a <&> (,st') . Right
      Failure doc      -> pure (Left $ _errDoc doc) <&> (,st)

evalBody :: Term Ann Text -> IO (Either (Doc AnsiStyle) Policy)
evalBody syn =
  rescue (ioM mempty (runCM (checkAndCompile syn) dummyConstraintEnv)) >>= \case
    Left doc -> return $ Left doc
    Right (t, c) | t == Type.policy -> liftIO $ do
      prims <- primOps
      let cafg = _Global # glob Idfix (mkModuleName "policy" "Policy") " policy" :: Id
      caf <- allocGlobal absurd $
        optimize c
      ms <- defaultMachineState 512 $ HM.insert cafg caf prims
      _ms' <- eval (compile 0 absurd $ _Id # cafg) def (ms & trace .~ pure . const ())
      res <- peek caf
      case res of
        Closure (LambdaForm (Sorted 0 0 1) 0 False (G.App 0 (G.Con 0) (Sorted (B.toList -> []) (B.toList -> []) (B.toList -> [Local 0])))) env -> do
          val <- unsafeCoerce <$> B.headM (_envN env)
          return $ Right val
        _ -> do
          return $ Left "[E003] Evaluation stalled"
    Right (t, _) -> liftIO $ do
      return . Left $ "[E002] Input must be a Policy, but is actually:" <+> prettyType t names (-1)
  where
    rescue :: IO (Either (Doc AnsiStyle) a) -> IO (Either (Doc AnsiStyle) a)
    rescue = fmap (join . left (\d@(Diagnostic{}) -> explain (d ^. rendering) (d ^. err))) . E.try
