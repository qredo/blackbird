{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}

--------------------------------------------------------------------
-- |
-- Copyright :  (c) Dan Doel 2013
-- License   :  BSD2
-- Maintainer:  Qredo LTD <support@qredo.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- Tools for pretty-printing 'Core' expressions.
--------------------------------------------------------------------

module Blackbird.Pretty.Core
 ( prettyCore
 , prettyHardCore
 , prettyId
 ) where

import Bound
import Control.Lens
import Data.Bifunctor
import Data.Text.Lens hiding (text)
import Data.Traversable
import Data.Word
import Blackbird.Pretty
import Blackbird.Pretty.Global
import Blackbird.Pretty.Literal
import Blackbird.Pretty.Type
import Blackbird.Syntax.Convention
import Blackbird.Syntax.Core
import Blackbird.Syntax.Global
import Blackbird.Syntax.Head
import Blackbird.Syntax.Id
import Blackbird.Syntax.Name

-- | Pretty print a 'HardCore' expression.
prettyHardCore :: Int -> HardCore -> Doc s
prettyHardCore _ (Super   i)     = pretty $ "super{" ++ show i ++ "}"
prettyHardCore _ (Slot    i)     = pretty $ "slot{"  ++ show i ++ "}"
prettyHardCore _ (Lit     l)     = prettyLiteral l
prettyHardCore n (Error   s)     = parensIf (n>10) . pretty $ "error " ++ show s
prettyHardCore _ (Id      g)     = prettyId g

prettyId :: Id -> Doc s
prettyId (GlobalId g)   = prettyGlobal g
prettyId (InstanceId h) = prettyHead h

prettyHead :: Head -> Doc s
prettyHead i = pretty ("instance{" ++ (i^.headClass.name.unpacked))
     <+> hsep ((prettyType ?? repeat "_" ?? 1000) . bimap (const "_") (const "_")
           <$> i^.headTypeArgs)
      <> pretty "}"

-- | Pretty print a 'HardCore' expression.
prettyCore :: forall f a s. Applicative f
           => [String] -> Int -> (a -> Int -> f (Doc s)) -> Core Convention a -> f (Doc s)
prettyCore _  prec k (Var v) = k v prec
prettyCore _  prec _ (HardCore h) = pure $ prettyHardCore prec h
prettyCore vs prec k (Data cc tg g fs) =
  parensIf (prec > 10) . coreData cc tg g <$> traverse (prettyCore vs 11 k) fs
prettyCore vs _    k (Prim cc r g fs) =
  corePrim cc r g <$> traverse (prettyCore vs 11 k) fs
prettyCore vs prec k (App _ f x) =
  (\df dx -> parensIf (prec>10) $ df <+> dx)
    <$> prettyCore vs 10 k f <*> prettyCore vs 11 k x
prettyCore vs prec k (Lam cc (Scope e)) =
  coreLam cc prec ws <$> prettyCore rest (-1) k' e
 where
 (ws, rest) = first (fmap pretty) $ splitAt (length cc) vs
 k' (B i) _ = pure $ ws !! fromIntegral i
 k' (F c) p = prettyCore rest p k c
prettyCore (v:vs) prec k (Case e m d) =
  coreCase dv prec
    <$> prettyCore vs (-1) k e
    <*> branches
    <*> traverse (prettyCore vs (-1) l . unscope) d
 where
 l (B _) _ = pure dv
 l (F c) p = prettyCore vs p k c
 dv = pretty v
 branches = for (itoList m) $ \(tg, Match cc g (Scope b)) ->
   let (ws,rest) = first (fmap pretty) $ splitAt (length cc) vs
       k' (B 0) _ = pure dv
       k' (B i) _ = pure $ ws !! fromIntegral (i-1)
       k' (F c) p = prettyCore rest p k c
    in (\bd -> nest 2 $ coreData cc tg g ws <+> pretty "->" <+> bd)
          <$> prettyCore rest (-1) k' b

prettyCore vs prec k (Let bs e) = h <$> traverse pc bs <*> pc e
 where
 pc :: Scope Word64 (Core Convention) a -> f (Doc s)
 pc = prettyCore rest (-1) k' . unscope
 n = length bs
 (ws,rest) = first (fmap pretty) $ splitAt n vs
 k' (B i) _ = pure $ ws !! fromIntegral i
 k' (F c) p = prettyCore rest p k c
 eq l r = l <+> pretty "=" <+> r
 h bds ed = parensIf (prec>=0) . nest 2 $
              pretty "let" <+> block (zipWith eq ws bds) <+> pretty "in" <+> ed
prettyCore vs _ k (Dict sups sls) =
  (\xs ys -> pretty "dict" <> block [pretty "supers" `eq` block xs, pretty "slots" `eq` block (zipWith eq slotNames ys)])
    <$> traverse (prettyCore vs 0 k) sups
    <*> traverse (prettyCore rest 0 k' . unscope) sls
 where
  (slotNames,rest) = first (fmap pretty) $ splitAt (length sls) vs
  eq l r = l <+> pretty "=" <+> r
  k' (B i) _ = pure $ slotNames !! fromIntegral i
  k' (F c) p = prettyCore rest p k c
prettyCore [] _ _ _ = error "panic: prettyCore ran out of variable names"

-- \{a,b} [C,C]->C body
coreLam :: [Convention] -> Int -> [Doc s] -> Doc s -> Doc s
coreLam cc prec ws e = parensIf (prec>=0) $
  pretty "\\" <> encloseSep lbrace rbrace comma ws <+> encloseSep lbracket rbracket comma (pretty.show<$>cc) <> pretty "->" <+> e

-- Nothing<0,[]>
-- Just<1,[C]> a
coreData :: [Convention] -> Word64 -> Global -> [Doc s] -> Doc s
coreData cc tg g fds = pretty (g^.name.unpacked) <> pretty "<" <> pretty tg <> pretty "," <> pretty (show cc) <> pretty ">" <+> hsep fds

corePrim :: [Convention] -> Convention -> Global -> [Doc s] -> Doc s
corePrim cc tg g fds = pretty (g^.name.unpacked) <> pretty "<" <> pretty (show tg) <> pretty "," <> pretty (show cc) <> pretty ">" <+> hsep fds

coreCase :: Doc s -> Int -> Doc s -> [Doc s] -> Maybe (Doc s) -> Doc s
coreCase dv prec de dbs mdd = parensIf (prec>=0) $
  nest 2 $ pretty "case" <+> de <+> pretty "of" <+> vsep [dv, coreBlock dbs']
 where dbs' | Just dd <- mdd = dbs ++ [pretty "_ ->" <+> dd]
            | otherwise      = dbs

coreBlock :: [Doc s] -> Doc s
coreBlock [    ] = pretty "{}"
coreBlock (d:ds) = vsep (lbrace <+> d : map (semi <+>) ds) <> line <> rbrace
