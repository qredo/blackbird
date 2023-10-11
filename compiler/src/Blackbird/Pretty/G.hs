{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Blackbird.Pretty.G
  ( prettyG
  , defaultCxt
  ) where

import Control.Lens hiding (index)
import Control.Monad.State
import Data.Foldable as Foldable
import Data.Functor.Rep
import Data.Maybe
import Data.Map as Map hiding (splitAt)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word
import Blackbird.Pretty
import Blackbird.Pretty.Core
import Blackbird.Syntax.Sort
import Blackbird.Syntax.G

defaultCxt :: Sort -> Ref -> Doc s
defaultCxt U (Lit n) = pretty n
defaultCxt s (Lit n) = "bad lit?" <+> pretty (show s) <> "{" <> pretty n <> "}"
defaultCxt N (Native _) = "native"
defaultCxt s (Native _) = "bad native?" <+> pretty (show s)
defaultCxt s (Global n) = "global?" <+> pretty (show s) <> "{" <> prettyId n <> "}"
defaultCxt s (Stack n) = "stack?" <+> pretty (show s) <> "{" <> pretty n <> "}"
defaultCxt s (Local n) = "local?" <+> pretty (show s) <> "{" <> pretty n <> "}"

prettyG :: [Doc s] -> (Sort -> Ref -> Doc s) -> G -> Doc s
prettyG vs pr (Case e bs) =
      "case"
  <+> prettyG vs pr e
  <+> "of"
  <+> prettyContinuation vs pr bs
prettyG vs pr (CaseLit r bs) =
      "case#"
  <+> pr U r
  <+> "of"
  <+> prettyContinuation vs pr bs
prettyG _  pr (App _n f xs) = hsep $ prettyFunc pr f : prettySorted (imap (fmap Foldable.toList . fmap . pr) xs)
prettyG vs pr (Let bs e)    = prettyLet vs pr False bs e
prettyG vs pr (LetRec bs e) = prettyLet vs pr True bs e
prettyG _ _   Slot          = "slot"

prettyFunc :: (Sort -> Ref -> Doc s) -> Func -> Doc s
prettyFunc pr (Ref  r) = pr B r
prettyFunc _  (Con  t) = "<" <> pretty (show t) <> ">"

rummage :: [a] -> Word64 -> Either a Word64
rummage []     n = Right n
rummage (x:_)  0 = Left x
rummage (_:xs) n = rummage xs (n-1)

push :: Sorted [Doc s] -> (Sort -> Ref -> Doc s) -> Sort -> Ref -> Doc s
push xs f s = either id (f s) . _Stack (rummage (xs^.sort s))

prettyContinuation :: [Doc s] -> (Sort -> Ref -> Doc s) -> Continuation -> Doc s
prettyContinuation vs pr (Continuation bs d) = block $ (f <$> Map.toList bs) ++ ldd
  where
    ldd = maybeToList $ (\c -> "_ ->" <+> prettyG vs pr c) <$> d
    f (t, (n, c)) = "<" <> pretty (show t) <> ">"
                <+> hsep (prettySorted ws ++ ["->" <+> prettyG rest (push ws pr) c])
      where (ws, rest) = splitSorted n vs

semiBraces :: [Doc s] -> Doc s
semiBraces = encloseSep lbrace rbrace semi

prettySorted :: Sorted [Doc s] -> [Doc s]
prettySorted cvs = ifoldMap go cvs where
  go cc ws = pretty (show cc) <> semiBraces ws <$ guard (not (Prelude.null ws))

splitSorted :: Sorted Word64 -> [a] -> (Sorted [a], [a])
splitSorted = runState . traverse (state . splitAt . fromIntegral)

prettyLet :: [Doc s] -> (Sort -> Ref -> Doc s) -> Bool -> Vector PreClosure -> G -> Doc s
prettyLet vs pr rec bs e =
      "let"
  <+> block (zipWith (\w bd -> w <+> "=" <+> bd) ws (Foldable.toList bds))
  <+> "in"
  <+> prettyG rest pr' e
 where
 bl = V.length bs
 (ws, rest) = splitAt bl vs
 pr' = push (Sorted ws [] []) pr
 bds = prettyPreClosure rest (if rec then pr' else pr) <$> bs

prettyPreClosure :: forall s. [Doc s] -> (Sort -> Ref -> Doc s) -> PreClosure -> Doc s
prettyPreClosure vs pr (PreClosure cfvs (LambdaForm fa ba up bo))
  = hsep
  $ dfvs : dl : prettySorted cbvs ++ ["->" <+> prettyG rest pr' bo]
  where
    dl :: Doc s
    dl = "\\" <> if up then "u" else "n"
    dfvs = semiBraces $ join $ Foldable.toList $ imap (fmap Foldable.toList . fmap . pr) cfvs
    (cbvs, rest) = splitSorted ba vs
    pr' c (Local n)
      | n < index fa c = pr c $ index cfvs c V.! fromIntegral n
      | otherwise = error "PANIC: prettyPreClosure: Bad variable reference"
      where
    pr' c r = push cbvs pr c r
