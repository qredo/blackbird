{-# OPTIONS_GHC -fno-warn-type-defaults #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett and Dan Doel 2012-2014
-- License   :  BSD2
-- Maintainer:  Qredo LTD <support@qredo.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- General-purpose utilities for pretty printing.
--------------------------------------------------------------------
module Blackbird.Pretty
  ( module Prettyprinter
  , names
  , parensIf
  , hyph
  , prePunctuate
  , prePunctuate'
  , block
  , hSay
  , hSayLn
  , say
  , sayLn
  , chooseNames
  , AnsiStyle
  , (</>)
  ) where

import Control.Monad.IO.Class
import Control.Lens
import Data.Bifunctor
import Data.Text (unpack)
import Blackbird.Syntax.Hint
import Numeric.Lens
import Prettyprinter
import Prettyprinter.Render.Terminal (AnsiStyle, renderIO)
import System.IO
import Text.Hyphenation
import Text.Trifecta.Delta () -- for Text.Trifecta.Instances

-- | Concatenates two documents with a 'softline' between.
infixr 5 </>
(</>) :: Doc s -> Doc s -> Doc s
a </> b = fillSep [a,b]

-- | This is an infinitely large free variable supply you can trim your used variables out of.
names :: [String]
names = map pure az
    ++ [ i : review (base 36) j | j <- [1..], i <- az ] where
  az = ['a'..'z']

-- | Pretty print parentheses
parensIf :: Bool -> Doc s -> Doc s
parensIf True  = parens
parensIf False = id

-- | Hyphenate a word using standard TeX-style 'english_US' hyphenation.
hyph :: String -> Doc s
hyph t = column $ \k -> pageWidth $ \pw ->
  let n = case pw of { AvailablePerLine l _ -> l ; Unbounded -> 80 }
      (pr,sf) = bimap (fmap fst) (fmap fst) $ span (\ (_,d) -> k + d < n) $ zip xs ls
      ls = tail $ scanl (\a b -> a + length b) 0 xs
      xs = hyphenate english_US t
  in if null pr
     then pretty (concat sf)
     else if null sf
          then pretty (concat pr)
          else vsep [pretty (concat pr) <> pretty '-', pretty (concat sf)]

prePunctuate :: Doc s -> [Doc s] -> [Doc s]
prePunctuate _ [    ] = []
prePunctuate p (d:ds) = d : map (p <+>) ds

prePunctuate' :: Doc s -> Doc s -> [Doc s] -> [Doc s]
prePunctuate' _  _ [    ] = []
prePunctuate' fp p (d:ds) = (fp <+> d) : map (p <+>) ds

-- | Format a layout block in explicit style.
block :: [Doc s] -> Doc s
block [    ] = pretty "{}"
block (d:ds) = sep (lbrace <+> d : map (semi <+>) ds) <> line <> rbrace

-- | Pretty print to a handle
hSay :: MonadIO m => Handle -> Doc AnsiStyle -> m ()
hSay h = liftIO . renderIO h . layoutPretty (defaultLayoutOptions { layoutPageWidth = AvailablePerLine 80 0.8 })

-- | Pretty print to a handle with a 'line'' after.
hSayLn :: MonadIO m => Handle -> Doc AnsiStyle -> m ()
hSayLn h d = hSay h (d <> line')

-- | Pretty print to 'stdout'
say :: MonadIO m => Doc AnsiStyle -> m ()
say = hSay stdout

-- | Pretty print to 'stdout' with a 'line'' after.
sayLn :: MonadIO m => Doc AnsiStyle -> m ()
sayLn d = say (d <> line')

chooseNames :: (String -> Bool) -> [Hint] -> [String] -> ([String], [String])
chooseNames p ahs = go p ahs . filter (\n -> n `notElem` avoid && not (p n))
 where
 avoid = [ unpack h | Just h <- ahs ]

 go _     [] supply = ([], supply)
 go taken (Nothing : hs) (n:supply) = (n:) `first` go taken hs supply
 go taken (Just h  : hs) supply@(n:ns)
   | taken h'  = (n:) `first` go taken hs ns
   | otherwise = (h':) `first` go (\x -> x == h' || taken x) hs supply
  where h' = unpack h
 go _ _ _ = error "PANIC: chooseNames: ran out of names"
