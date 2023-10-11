{-# LANGUAGE TupleSections #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett and Dan Doel 2013-2014
-- License   :  BSD2
-- Maintainer:  Qredo LTD <support@qredo.com>
-- Stability :  experimental
-- Portability: non-portable
--------------------------------------------------------------------
module Blackbird.Builtin.Core
  (
  -- * Host Literals
    Lit(..)
  -- * Lists
  , cons
  , nil
  -- * Maybe
  , just
  , nothing
  -- * prim wrappers
  , stringh
  , integerh
  , inth
  , wordh
  , longh
  , nlisth
  , policyh
  -- * primops
  , cPutStrLn
  , cShowInt
  , cShowLong
  , cShowWord
  , cShowLongHash
  , cAddLong
  , cFromIntegerToInt
  , cFromIntegerToLong
  , cFromIntegerToWord
  , cFoldr
  , cNNil
  , cNCons
  , cAll
  , cAny
  , cAnd
  , cOr
  , cPerson
  ) where

import Bound
import Control.Lens ((#), review)
import Data.Int
import qualified Data.Map as M
import Data.Text hiding (zip, length, concatMap, cons)
import Blackbird.Builtin.Global
import Blackbird.Syntax.Convention
import Blackbird.Syntax.Core
import Blackbird.Syntax.Global hiding (N)
import Blackbird.Syntax.Id
import Blackbird.Syntax.Literal

--
-- $setup
-- >>> :m + Text.Groom Ermine.Builtin.Core Data.Int
--
-- >>> putStrLn $ groom (lit (1 :: Int32) `cons` nil :: Core Convention a)
-- Data [C, C] 1
--   (glob (Infix R 5) (mkModuleName "base" "Builtin") "::")
--   [Data [U] 0
--      (glob Idfix (mkModuleName "base" "Builtin") "Literal")
--      [HardCore (Lit (Int 1))],
--    Data [] 0 (glob Idfix (mkModuleName "base" "Builtin") "[]") []]

-- | The built-in '[]' constructor for a list.
nil :: Core cc a
nil = Data [] 0 nilg []

-- | The built-in '::' constructor for a list.
cons :: AsConvention cc => Core cc a -> Core cc a -> Core cc a
cons a as = Data (review _Convention <$> [C,C]) 1 consg [a,as]

-- | The built-in 'Just' constructor for 'Maybe'.
just :: AsConvention cc => Core cc a -> Core cc a
just a = Data [_Convention # C] 1 justg [a]

-- | The built-in 'Nothing' constructor for 'Maybe'.
nothing :: Core cc a
nothing = Data [] 0 nothingg []

stringh :: AsConvention cc => Core cc a -> Core cc a
stringh s = Data [_Convention # N] 0 stringhg [s]

integerh :: AsConvention cc => Core cc a -> Core cc a
integerh s = Data [_Convention # N] 0 integerhg [s]

inth :: AsConvention cc => Core cc a -> Core cc a
inth i = Data [_Convention # U] 0 inthg [i]

wordh :: AsConvention cc => Core cc a -> Core cc a
wordh i = Data [_Convention # U] 0 wordhg [i]

longh :: AsConvention cc => Core cc a -> Core cc a
longh l = Data [_Convention # U] 0 longhg [l]

nlisth :: AsConvention cc => Core cc a -> Core cc a
nlisth s = Data [_Convention # N] 0 nlisthg [s]

policyh :: AsConvention cc => Core cc a -> Core cc a
policyh s = Data [_Convention # N] 0 policyhg [s]

cPutStrLn :: AsConvention cc => Core cc a
cPutStrLn =
  Lam [_Convention # C] . Scope $
    Case (Var (B 0))
      (M.singleton 0 . Match [_Convention # N] stringg
        . Scope . App (_Convention # N) (_Id._Global # putStrLnhg) $ Var (B 1))
      Nothing

cShowNumber :: AsConvention cc => Global -> Global -> Core cc a
cShowNumber hg showHg =
  Lam [_Convention # C] . Scope $
    Case (Var (B 0))
      (M.singleton 0 . Match [_Convention # U] hg
        . Scope . App (_Convention # U) (_Id._Global # showHg) $ Var (B 1))
      Nothing

cShowInt, cShowLong, cShowWord :: AsConvention cc => Core cc a
cShowInt  = cShowNumber inthg  showInthg
cShowLong = cShowNumber longhg showLonghg
cShowWord = cShowNumber wordhg showWordhg

cShowLongHash :: Core cc a
cShowLongHash = _Id._Global # showLonghg

cAddLong :: AsConvention cc => Core cc a
cAddLong =
  Lam (review _Convention <$> [C,C]) . Scope $
    Case (Var (B 0))
      (M.singleton 0 . Match [_Convention # U] longhg . Scope
        $ Case (Var . F . Var . B $ 1)
            (M.singleton 0 . Match [_Convention # U] longhg . Scope
              $ App (_Convention # U)
                  (App (_Convention # U)
                    (_Id._Global # addLonghg) $ (Var . F . Var . B $ 1))
                  (Var $ B 1))
            Nothing)
      Nothing

cFromIntegerTo :: AsConvention cc => Global -> Core cc a
cFromIntegerTo fromIntegerTohg =
  Lam [_Convention # C] . Scope $
    Case (Var (B 0))
      (M.singleton 0 . Match [_Convention # N] integerhg . Scope
        . App (_Convention # N) (_Id._Global # fromIntegerTohg) $ Var (B 1))
      Nothing

cFromIntegerToInt, cFromIntegerToLong, cFromIntegerToWord :: AsConvention cc => Core cc a
cFromIntegerToInt  = cFromIntegerTo fromIntegerToInthg
cFromIntegerToLong = cFromIntegerTo fromIntegerToLonghg
cFromIntegerToWord = cFromIntegerTo fromIntegerToWordhg

-- erase when libraries are properly supported?
cFoldr :: AsConvention cc => Core cc a
cFoldr =
  Lam [_Convention # C, _Convention # C] . Scope $
    Let
      [Scope . Lam [_Convention # C] . Scope $
        Case
          (Var $ B 0)
          (M.fromList
            [(0,Match [] nilg . Scope . Var . F . Var . F . Var . F . Var $ B 1)
            ,(1,Match [_Convention # C, _Convention # C] consg . Scope $
              App (_Convention # C)
                (App (_Convention # C)
                  (Var . F . Var . F . Var . F . Var $ B 0)
                  (Var $ B 1))
                (App (_Convention # C)
                  (Var . F . Var . F . Var $ B 0)
                  (Var $ B 2)))
            ])
          Nothing]
      (Scope . Var $ B 0)

cNNil :: Core cc a
cNNil = _Id . _Global # nnilhg

cNCons :: AsConvention cc => Core cc a
cNCons =
  Lam (review _Convention <$> [C,C]) . Scope $
    Case (Var (B 0))
      (M.singleton 0 . Match [_Convention # N] policyhg . Scope
        $ Case (Var . F . Var . B $ 1)
            (M.singleton 0 . Match [_Convention # N] nlisthg . Scope
              $ App (_Convention # N)
                  (App (_Convention # N)
                    (_Id._Global # nconshg) $ (Var . F . Var . B $ 1))
                  (Var $ B 1))
            Nothing)
      Nothing

cFromListToNList :: AsConvention cc => Core cc a
cFromListToNList = App (_Convention # C)
  (App (_Convention # C)
    (_Id . _Global # foldrg)
    (_Id . _Global # nconsg))
  (_Id . _Global # nnilg)

cAll :: AsConvention cc => Core cc a
cAll =
  Lam [_Convention # C] . Scope $
    Case (App (_Convention # C) cFromListToNList (Var (B 0)))
      (M.singleton 0 . Match [_Convention # N] nlisthg . Scope
        . App (_Convention # N) (_Id._Global # allhg) $ Var (B 1))
      Nothing

cAny :: AsConvention cc => Core cc a
cAny =
  Lam (review _Convention <$> [C,C]) . Scope $
    Case (Var (B 0))
      (M.singleton 0 . Match [_Convention # U] longhg . Scope
        $ Case (App (_Convention # C) cFromListToNList (Var . F . Var . B $ 1))
            (M.singleton 0 . Match [_Convention # N] nlisthg . Scope
              $ App (_Convention # N)
                  (App (_Convention # U)
                    (_Id._Global # anyhg) $ (Var . F . Var . B $ 1))
                  (Var $ B 1))
            Nothing)
      Nothing

cAnd :: AsConvention cc => Core cc a
cAnd =
  Lam (review _Convention <$> [C,C]) . Scope $
    Case (Var (B 0))
      (M.singleton 0 . Match [_Convention # N] policyhg . Scope
        $ Case (Var . F . Var . B $ 1)
            (M.singleton 0 . Match [_Convention # N] policyhg . Scope
              $ App (_Convention # N)
                  (App (_Convention # N)
                    (_Id._Global # andhg) $ (Var . F . Var . B $ 1))
                  (Var $ B 1))
            Nothing)
      Nothing

cOr :: AsConvention cc => Core cc a
cOr =
  Lam (review _Convention <$> [C,C]) . Scope $
    Case (Var (B 0))
      (M.singleton 0 . Match [_Convention # N] policyhg . Scope
        $ Case (Var . F . Var . B $ 1)
            (M.singleton 0 . Match [_Convention # N] policyhg . Scope
              $ App (_Convention # N)
                  (App (_Convention # N)
                    (_Id._Global # orhg) $ (Var . F . Var . B $ 1))
                  (Var $ B 1))
            Nothing)
      Nothing

cPerson :: AsConvention cc => Core cc a
cPerson =
  Lam [_Convention # C] . Scope $
    Case (Var (B 0))
      (M.singleton 0 . Match [_Convention # N] stringhg . Scope
        . App (_Convention # N) (_Id._Global # personhg) $ Var (B 1))
      Nothing

-- | Lifting of literal values to core.
class Lit a where
  lit  :: AsConvention cc => a   -> Core cc b
  lits :: AsConvention cc => [a] -> Core cc b
  lits = Prelude.foldr (cons . lit) nil

instance Lit Int64 where
  lit l = Data [_Convention # U] 0 literalg [HardCore $ Lit $ Long l]

instance Lit Int32 where
  lit i = Data [_Convention # U] 0 literalg [HardCore $ Lit $ Int i]

instance Lit Char where
  lit c  = Data [_Convention # U] 0 literalg [HardCore $ Lit $ Char c]
  lits = lit . pack

instance Lit Text where
  lit s = Data [_Convention # N] 0 stringg [HardCore $ Lit $ String s]

instance Lit Int8 where
  lit b = Data [_Convention # U] 0 literalg [HardCore $ Lit $ Byte b]

instance Lit Int16 where
  lit s = Data [_Convention # U] 0 literalg [HardCore $ Lit $ Short s]

instance (Lit a, Lit b) => Lit (a, b) where
  lit (a,b) = Data (review _Convention <$> [C,C]) 0 (tupleg 2) [lit a, lit b]

instance Lit a => Lit [a] where
  lit = lits

instance Lit a => Lit (Maybe a) where
  lit = maybe nothing (just . lit)
