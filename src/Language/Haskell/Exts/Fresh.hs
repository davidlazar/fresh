-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Haskell.Exts.Fresh
-- Copyright   :  (c) David Lazar, 2011
-- License     :  MIT
--
-- Maintainer  :  lazar6@illinois.edu
-- Stability   :  experimental
-- Portability :  unknown
--
-- In the context of this library, a fresh variable has the form:
--
-- > Ident "@foo"
--
-- where 'Ident' is a constructor of the 'Name' type from
-- Language.Haskell.Exts, and \"foo\" is the variable's name. A concrete
-- variable is any other variable that appears in the source code.
--
-- To concretize a fresh variable means to remove the '@' character that
-- appears before its name and to rename the variable (keeping its existing
-- name as a prefix) so that it is globally unique across an AST.
-----------------------------------------------------------------------------

module Language.Haskell.Exts.Fresh
    ( concretize
    , concretize'
    , ConflictTable
    ) where

import Control.Arrow ((***), first)
import Control.Monad (join, replicateM)
import Data.Generics
import Data.List (mapAccumR, partition)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Language.Haskell.Exts (Name(..))

-- | Maps fresh variables with conflicting names to unique names that are
-- non-conflicting.
type ConflictTable = Map String String

-- | Concretize all of the fresh variables appearing in the given 'Data'
-- value (most likely a Haskell AST).
concretize :: (Data a) => a -> a
concretize = fst . concretize'

-- | Same as 'concretize', but returns the 'ConflictTable' used for
-- concretization.
concretize' :: (Data a) => a -> (a, ConflictTable)
concretize' a = (everywhere (mkT (concretizeVar cft)) a, cft)
    where cft = genConflictTable a

-- | Turn a fresh variable into a concrete variable by looking up the
-- variable's name in the given conflict table; if not present in the
-- conflict table, the leading '@' is simply dropped. Concrete variables
-- are returned unmodified. 
concretizeVar :: ConflictTable -> Name -> Name
concretizeVar cft (Ident ('@':var)) = Ident (Map.findWithDefault var var cft)
concretizeVar _ name = name

-- | @genConflictTable data@ constructs a 'ConflictTable' using the sets of
-- fresh variables and concrete variables that appear in @data@ (most likely
-- a Haskell AST).
genConflictTable :: (Data a) => a -> ConflictTable
genConflictTable
    = uncurry mkConflictTable
    . join (***) Set.fromList
    . first (map stripFresh)
    . partition isFresh
    . map unIdent
    . listify isIdent

-- | Construct a 'ConflictTable' with the given Sets of fresh and concrete
-- variable names.
mkConflictTable :: Set String -> Set String -> ConflictTable
mkConflictTable fresh concrete
    = Map.fromList . snd $ mapAccumR uniquifyAcc allvars conflicting
    where allvars     = Set.union fresh concrete
          conflicting = Set.toList (Set.intersection fresh concrete)

-- | Accumulator function to uniquify a list of Strings (updates the Set of
-- Strings as Strings are uniquified).
uniquifyAcc :: Set String -> String -> (Set String, (String, String))
uniquifyAcc set prefix = (Set.insert uniq set, (prefix, uniq))
    where uniq = uniquify set prefix

-- | @uniquify set prefix@ returns a String with the given prefix that is not
-- contained in @set@.
uniquify :: Set String -> String -> String
uniquify set prefix = first unique choices
    where first  p = head . filter p
          unique x = Set.notMember x set
          choices  = map (prefix ++) ("" : letters)

-- | Infinite stream of letters to be used as suffixes:
-- ["a".."z","aa","ab"..]
letters :: [String]
letters = concatMap (flip replicateM ['a'..'z']) [1..]


{- Tiny utility functions -}

stripFresh :: String -> String
stripFresh ('@':s) = s
stripFresh s = s

isFresh :: String -> Bool
isFresh ('@':_) = True
isFresh _ = False

unIdent :: Name -> String
unIdent (Ident s) = s
unIdent _ = error "unIdent: not an Ident!"

isIdent :: Name -> Bool
isIdent (Ident _) = True
isIdent _ = False
