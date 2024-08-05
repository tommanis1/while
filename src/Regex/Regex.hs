{-# OPTIONS_GHC -W #-}
{-# LANGUAGE LambdaCase, BlockArguments, RecursiveDo #-}

module Regex.Regex where

import Data.List (find)
import Data.Maybe (isJust)

data Regex e
    = Empty
    | Lit e
    | And (Regex e) (Regex e)
    | Or (Regex e) (Regex e)
    | Mark (Regex e)
    | Plus (Regex e)
    | Star (Regex e)
    -- | Sequence (Regex e) (Regex e)
    -- | Times Int Regex
    deriving Show

matchStream :: (Eq e) => Regex e -> [e] -> [[e]]
matchStream (Empty) syms        = [syms]
matchStream (Lit _) []          = []
matchStream (Lit c) (sym:syms)  = if c == sym then [syms] else []
matchStream (And re re') syms   = matchStream re syms >>= matchStream re'
-- matchStream (Sequence re re') syms   = matchStream re syms >>= matchStream re'
matchStream (Or re re')  syms   = matchStream re syms <> matchStream re' syms
matchStream (Mark re)    syms   = matchStream re syms <> [syms]
matchStream (Plus re)    syms   = matchStream re syms >>= matchStream (Star re)
matchStream star@(Star re) syms = (<> [syms]) do
    syms' <- matchStream re syms
    if syms' /= syms then matchStream star syms' else []

match :: (Eq e) => Regex e -> [e] -> Bool
match re syms = isJust $ find (== []) $ matchStream re syms