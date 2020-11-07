{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Compiler.Common where

import qualified Data.Text.Format              as F
import qualified Data.Text.Lazy                as T
import           AbsInstant

int32MAX = 2147483647
int16MAX = 32767
int8MAX = 127

format f s = T.unpack $ F.format f s

data Exception a = UnboundVariable a Ident | LiteralOverflow a Integer

positionString line column =
    format "Error at line {} column {}:\n" (show line, show column)

instance Show (Exception (Maybe (Int, Int))) where
    show (UnboundVariable (Just (line, column)) (Ident name)) =
        positionString line column ++ "UnboundVariable " ++ name
    show (LiteralOverflow (Just (line, column)) e) =
        positionString line column ++ "Literal Overflow " ++ show e

type InstrBuilder = [String] -> [String]

type Instructions = [String]
