{-# LANGUAGE FlexibleInstances #-}

module Compiler.Common where

import qualified Data.Text.Format as F
import qualified Data.Text.Lazy as T

int32MAX = 2147483647
int16MAX = 32767
int8MAX = 127

format f s = T.unpack $ F.format f s

data Exception a = UnboundVariable a | LiteralOverflow a Integer

positionString line column =
    "Error at line " ++ show line ++ " column " ++ show column ++ ":\n"

instance Show (Exception (Maybe (Int, Int))) where
    show (UnboundVariable (Just (line, column))) =
        positionString line column ++ "UnboundVariable"
    show (LiteralOverflow (Just (line, column)) e) =
        positionString line column ++ "Literal Overflow " ++ show e
