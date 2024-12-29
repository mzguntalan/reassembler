module Optimizer where

class ComputationNode a where
    simplifyToValue :: a -> Float
    hash :: a -> String
    structEq :: a -> a -> Bool
    hashEq :: a -> a -> Bool
    isPoint :: a -> Bool
