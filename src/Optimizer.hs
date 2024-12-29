module Optimizer where

class ComputationNode a where
    simplify :: a -> a
    hash :: a -> String
    structEq :: a -> a -> Bool
    hashEq :: a -> a -> Bool
    isSimplified :: a -> Bool
