module ComputationGraph where

class ComputationNode a where
    simplifyToValue :: (Num b) => a -> b
    hash :: a -> Int
    structEq :: a -> a -> Bool
    hashEq :: a -> a -> Bool
    isPoint :: a -> Bool

data Node
    = Add Node Node
    | Negate Node
    | Point Float

simplifyNodeToPointValue :: Node -> Float
simplifyNodeToPointValue (Point p) = p
simplifyNodeToPointValue (Add (Point x) (Point y)) = x + y
simplifyNodeToPointValue (Add r s) = simplifyNodeToPointValue (Add (Point (simplifyNodeToPointValue r)) (Point (simplifyNodeToPointValue s)))
simplifyNodeToPointValue (Negate (Point x)) = -x
simplifyNodeToPointValue (Negate r) = simplifyNodeToPointValue (Negate (Point (simplifyNodeToPointValue r)))
