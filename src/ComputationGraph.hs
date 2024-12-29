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

simplifyNodeToPoint :: Node -> Float
simplifyNodeToPoint (Point p) = p
simplifyNodeToPoint (Add (Point x) (Point y)) = x + y
simplifyNodeToPoint (Add r s) = simplifyNodeToPoint (Add (Point (simplifyNodeToPoint r)) (Point (simplifyNodeToPoint s)))
simplifyNodeToPoint (Negate (Point x)) = -x
simplifyNodeToPoint (Negate r) = simplifyNodeToPoint (Negate (Point (simplifyNodeToPoint r)))
