module ComputationGraph where

import Optimizer qualified

data Node
  = Add Node Node
  | Negate Node
  | Point Float

simplifyNodeToPoint :: Node -> Node
simplifyNodeToPoint (Point p) = Point p
simplifyNodeToPoint (Add (Point x) (Point y)) = Point (x + y)
simplifyNodeToPoint (Add r s) = simplifyNodeToPoint (Add (simplifyNodeToPoint r) (simplifyNodeToPoint s))
simplifyNodeToPoint (Negate (Point x)) = Point (-x)
simplifyNodeToPoint (Negate r) = simplifyNodeToPoint (Negate (simplifyNodeToPoint r))

hash :: Node -> String
hash (Point p) = "point"
hash (Add a b) = "add" ++ hash a ++ hash b
hash (Negate a) = "negate" ++ hash a

hashEq :: Node -> Node -> Bool
hashEq a b = hash a == hash b

isPoint :: Node -> Bool
isPoint (Point p) = True
isPoint _ = False

structEq :: Node -> Node -> Bool
structEq (Point r) (Point s) = True
structEq (Add a b) (Add c d) = structEq a c && structEq b d

instance Optimizer.ComputationNode Node where
  simplify = simplifyNodeToPoint
  hash = hash
  structEq = structEq
  hashEq = hashEq
  isSimplified = isPoint
