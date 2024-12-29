module ComputationGraph where

import Optimizer qualified

data OperationNode = Add PointNode PointNode | Negate PointNode

data PointNode -- reduces to a point when simplified
  = Operation OperationNode
  | Point Float

simplifyNodeToPoint :: PointNode -> PointNode
simplifyNodeToPoint (Point p) = Point p
simplifyNodeToPoint (Operation (Add (Point x) (Point y))) = Point (x + y)
simplifyNodeToPoint (Operation (Add r s)) =
  simplifyNodeToPoint (Operation (Add (simplifyNodeToPoint r) (simplifyNodeToPoint s)))
simplifyNodeToPoint (Operation (Negate (Point x))) = Point (-x)
simplifyNodeToPoint (Operation (Negate r)) = simplifyNodeToPoint (Operation (Negate (simplifyNodeToPoint r)))

hash :: PointNode -> String
hash (Point p) = "point"
hash (Operation (Add a b)) = "add" ++ hash a ++ hash b
hash (Operation (Negate a)) = "negate" ++ hash a

hashEq :: PointNode -> PointNode -> Bool
hashEq a b = hash a == hash b

isPoint :: PointNode -> Bool
isPoint (Point p) = True
isPoint _ = False

structEq :: PointNode -> PointNode -> Bool
structEq (Point r) (Point s) = True
structEq (Operation (Add a b)) (Operation (Add c d)) = structEq a c && structEq b d

instance Optimizer.ComputationNode PointNode where
  simplify = simplifyNodeToPoint
  hash = hash
  structEq = structEq
  hashEq = hashEq
  isSimplified = isPoint
