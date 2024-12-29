module ComputationGraph where

class ComputationNode a where
    simplifyToValue :: a -> Float
    hash :: a -> String
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

hashNode :: Node -> String
hashNode (Point p) = "point"
hashNode (Add a b) = "add" ++ hashNode a ++ hashNode b
hashNode (Negate a) = "negate" ++ hashNode a

hashEqNode :: Node -> Node -> Bool
hashEqNode a b = hashNode a == hashNode b

isNodePoint :: Node -> Bool
isNodePoint (Point p) = True
isNodePoint _ = False

structEqNode :: Node -> Node -> Bool
structEqNode (Point r) (Point s) = True
structEqNode (Add a b) (Add c d) = structEqNode a c && structEqNode b d

instance ComputationNode Node where
    simplifyToValue = simplifyNodeToPointValue
    hash = hashNode
    structEq = structEqNode
    hashEq = hashEqNode
    isPoint = isNodePoint

possiblySameThread :: (ComputationNode a) => a -> a -> Bool
possiblySameThread r s = hashEq r s && structEq r s
