module ComputationGraph where

class ComputationNode a where
    simplifyToValue :: (Num b) => a -> b
    hash :: a -> Int
    structEq :: a -> a -> Bool
    hashEq :: a -> a -> Bool
    isPoint :: a -> Bool

data OperationType = Add | Negate deriving (Enum, Show)

data Node
    = Operation OperationType [Node]
    | Point Float

simplifyNodeToPoint :: Node -> Float
simplifyNodeToPoint (Point p) = p
simplifyNodeToPoint (Operation Add [Point x, Point y]) = x + y
simplifyNodeToPoint (Operation Negate [Point x]) = -x
simplifyNodeToPoint (Operation t parameters) = simplifyNodeToPoint (Operation t simplifiedParameterNodes)
  where
    simplifiedParameterNodes = map simplifyNodeToPointNode parameters
    simplifyNodeToPointNode x = Point (simplifyNodeToPoint x)
