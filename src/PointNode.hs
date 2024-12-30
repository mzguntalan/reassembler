{-# LANGUAGE GADTs #-}

module PointNode where

class Node a where
    simplify :: a -> a
    hash :: a -> String
    structEq :: a -> a -> Bool
    hashEq :: a -> a -> Bool

class Operator a where
    isParametersValid :: a -> [PointNode] -> Bool
    perform :: a -> [PointNode] -> PointNode
    hashOperator :: a -> String

data BinaryOperator = Add | Subtract deriving (Enum, Show, Eq)

instance Operator BinaryOperator where
    isParametersValid Add [a, b] = True
    isParametersValid Add _ = False
    isParametersValid Subtract [a, b] = True
    isParametersValid Subtract _ = False
    perform Add [Point a, Point b] = Point (a + b)
    perform Add [a, b] = perform Add [simplify a, simplify b]
    perform Subtract [Point a, Point b] = Point (a - b)
    perform Subtract [a, b] = perform Subtract [simplify a, simplify b]
    hashOperator = show

data UnaryOperator = Negate deriving (Enum, Show, Eq)

instance Operator UnaryOperator where
    isParametersValid Negate [a] = True
    isParametersValid Negate _ = False
    perform Negate [Point a] = Point (-a)
    perform Negate [a] = perform Negate [simplify a]
    hashOperator = show

data CollectOperator = Collect deriving (Enum, Show, Eq)

instance Operator CollectOperator where
    isParametersValid Collect [] = False
    isParametersValid Collect _ = True
    perform Collect = Collection
    hashOperator = show

applyOperator :: (Operator a) => a -> [PointNode] -> PointNode
applyOperator = Operation

data MapOperator = MapOp deriving (Enum, Show, Eq)

instance Operator MapOperator where
    isParametersValid MapOp [Operation op _, Collection []] = False
    isParametersValid MapOp [Operation op _, Collection _] = True
    isParametersValid MapOp _ = False
    hashOperator = show
    perform MapOp [Operation op _, Point _] = error "perform expects [Operation op _, Collection params] instead found [Operation op _, Point _] "
    perform MapOp [Operation op _, Collection params] =
        simplify (Collection nodes)
      where
        nodes = map (applyOperator op) spreadParams
        spreadParams = map (\x -> [x]) params
    perform MapOp [Operation op dummy, pointnode] = perform MapOp [Operation op dummy, simplify pointnode]

data PointNode where
    Operation :: (Operator a) => a -> [PointNode] -> PointNode
    Point :: Float -> PointNode
    Collection :: [PointNode] -> PointNode
    Dummy :: PointNode

simplifyPointNode :: PointNode -> PointNode
simplifyPointNode (Point a) = Point a
simplifyPointNode (Collection as) = Collection (map simplifyPointNode as)
simplifyPointNode (Operation op params) = perform op params
simplifyPointNode Dummy = Dummy

instance Node PointNode where
    simplify = simplifyPointNode
    hash (Operation op params) = concat (hashOperator op : map hash params)
    hash (Collection pointnodes) = concatMap hash pointnodes
    hash (Point a) = show a
    hashEq a b = hash a == hash b
    structEq (Point a) (Point b) = a == b
    structEq (Collection (a : as)) (Collection (b : bs)) = structEq a b && structEq (Collection as) (Collection bs)
    structEq (Operation op1 params1) (Operation op2 params2) = (hashOperator op1 == hashOperator op2) && all (uncurry structEq) (zip params1 params2)
    structEq a b = False

instance Show PointNode where
    show (Point a) = "(" ++ show a ++ ")"
    show (Collection nodes) = "[" ++ concatMap show nodes ++ "]"
    show (Operation op params) = "{\n" ++ "Op:" ++ hashOperator op ++ "; \n" ++ show (Collection params) ++ "\n}"
    show Dummy = "Dummy"
