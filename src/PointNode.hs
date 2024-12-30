{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wall #-}

module PointNode where

class Node a where
    simplify :: a -> a
    hash :: a -> String
    structEq :: a -> a -> Bool
    hashEq :: a -> a -> Bool

class Operator a where
    perform :: a -> PointNode -> PointNode
    hashOperator :: a -> String

data BinaryOperator = Add | Subtract deriving (Enum, Show, Eq)

instance Operator BinaryOperator where
    perform Add (Collection [Point a, Point b]) = Point (a + b)
    perform Add (Collection [a, b]) = perform Add (Collection [simplify a, simplify b])
    perform Add pointnode = perform Add (simplify pointnode)
    perform Subtract (Collection [Point a, Point b]) = Point (a - b)
    perform Subtract (Collection [a, b]) = perform Subtract (Collection [simplify a, simplify b])
    perform Subtract pointnode = perform Subtract (simplify pointnode)
    hashOperator = show

data UnaryOperator = Negate deriving (Enum, Show, Eq)

instance Operator UnaryOperator where
    perform Negate (Collection [Point a]) = Point (-a)
    perform Negate pointnode = perform Negate (simplify pointnode)
    hashOperator = show

data CollectOperator = Collect | Zip deriving (Enum, Show, Eq) -- produces a Collection PointNode when simplified

instance Operator CollectOperator where
    perform Collect cs = cs
    perform Zip (Collection [Point _, _]) = error "Zip expects [Collection as, Collection bs] got [Point _, _]"
    perform Zip (Collection [_, Point _]) = error "Zip expects [Collection as, Collection bs] got [_, Point _]"
    perform Zip (Collection [Collection as, Collection bs]) = Collection (map (\(x, y) -> Collection [x, y]) (zip as bs))
    perform Zip (Collection [pointnodeA, pointnodeB]) = perform Zip (Collection [simplify pointnodeA, simplify pointnodeB])
    perform Zip pointnode = perform Zip (simplify pointnode)
    hashOperator = show

applyOperator :: (Operator a) => a -> PointNode -> PointNode
applyOperator = Operation

data MapOperator = MapOp deriving (Enum, Show, Eq)

instance Operator MapOperator where
    hashOperator = show
    perform MapOp (Collection [Operation _ _, Point _]) = error "perform expects [Operation _ _, Collection params] instead found [Operation op _, Point _] "
    perform MapOp (Collection [Operation op _, Collection params]) =
        simplify (Collection nodes)
      where
        nodes = map (applyOperator op) spreadParams
        spreadParams = map (\x -> Collection [x]) params
    perform MapOp (Collection [Operation op dummy, pointnode]) = perform MapOp (Collection [Operation op dummy, simplify pointnode])
    perform MapOp pointnode = perform MapOp (simplify pointnode)

data PointNode where
    Operation :: (Operator a) => a -> PointNode -> PointNode
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
    hash (Operation op (Collection params)) = concat (hashOperator op : map hash params)
    hash (Operation op pointnode) = hashOperator op ++ hash pointnode
    hash (Collection pointnodes) = concatMap hash pointnodes
    hash (Point a) = show a
    hash Dummy = "Dummy"
    hashEq a b = hash a == hash b
    structEq (Point a) (Point b) = a == b
    structEq (Collection (a : as)) (Collection (b : bs)) = structEq a b && structEq (Collection as) (Collection bs)
    structEq (Operation op1 (Collection params1)) (Operation op2 (Collection params2)) = (hashOperator op1 == hashOperator op2) && all (uncurry structEq) (zip params1 params2)
    structEq _ _ = False

instance Show PointNode where
    show (Point a) = "(" ++ show a ++ ")"
    show (Collection nodes) = "[" ++ concatMap show nodes ++ "]"
    show (Operation op (Collection params)) = "{\n" ++ "Op:" ++ hashOperator op ++ "; \n" ++ show (Collection params) ++ "\n}"
    show (Operation op pointnodes) = "{\n" ++ "Op:" ++ hashOperator op ++ "; \n" ++ hash pointnodes ++ "\n}"
    show Dummy = "Dummy"
