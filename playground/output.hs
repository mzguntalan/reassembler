module OutputCompute where
import PointNode (BinaryOperator (Subtract), PointNode (Collection, Operation, Point))
m = Point 5.5
n = Point (-6.5)
s = Operation Subtract (Collection [m, n])
