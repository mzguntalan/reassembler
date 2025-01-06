module OutputCompute where
import PointNode
n3 = Point 1.2
n2 = Point 5.4
n1 = Point 4.5
m3 = Point 1.5
m2 = Point (-6.5)
m1 = Point 5.5

ns = Collection [n1,n2,n3]
ms = Collection [m1,m2,m3]
negateT = Operation Negate (Collection [Dummy])
sub = Operation Subtract (Collection [Dummy,Dummy])
add = Operation Add (Collection [Dummy,Dummy])

negateMs = Operation Map1 (Collection [negateT,ms])
negateNs = Operation Map1 (Collection [negateT,ns])

out = Operation Map2 (Collection [add,negateMs,negateNs])
