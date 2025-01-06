module OutputCompute where
import PointNode
add = Operation Add (Collection [Dummy,Dummy])
sub = Operation Subtract (Collection [Dummy,Dummy])
negateT = Operation Negate (Collection [Dummy])

m1 = Point 5.5
m2 = Point (-6.5)
m3 = Point 1.5
ms = Collection [m1,m2,m3]

n1 = Point 4.5
n2 = Point 5.4
n3 = Point 1.2
ns = Collection [n1,n2,n3]

negateNs = Operation Map1 (Collection [negateT,ns])
negateMs = Operation Map1 (Collection [negateT,ms])

out = Operation Map2 (Collection [add,negateMs,negateNs])

