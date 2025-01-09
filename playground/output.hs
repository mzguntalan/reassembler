module OutputCompute where
import PointNode
-- new struct
n3 = Point 1.2
n2 = Point 5.4
n1 = Point 4.5
m3 = Point 1.5
m2 = Point (-6.5)
m1 = Point 5.5
-- prio
-- new struct
negateT = Operation Negate (Collection [Dummy])
-- new struct
sub = Operation Subtract (Collection [Dummy,Dummy])
-- new struct
ms = Collection [m1,m2,m3]
ns = Collection [n1,n2,n3]
-- new struct
add = Operation Add (Collection [Dummy,Dummy])
-- prio
-- new struct
negateMs = Operation Map1 (Collection [negateT,ms])
negateNs = Operation Map1 (Collection [negateT,ns])
-- prio
-- new struct
out = Operation Map2 (Collection [add,negateMs,negateNs])
-- prio
