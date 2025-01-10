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
sum4 = Operation Map2 (Collection [add,ms,ns])
sum5 = Operation Map2 (Collection [add,ns,ns])
-- new struct
negateMs = Operation Map1 (Collection [negateT,ms])
negateNs = Operation Map1 (Collection [negateT,ns])
-- prio
-- new struct
sum3 = Operation Map2 (Collection [add,negateNs,negateNs])
sum2 = Operation Map2 (Collection [add,negateMs,negateMs])
sum1 = Operation Map2 (Collection [add,negateMs,negateNs])
-- prio
-- new struct
sum6 = Operation Map2 (Collection [add,sum1,sum2])
-- prio
-- new struct
sum7 = Operation Map2 (Collection [add,sum6,sum3])
-- prio
-- new struct
sum8 = Operation Map2 (Collection [add,sum7,sum4])
-- prio
-- new struct
out = Operation Map2 (Collection [add,sum8,sum5])
-- prio
