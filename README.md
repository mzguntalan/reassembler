# reassembler

## Note to Self

My attempt to learn Haskell and compilers by writing an optimizing compiler.. maybe also a small language.

## Compute language

The `compute` language is a simple language meant to be very literal and low-level for the compiller(reassembler). It has one syntax

```
[VARNAME] [FUNCNAME] [PARAM_1] [PARAM_2] ...
```

If `[FUNCNAME]` is `Point`, then the parameter can only be a float. Otherwise, the parameter can only be a variable name.

## reassembler

The eventual goal of the reassembler is to be able to make the optimal cuda kernels for your program. To do this it does the following:

- [x] determine dependencies -> Each line receives a priority: which is 1 more than the max priority of the parameters
- [x] re-arrange by ascending priority -> This ensures that statements will only have dependencies with lines above them
- [x] for each priority, group operations the are structurally equal -> meaning, they can potentially be run in a SIMT fashion
- [ ] analyze how to fuse operations (operations can be fused with their dependencies or broken up) to run optimally (NOT DONE -- currently here)
- [x] Current Alternative: transpile to a Haskell program

## Demo

The demo can be found in the playground. I only have the following functions: Add, Subtract, Negate, Map1, Map2, Zip, Collection.

- `sample.compute` -> a sample `.compute` program
- `output.hs` -> the transpiled HS code which has delimeters for new priority or new struct(meaning a new group of structurally the same operations)

For quicker reference, here is a small demo, I have also added line numbers:

```
[01] add Add Dummy Dummy
[02] sub Subtract Dummy Dummy
[03] negateT Negate Dummy
[04]
[05] m1 Point 5.5
[06] m2 Point (-6.5)
[07] m3 Point 1.5
[08] ms Collection m1 m2 m3
[09]
[10] n1 Point 4.5
[11] n2 Point 5.4
[12] n3 Point 1.2
[13] ns Collection n1 n2 n3
[14]
[15] negateNs Map1 negateT ns
[16] negateMs Map1 negateT ms
[17]
[18] out Map2 add negateMs negateNs
```

From the above `.compute` file, we produce the `output` haskell code as follows. Notice that the lines have been re-arranged and grouped.

```haskell
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
```

`-- struct` marks the start of a new structure (operator), and `-- prio` marks an increase of priority after that line.
