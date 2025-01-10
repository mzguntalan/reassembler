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
