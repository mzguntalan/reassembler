# Design <LANGUAGE> compute

`compute` is the language that will use this compiler. File extension is `.compute`

```
[VAR-NAME] Point [Float Number]
[VAR-NAME] [Operation] [Parameter 1] [Parameter 2] ...
[Last VAR-NAME] ...
```

`[Operation]` can be `Point` `Collection` `Add` `Subtract` `Negate` `Zip` `Map1` and `Map2`.

The `[Last VAR-NAME]` will be the output of the program. Only `Point` can accept a float.

## Compilation

To compile, reassembler will go through each statement one by one,

- `Point` Statements will always have no dependencies and are given priority 0
- non-`Point` statements will always have at least 1 dependencies and these are exactly each of the parameters. It receives a priority of one more than the highest priority of the parameters.

After going all the lines, each priority (0 - inf), will have a bunch of statements that are collission free (no interdependence) and so operations that are (structurally) the same can be theoretically ran together in parallel with the same instruction (maybe in SIMT fashion).

[Insert think about HOF operations like Map and Zip]
