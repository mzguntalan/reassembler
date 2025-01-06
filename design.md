# Design <LANGUAGE> compute

`compute` is the language that will use this compiler. File extension is `.compute`

```
[VAR-NAME] Point [Float Number]
[VAR-NAME] [Operation] [Parameter 1] [Parameter 2] ...
[Last VAR-NAME] ...
```

`[Operation]` can be `Point` `Collection` `Add` `Subtract` `Negate` `Zip` `Map1` and `Map2`.

The `[Last VAR-NAME]` will be the output of the program. Only `Point` can accept a float.
