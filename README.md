# Amaze

A random depth-first search maze generator in Haskell.

## Examples

```
$ stack build --exec 'amaze 5 5'
   _ _ _ _
|_  |  _  |
|   |_  | |
| |_ _|_| |
|  _ _ _ _|
|_ _ _ _  |
```

```
$ stack build --exec 'amaze 10 10'
   _ _ _ _ _ _ _ _ _
| |     |   | |     |
| | | |_ _| |  _| |_|
|  _|_  |  _|_  |_  |
| | |  _| |   | | | |
|  _| |  _| |_| | | |
|_|  _|_  |_   _ _| |
|  _|  _|_  |_ _|   |
| |_ _   _| |  _ _| |
|_ _ _|   | |_  | | |
|_ _ _ _|_ _ _ _|_  |
```

```
$ stack build --exec 'amaze 15 15'
   _ _ _ _ _ _ _ _ _ _ _ _ _ _
|_  |  _ _  |_    | |     |_  |
|  _|_ _| |_ _ _| |  _| |_ _  |
|_ _  |   |  _|  _|_ _| |   | |
|  _  | |_ _  |_  |  _ _| | |_|
|_  |_| |  _|_ _ _|_   _ _|_  |
|  _|  _|    _ _ _  |_ _ _ _| |
| |  _|_ _|_  |_  |_ _ _  |   |
| | |_   _  |_  |_ _   _|_ _| |
| |_  | | |_  |_ _  |    _|  _|
|   |_ _|  _ _|  _ _|_|_  | | |
|_|_  |  _|  _  |   |   | |_  |
|  _ _| |  _| |_ _|_ _| |  _ _|
|   |  _|_ _ _ _  |  _| | |   |
| | | |   |   |_ _ _|  _|_ _| |
|_|_ _ _|_ _|_ _ _ _ _|_ _ _  |
```

```
$ stack build --exec 'amaze 20 20'
   _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
|   |  _ _ _  |_     _ _ _  |    _  |   |
| | | |   | |_  |_| |_  |  _| | | | | | |
|_| |_ _| |   |_  |_  | | |_  | |  _|_| |
|   | |  _| |  _|_ _ _| |_  |_| |_ _  | |
| |_ _|_ _|_| |   |  _  | |_  | |_ _ _ _|
| |  _ _   _  | |_ _| | |  _ _|   |  _  |
| | |   | | | | |_    | | |  _ _|_ _ _| |
|_ _| | | |_ _|_ _ _| | | | |_ _    |   |
|    _|_ _|  _ _    | |_ _|_  |  _| | |_|
| |_|   |  _ _|  _| |_ _  | | | |  _|_  |
|_  | |_ _ _|  _|  _|   | | |_ _|_  |  _|
|  _|_  |  _ _|  _ _ _|_| |    _|  _|_  |
|  _ _ _| |  _ _ _|  _ _ _| |_ _ _|_  | |
| |   | | | |   |  _|  _  |_  |_ _   _| |
|_ _|_  | |_ _| |_  | | |_  |_ _ _  |   |
|  _ _ _|_  | |_  |_ _|   | |  _  |_| | |
| | |   |  _|  _ _|  _ _| | | | |_ _ _| |
|_  | |_ _|_ _ _ _| |  _|_ _| |  _  |  _|
|  _|_ _ _  |  _ _ _|_ _ _   _| |  _| | |
|_ _ _ _ _|_ _ _ _ _ _ _ _ _|_ _|_ _ _  |
```

```
$ stack build --exec 'amaze 25 25'
   _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
|_ _ _ _ _ _ _   _ _ _  |  _ _ _  |  _ _  |_ _ _  |
|  _ _  |  _ _ _|  _  |_ _|_ _  | | |  _|_ _ _ _  |
|_  | | |_ _  | | | | |      _ _|_ _| |  _   _|  _|
|  _| |_  |   |_ _ _|_  | |_|  _ _   _|_  |_ _ _| |
| |  _ _ _ _|_|  _ _  |_|_ _ _   _|_    |_ _  |_  |
| |_|  _ _  |  _|_  |_ _ _  |  _|  _ _|_ _  |_  | |
|   | | |   |_ _  |   |_  | | | | |    _|  _| | | |
| | | | | |_ _ _|_ _|_  | | |_  | | |_  | |   | | |
| | | | |_ _  |  _   _|_  | |  _| |_  |_ _ _|_|_  |
|_| | |_    |_ _ _| |  _ _| |_ _| |  _ _|   |  _ _|
|  _|_ _ _|_  |  _ _|_  |  _|   | |_|   | |_ _|   |
|    _ _|  _ _| |  _  | |_ _ _| |_ _ _| |  _ _ _| |
|_| |  _ _| |  _| |  _|_ _  | | |  _  |  _|   |_ _|
|  _| | |   |_  | |_ _ _  | | | | | | |_ _| |_ _  |
|   | | | | |   | |_   _|_ _ _| |_  |_ _ _ _ _|   |
| | | |_ _| | |_|_  | |  _ _ _  | |_ _  |  _ _ _|_|
|_| |_  |   |_ _ _ _| |_ _ _  |_ _ _|  _|_ _ _ _  |
|   |  _| | | |    _  | |  _ _|  _  | | |   |   | |
| |_| |  _| |  _| |_ _  |_ _ _ _|  _|_  | |_ _| | |
|  _ _| | | |_ _|_  | |_ _ _ _  |_ _ _ _| | |  _| |
| |  _  |_ _ _ _  | |   | |  _ _|  _ _|  _|  _|   |
| |_  |_  |_   _|_ _| | | |_ _  |   |  _| | |_ _| |
|_  |_  |_  |   |  _  |  _|  _ _|_| |_|   |_ _  | |
| |_  |_|  _| | | | | |_|  _|_    |_   _| |_  | | |
|_ _ _ _ _|_ _|_ _ _|_ _ _ _ _ _|_ _ _|_ _ _ _|_  |
```

```
$ stack build --exec 'amaze 30 30'
   _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
|     |_ _  |  _ _   _ _ _  |  _ _ _ _ _ _ _   _ _ _ _  |   |
| | |_ _  | |_ _  |_ _  | | |_  |  _|   |   |_  |_    |_ _| |
| | |  _ _|    _| | |  _|  _|  _| |  _|_ _|   |_  |_|  _ _ _|
| | | |_  | |_ _ _| |_  | |_ _  |_ _|  _ _ _| | |_  |_  |  _|
| | | |  _|_  |_ _  |  _|_  |  _ _ _ _|_ _ _ _ _ _| |  _|   |
| | | | |  _ _ _ _ _| |   | |_|   |   |  _ _ _ _  | |_  |_| |
|_| | |_ _|  _  |   | | |_| |  _|_ _|_ _|_ _  |  _|_  |_ _  |
|  _|_ _  |   |_ _|_ _|_  | | |   |_ _   _  | | |   |_  | | |
| |  _  | |_|_  |  _ _ _  | | | |_  |  _|_  | |_ _|_  | |_ _|
| |_|   | |  _ _|   | |  _|_  | | | | |   | |    _  | | |   |
| |  _|_| |_  |_ _| |_ _  |  _| | | |_ _| | |_|_  |_| |  _| |
| |_    |_   _|   |  _  | |_  |_  |_ _  | |_ _  |_  | |_  | |
|   |_|_ _  |  _| |_  | |_|  _ _|_ _  | | |   |_  | |_  | | |
| |_ _ _  |_| |  _| | |_ _ _|  _  |  _| |_ _|   |_  | | |_| |
|_ _|   |  _ _|_  |_ _|  _  |  _| | | |_ _  | | |  _ _| |   |
|  _ _| |_  |   | |   |_  |_ _|  _|_ _ _  | |_| |_|  _ _| |_|
|   | |_  | | |_  | |  _| |  _|_  |  _  | |_  |_ _  |_ _ _  |
| | |   | |_|   |_| |_ _ _|_   _|_ _ _|_ _ _|_    |_ _ _ _  |
|_| |_|_ _  | | |  _|_ _    |_  |  _  |  _ _  | |_ _ _ _  |_|
|  _|   |  _| | | |     | | |   | |  _| |  _ _| |_     _|_  |
|_ _ _| | |_ _| | | | |_  |_ _| | |_ _ _| |_ _ _  | |_|   | |
|  _ _ _|_  |  _| | | |  _|   |_|_  |   | |  _  | |_  | |_  |
|_ _ _  |  _| |  _|_| | | | |_  |  _| |_|_ _|  _|_ _  |_  | |
|  _ _ _|  _ _| |    _|_  |  _| | |_ _   _  | |   | |_  | |_|
| |  _  |_|  _ _| |_  |  _| |_ _| |   |_ _| | | |_ _|  _|_  |
|_ _|  _|  _|_ _ _  | |_ _|_  |   | |_ _ _  | |_  |  _ _|  _|
|  _ _|  _|  _ _  |_| |  _ _ _|_| |   | |  _|_  |_ _|   |_  |
| |   | |  _  | | |   | |_ _   _ _| | |  _ _ _| |   | |_  | |
| | |_ _|_ _| | | | | | |  _ _|  _ _| |_|  _  |_ _| |_  |_  |
|_ _ _ _ _ _ _|_ _ _|_|_ _|_ _ _ _ _|_ _ _ _|_ _ _ _ _ _|_  |
```

## References

- https://en.wikipedia.org/wiki/Maze_generation_algorithm
