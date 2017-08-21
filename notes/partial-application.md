## [Partial application](https://wiki.haskell.org/Partial_application)

Partial application in Haskell involves passing less than the full number of arguments to a function that takes multiple arguments.

For example:

```haskell
add :: Int -> Int -> Int
add x y = x + y
 
addOne = add 1
```

In this example, `addOne` is the result of partially applying add. It is a new function that takes an integer, adds 1 to it and returns that as the result. The important property here is that the `->` operator is right associative, and function application is left associative, meaning the type signature of add actually looks like this:

```haskell
add :: Int -> (Int -> Int)
```

This means that add actually takes one argument and returns a function that takes another argument and returns an `Int`. 
