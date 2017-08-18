## [Functors](http://adit.io/posts/2013-04-17-functors,_applicatives,_and_monads_in_pictures.html)

When a value is wrapped in a context, you can't apply a normal function to it:

This is where `fmap` comes in. `fmap` knows how to apply functions to values that are wrapped in a context. For example, suppose you want to apply `(+3)` to `Just 2`. Use `fmap`:

```haskell
> fmap (+3) (Just 2)
Just 5
```

Bam! `fmap` shows us how it's done! But how does `fmap` know how to apply the function?
Just what is a `Functor`, really?

`Functor` is a typeclass. Here's the definition:

```haskell
class Functor f where
  fmap :: (a -> b) -> f a -> f b
```

A `Functor` is any data type that defines how `fmap` applies to it.

So we can do this:

```haskell
> fmap (+3) (Just 2)
Just 5
```

And `fmap` magically applies this function, because `Maybe` is a `Functor`. It specifies how `fmap` applies to `Just`s and `Nothing`s:

```haskell
instance Functor Maybe where
    fmap f (Just val) = Just (f val)
    fmap f Nothing = Nothing
```

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
