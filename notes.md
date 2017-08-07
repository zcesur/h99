## Functors

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
class Functor (f :: * -> *) where
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
