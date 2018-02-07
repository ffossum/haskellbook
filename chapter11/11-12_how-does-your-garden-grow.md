# Exercises: How does your Garden grow?

1. Given the type

```haskell
data FlowerType =
      Gardenia
    | Daisy
    | Rose
    | Lilac
    deriving Show

type Gardener = String
data Garden = Garden Gardener FlowerType
    deriving Show
```
What is the sum of products normal form of Garden?

Answer:
```haskell
type Gardener = String
data Garden =
      Gardenia Gardener
    | Daisy Gardener
    | Rose Gardener
    | Lilac Gardener
    deriving Show
```

