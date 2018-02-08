# Exercises: Programmers

Write a function that generates all possible values of `Programmer`. Use the provided lists of inhabitants of `OperatingSystem` and `ProgLang`.

```haskell
data OperatingSystem =
      GnuPlusLinux
    | OpenBSDPlusNevermindJustBSDStill
    | Mac
    | Windows
    deriving (Eq, Show)

data ProgLang =
      Haskell
    | Agda
    | Idris
    | PureScript
    deriving (Eq, Show)

data Programmer =
    Programmer { os :: OperatingSystem
               , lang :: ProgLang }
    deriving (Eq, Show)

allOperatingSystems :: [OperatingSystem]
allOperatingSystems =
    [ GnuPlusLinux
    , OpenBSDPlusNevermindJustBSDStill
    , Mac
    , Windows
    ]

allLanguages :: [ProgLang]
allLanguages = [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer]
allProgrammers = undefined
```

Answer:
```haskell
allProgrammers :: [Programmer]
allProgrammers = [ Programmer { os = o, lang = l } | o <- allOperatingSystems, l <- allLanguages ]
```