# Exercises: Cardinality

While we haven’t explicitly described the rules for calculating the cardinality of datatypes yet, you might already have an idea of how to do it for simple datatypes with nullary constructors. Try not to overthink these exercises — follow your intuition based on what you know.

1. `data PugType = PugData`
Answer: 1

2. For this one, recall that Bool is also defined with the |:
    ```haskell
    data Airline = PapuAir | CatapultsR'Us | TakeYourChancesUnited
    ```
    Answer: 3

3. Given what we know about Int8, what’s the cardinality of Int16?

    Answer:
    Cardinality of Int8 is 2^8^ = 256.
    Cardinality of Int16 is 2^16^ = 65536.

4. Use the REPL and `maxBound` and `minBound` to examine `Int` and
`Integer`. What can you say about the cardinality of those types?

    Answer: `Int`: -9223372036854775808 and 9223372036854775807.
    This means `Int` has a cardinality of 9223372036854775808 + 9223372036854775807 + 1 = 18446744073709551616. (2^64^)

    `Integer` has no bounds. Its cardinality is infinite.

5. Extra credit (impress your friends!): What’s the connection between the 8 in Int8 and that type’s cardinality of 256?
    Answer: See answer to 3.