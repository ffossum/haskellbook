module Queue where

data Queue a = Queue
  { enqueue :: [a]
  , dequeue :: [a]
  } deriving (Eq, Show)

empty :: Queue a
empty = Queue [] []

-- adds an item
push :: a -> Queue a -> Queue a
push a qa = qa {enqueue = a : (enqueue qa)}

pop :: Queue a -> Maybe (a, Queue a)
pop (Queue [] [])     = Nothing
pop (Queue es (d:ds)) = Just (d, Queue es ds)
pop (Queue es [])     = pop (Queue [] (reverse es))
