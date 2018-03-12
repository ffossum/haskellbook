module MonadInstances where

import           Control.Monad

type SSI = (String, String, Int)

applicativeMatchesMonad_prop :: (Monad m, Eq (m b)) => MatchProp m a b
applicativeMatchesMonad_prop f m = ((pure f) <*> m) == ((pure f) `ap` m)

type MatchProp m a b = (a -> b) -> (m a) -> Bool
