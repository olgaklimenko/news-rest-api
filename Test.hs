module Test where
import           Prelude                 hiding ( Maybe(..) , (.))
data Maybe a = Nothing | Just a deriving (Show)

-- instance Functor Maybe where
--     fmap f Nothing  = Nothing
--     fmap f (Just a) = Just (f a)

-- instance Applicative Maybe where
--     pure = Just
--     Just ab <*> Just a  = Just $ ab a
--     Nothing <*> _       = Nothing
--     Just ab <*> Nothing = Nothing

-- instance Monad Maybe where
--     return = Just
--     (>>=)  = monadBind
--     -- Nothing >>= amb = Nothing

-- monadBind :: Maybe a -> (a -> Maybe b) -> Maybe b
-- monadBind (Just a) amb = amb a

(.)    :: (b -> c) -> (a -> b) -> a -> c
(.) f g x = f (g x)

(.!) :: (a->b) -> (b->c) -> a -> c
(.!) f g x = g (f x)
