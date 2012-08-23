module StateCPS where

data StateCPS s w v = S { unS :: s -> (s -> v -> w) -> w }

instance Monad (StateCPS s w) where
  return v = S $ \s k -> k s v
  m >>= f = S $ \s k -> (unS m) s (\s' b -> (unS $ f b) s' k)

-- initial continuation
k0 :: a -> b -> b
k0 = \_ v -> v

runM :: StateCPS s v v -> s -> v
runM m = \s -> (unS m) s k0

fetch :: StateCPS v w v
fetch = S $ \s k -> k s s
store :: v -> StateCPS v () ()
store v = S $ \_ k -> k v ()
