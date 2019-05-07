module Task4
  ( newCHT
  , getCHT
  , putCHT
  , sizeCHT
  ) where

data ConcurrentHashTable k v = -- realisation

newCHT  :: IO (ConcurrentHashTable k v)

getCHT  :: k -> ConcurrentHashTable k v -> IO (Maybe v)

putCHT  :: k -> v -> ConcurrentHashTable k v -> IO ()

sizeCHT :: ConcurrentHashTable k v -> IO Int
