module Task5
  ( release
  , runAllocateT
  ) where

newtype AllocateT m a = -- definition

allocate
  :: IO a -- ^ действие, которое выделяет ресурс
  -> (a -> IO ()) -- ^ действие, которое освобождает его
  -> AllocateT m (a, ResourceKey) -- ^ ресурс и указатель на негоs

release :: ResourceKey -> AllocateT m ()

runAllocateT :: AllocateT m a -> m a
