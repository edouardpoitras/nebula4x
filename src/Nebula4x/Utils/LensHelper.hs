{-# LANGUAGE RankNTypes #-}

module Nebula4x.Utils.LensHelper where

import           Control.Lens
import           Data.Map

-- https://stackoverflow.com/questions/20901710/composing-lenses-with-at-and-ix
-- > view (at' 0) (fromList [(0,'b')])
-- 'b'
-- > view (at'' 0) (fromList [(0,'b')])
-- 'b'
-- > view (at' 1) (fromList [(0,'b')])
-- *** Exception: Could not find key in map!
-- > view (at'' 1) (fromList [(0,'b')])
-- *** Exception: Could not find key in map!
-- > set (at' 0) 'c' (fromList [(0,'b')])
-- fromList [(0,'c')]
-- > set (at'' 0) (Just 'c') (fromList [(0,'b')])
-- fromList [(0,'c')]
-- > set (at' 1) 'c' (fromList [(0,'b')])
-- fromList [(0,'b'),(1,'c')]
-- > set (at'' 1) (Just 'c') (fromList [(0,'b')])
-- fromList [(0,'b'),(1,'c')]
-- > set (at'' 0) Nothing (fromList [(0,'b')])
-- fromList []
at' :: (Ord a) => a -> Lens' (Map a b) b
at' a = lens r s
 where
  r m = case Data.Map.lookup a m of
    (Just b) -> b
    Nothing  -> error "Could not find key in map!"
  s m b' = insert a b' m

at'' :: (Ord a) => a -> Lens (Map a b) (Map a b) b (Maybe b)
at'' a = lens r s
 where
  r m = case Data.Map.lookup a m of
    (Just b) -> b
    Nothing  -> error "Could not find key in map!"
  s m Nothing   = delete a m
  s m (Just b') = insert a b' m
