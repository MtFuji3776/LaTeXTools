module Update where

import TikZ
import qualified Data.Map as M
import Graph

update :: Num a => M.Map Int a -> M.Map Int a -> M.Map Int a
update = M.unionWith (+)

update' :: Num a => M.Map Int a -> M.Map Int a -> M.Map Int a
update' = M.intersectionWith (+)
