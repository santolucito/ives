import Prelude hiding (map, foldr, foldl)
import Data.Map.Strict

exs :: [([Map Int Int], Map Int Int)]
exs = [( [ insert 1 1 $ insert 4 4 $ empty,
           insert 2 2 $ insert 3 3 $ empty ]
         , insert 1 1 $ insert 2 2 $
           insert 3 3 $ insert 4 4 $ empty )]

