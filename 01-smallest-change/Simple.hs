module Simple where

coins = [200, 100, 50, 20, 10, 5, 2, 1]

smallestChange :: Int -> [Int]
smallestChange total = concat . zipWith (flip replicate) coins $ go total coins
  where
    go 0         _     = []
    go total (coin:cs) = quotient:(go remainder cs)
      where (quotient, remainder) = total `quotRem` coin
