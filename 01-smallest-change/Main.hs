import Control.Monad.State

coins = [200, 100, 50, 20, 10, 5, 2, 1]

-- fit takes an int (in this case the coin value) and returns a function that
-- gives you the quotient and remainder of a number (in this case the remaining
-- balance) divided by that coin value. e.g. fit 200 returns a function like so: 
-- (balance -> (numberOfTimes200FitsIntoBalance, remainingBalance)
--
-- It is wrapped in the State object so it can be used monadically below. State
-- is the generalization of a function that takes some state and returns a value
-- and the updated state in a tuple. quotRem happens to return the quotient and
-- remainder in a tuple so can easily just wrap it up.
fit :: Int -> State Int Int
fit coin = state $ \balance -> quotRem balance coin

-- Takes the total and produces the fewest number of coins that make up that total.
smallestChange :: Int -> [Int]
-- Read the following bottom to top:
smallestChange total =
    concat . -- flatten the nested lists into a single list -> [200, 200, 50, 1]

    zipWith (flip replicate) coins . -- zips the list of quotients with the list of coin values using the
                                     -- `replicate` function. `replicate x y` produces list of size `x` where 
                                     -- all values are `y`. In this case x is the quotient and y is coin value -
                                     -- meaning we repeat the coin value for as many times as it was divided
                                     -- into the remaining balance.
                                     --
                                     -- `flip` swaps the arguments to the `replicate` function so that the
                                     -- arguments match-up correctly.
                                     --
                                     -- Given the quotients calculated below, the result at this
                                     -- point would be [[200, 200], [] [50]. [], [], [], [], [1]]

    fst .  -- 3) The computation returns tuple (value, newState), so we just take the value. In this case
           -- the list of quotients

    runState -- 2) runState unwraps the function so the `total` argument to `smallestChange`
             -- can be applied to it. Given the total is 451, this will produce:
             -- (value, newState) = ([2, 0, 1, 0, 0, 0, 0, 1], 0)

        (mapM fit coins) -- 1) mapM is essentially `map` followed by `sequence`. we
                         -- `map` the state-generator function (fit) over the coin values
                         -- so we produce a list of stateful computations that 
                         -- return the quotient and remainder of the balance divided by
                         -- each of the coin values in the `coins` list.
                         --
                         -- `sequence` then sets up a pipeline of monadic binds from
                         -- one element to the next and adding the result of each computation to
                         -- a list inside a resulting state monad. The bind implementation for State
                         -- will pass the updated state as the argument to the next function in the list -
                         -- meaning the remaining balane is used as the argument to the next State function.
                         --
                         -- `sequence` also wraps this up into a single State Monad so we have a single function
                         -- that takes some state and produces a list of quotients for each of the coins
                         -- and the final remainder (0 in this case)

            $ total -- 0) Once the computation is built up. Apply it to the total

-- main :: IO ()
-- main = putStrLn . show . smallestChange . read =<< getLine >> main
