module Src.Modules.Gamble (realDice
    , rollDice
    , coinToss
    , getRandom
) where

import System.Random


realDice :: Int
realDice = 4

rollDice :: IO Int
rollDice = randomRIO (1, 6)

coinToss :: IO Int
coinToss = randomRIO (0, 1)

getRandom :: Int -> Int -> IO Int
getRandom lo hi = randomRIO (lo, hi)
