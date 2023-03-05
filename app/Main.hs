{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           Data.Char
import           Options.Generic

data CesarCrypt = CesarCrypt
  { shift :: Int,
    msg   :: String,
    len   :: Maybe Int
  }
  deriving (Generic, Show)

instance ParseRecord CesarCrypt

listLen :: [a] -> Int
listLen [] = 0
listLen xs = sum [1 | _ <- xs]

cesar :: CesarCrypt -> [Char]
cesar payload = map chr m
  where
    m = map mp (msg payload)
      where
        mp = (\x -> ((((ord x) - ord 'a')) `mod` 26) + (shift payload) + ord 'a')

main :: IO ()
main = do
  input <- getRecord "simple cesar crypt"
  putStrLn .  show  $ cesar ( input :: CesarCrypt )
