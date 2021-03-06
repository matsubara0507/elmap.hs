{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import qualified Data.Text           as Text
import           Servant.Elm.Mapping
import qualified Test
import           Test.Tasty
import           Test.Tasty.HUnit

main :: IO ()
main = do
  expected <- Text.pack <$> readFile "test/Expected.elm"
  defaultMain $ testGroup "Servant.Elm.Mapping"
    [ testCase "generateElmForAPI" $ (Text.unlines $ generateElmForAPI Test.api) @?= expected
    ]
