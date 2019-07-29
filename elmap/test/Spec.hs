module Main where

import qualified Elm.MappingSpec

import           Test.Tasty

main :: IO ()
main = defaultMain $ testGroup "elmap package"
  [ Elm.MappingSpec.tests
  ]
