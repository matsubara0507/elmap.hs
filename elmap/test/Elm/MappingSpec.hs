module Elm.MappingSpec where

import           Data.Functor.Identity (Identity)
import           Data.Map              (Map)
import           Data.Text             (Text)
import           Elm.Mapping
import           Test.Tasty
import           Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Elm.Mapping"
  [ testGroup "compileElmType"
      [ testCase "compile Haslell's Int to Elm's Int" $
          renderElm (compileElmType (Proxy @ Int)) @?= "Int"
      , testCase "compile Haslell's Integer to Elm's Int" $
          renderElm (compileElmType (Proxy @ Integer)) @?= "Int"
      , testCase "compile Haslell's Float to Elm's Float" $
          renderElm (compileElmType (Proxy @ Float)) @?= "Float"
      , testCase "compile Haslell's Double to Elm's Float" $
          renderElm (compileElmType (Proxy @ Double)) @?= "Float"
      , testCase "compile Haslell's Ordering to Elm's Float" $
          renderElm (compileElmType (Proxy @ Ordering)) @?= "Order"
      , testCase "compile Haslell's Bool to Elm's Bool" $
          renderElm (compileElmType (Proxy @ Bool)) @?= "Bool"
      , testCase "compile Haslell's Char to Elm's Char" $
          renderElm (compileElmType (Proxy @ Char)) @?= "Char"
      , testCase "compile Haslell's String to Elm's String" $
          renderElm (compileElmType (Proxy @ String)) @?= "String"
      , testCase "compile Haslell's Text to Elm's String" $
          renderElm (compileElmType (Proxy @ Text)) @?= "String"
      , testCase "compile Haslell's () to Elm's ()" $
          renderElm (compileElmType (Proxy @ ())) @?= "()"
      , testCase "compile Haslell's [a] to Elm's List a" $
          renderElm (compileElmType (Proxy @ [Int])) @?= "(List Int)"
      , testCase "compile Haslell's Maybe a to Elm's Maybe a" $
          renderElm (compileElmType (Proxy @ (Maybe Int))) @?= "(Maybe Int)"
      , testCase "compile Haslell's Map k v to Elm's Dict k v" $
          renderElm (compileElmType (Proxy @ (Map Int String))) @?= "(Dict Int String)"
      , testCase "compile Haslell's (a,b) to Elm's (a,b)" $
          renderElm (compileElmType (Proxy @ (Int, Float))) @?= "(Int, Float)"
      , testCase "compile Haslell's (a,b,c) to Elm's (a,b,c)" $
          renderElm (compileElmType (Proxy @ (Int, Float, String))) @?= "(Int, Float, String)"
      , testCase "compile Haslell's (a,b,c,d) to Elm's (a,b,c,d)" $
          renderElm (compileElmType (Proxy @ (Int, Float, String, Bool))) @?= "(Int, Float, String, Bool)"
      , testCase "compile Haslell's Identity a to Elm's a" $
          renderElm (compileElmType (Proxy @ (Identity Int))) @?= "Int"
      ]
  , testGroup "renameEType"
      [ testCase "ETyVar" $
          renameEType "Hoge" (ETyVar $ ETVar "a") @?= ETyVar (ETVar "a")
      , testCase "ETyCon" $
          renameEType "Hoge" (ETyCon $ ETCon "Fuga") @?= ETyCon (ETCon "Hoge")
      , testCase "ETyApp" $
          renameEType "Hoge" (ETyApp (ETyCon $ ETCon "Fuga") (ETyCon $ ETCon "Int")) @?= ETyApp (ETyCon $ ETCon "Hoge") (ETyCon $ ETCon "Int")
      , testCase "ETyTuple" $
          renameEType "Hoge" (ETyTuple 2) @?= ETyTuple 2
      ]
  ]
