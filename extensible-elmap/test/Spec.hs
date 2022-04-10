{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import           Data.Extensible
import           Data.Extensible.Elm.Mapping
import           Elm.Mapping
import           Test.Tasty
import           Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ testGroup "Data.Extensible.Elm.Mapping"
  [ testGroup "compileElmRecordTypeWith"
      [ testCase "compile Haskell's User type definition" $
          renderElm (compileElmType (Proxy @User)) @?= "User"
      , testCase "compile Haskell's Message type difinition" $
          renderElm (compileElmType (Proxy @Message)) @?= "Message"
      ]
  , testGroup "compileElmRecordAliasWith"
      [ testCase "compile Haskell's User type definition" $
          renderElm (compileElmDef (Proxy @User)) @?= unlines
            [ "type alias User  ="
            , "   { id: Int"
            , "   , name: String"
            , "   , admin: Bool"
            , "   }"
            ]
      , testCase "compile Haskell's Message type difinition" $
          renderElm (compileElmDef (Proxy @Message)) @?= unlines
            [ "type alias Message  ="
            , "   { id: Int"
            , "   , text: String"
            , "   , author: User"
            , "   }"
            ]
      ]
  ]

type User = Record
  '[ "id"    >: Int
   , "name"  >: String
   , "admin" >: Bool
   ]

instance IsElmType User where
  compileElmType = compileElmRecordTypeWith "User"

instance IsElmDefinition User where
  compileElmDef = ETypeAlias . compileElmRecordAliasWith "User"

type Message = Record
  '[ "id"     >: Int
   , "text"   >: String
   , "author" >: User
   ]

instance IsElmType Message where
  compileElmType = compileElmRecordTypeWith "Message"

instance IsElmDefinition Message where
  compileElmDef = ETypeAlias . compileElmRecordAliasWith "Message"
