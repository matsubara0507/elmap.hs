module Servant.Elm.Mapping
  ( module X
  , ElmOptions (..)
  , defElmImports
  , defElmOptions
  , generateElmModule
  , generateElmModuleWith
  , generateElmForAPI
  , generateElmForAPIWith
  ) where

import           Data.List                     (intercalate, intersperse, nub)
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T
import           Data.Text.IO                  as TIO
import qualified Data.Text.Lazy                as TL
import           Elm.Mapping                   as X
import qualified Elm.Module                    as Elm
import           Servant.Elm.Internal.Generate (ElmOptions (..), Namespace,
                                                defElmImports, defElmOptions,
                                                docToText,
                                                generateElmForRequest)
import           Servant.Elm.Mapping.Foreign   as X
import qualified Servant.Foreign               as F
import           System.Directory              (createDirectoryIfMissing)
import           Text.PrettyPrint.Leijen.Text

generateElmModuleWith ::
  (F.HasForeign LangElmap EType api, F.GenerateList EType (F.Foreign EType api))
  => ElmOptions
  -> Namespace
  -> Text
  -> FilePath
  -> [Elm.DefineElm]
  -> Proxy api
  -> IO ()
generateElmModuleWith options namespace imports rootDir typeDefs api = do
  createDirectoryIfMissing True filePath
  TIO.writeFile fileName out
  where
    out = T.unlines $
      [ "module " <> moduleName <> " exposing(..)"
      , imports
      , T.pack $ Elm.makeModuleContentWithAlterations (elmAlterations options) typeDefs
      ] ++ generateElmForAPIWith options api
    moduleName = T.pack $ intercalate "." namespace
    filePath = intercalate "/" $ rootDir : init namespace
    fileName = intercalate "/" $ filePath : [last namespace ++ ".elm"]


generateElmModule ::
     ( F.HasForeign LangElmap EType api
     , F.GenerateList EType (F.Foreign EType api)
     )
  => Namespace
  -> Text
  -> FilePath
  -> [Elm.DefineElm]
  -> Proxy api
  -> IO ()
generateElmModule = generateElmModuleWith defElmOptions

generateElmForAPI ::
  (F.HasForeign LangElmap EType api, F.GenerateList EType (F.Foreign EType api))
  => Proxy api
  -> [Text]
generateElmForAPI = generateElmForAPIWith defElmOptions

generateElmForAPIWith ::
  (F.HasForeign LangElmap EType api, F.GenerateList EType (F.Foreign EType api))
  => ElmOptions
  -> Proxy api
  -> [Text]
generateElmForAPIWith opts =
  intersperse "" . nub . map docToText
    . map (generateElmForRequest opts) . getEndpoints
