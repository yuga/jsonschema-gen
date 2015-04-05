{-# LANGUAGE FlexibleContexts #-}

module Data.JSON.Schema.Generator
    ( module Data.JSON.Schema.Generator.Class
    , module Data.JSON.Schema.Generator.Generate
    , module Data.JSON.Schema.Generator.Generic
    , generate
    , generate'
    , generateWithOptions
    ) where

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.JSON.Schema.Generator.Class (JSONSchemaGen(..), genericToSchema)
import Data.JSON.Schema.Generator.Generate (convert, convertWithOptions)
import Data.JSON.Schema.Generator.Generic (GJSONSchemaGen(..), Options(..), defaultOptions)
import Data.Proxy (Proxy)
import GHC.Generics (Generic, Rep)

import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A

generate :: JSONSchemaGen a => Proxy a -> ByteString
generate = A.encode . convert . toSchema

generate' :: JSONSchemaGen a => A.Options -> Proxy a -> ByteString
generate' opts = A.encode . convertWithOptions opts . toSchema

generateWithOptions :: (Generic a, GJSONSchemaGen (Rep a)) => Options -> A.Options -> Proxy a -> ByteString
generateWithOptions opts aopts = A.encode . convertWithOptions aopts . genericToSchema opts

