{-# LANGUAGE FlexibleContexts #-}

module Data.JSON.Schema.Generator
    ( module Data.JSON.Schema.Generator.Generate
    , module Data.JSON.Schema.Generator.Generic
    , generate
    , generateWithOptions
    ) where

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.JSON.Schema.Generator.Generate (convert, convertWithOptions)
import Data.JSON.Schema.Generator.Generic (JSONSchemaGen(..), GJSONSchemaGen(..), Options(..)
    , defaultOptions, gJSONSchema, gJSONSchemaWithOptions)
import Data.Proxy (Proxy)
import GHC.Generics (Generic, Rep)

import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A

generate :: (Generic a, GJSONSchemaGen (Rep a)) => Proxy a -> ByteString
generate = A.encode . convert . gJSONSchema

generateWithOptions :: (Generic a, GJSONSchemaGen (Rep a)) => Options -> A.Options -> Proxy a -> ByteString
generateWithOptions opts aopts = A.encode . convertWithOptions aopts . gJSONSchemaWithOptions opts

