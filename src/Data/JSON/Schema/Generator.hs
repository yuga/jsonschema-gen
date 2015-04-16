{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module:     Data.JSON.Schema.Generator
-- Copyright:  (c) 2015 Shohei Murayama
-- License:    BSD3
-- Maintainer: Shohei Murayama <shohei.murayama@gmail.com>
-- Stability:  experimental
--
-- A generator for JSON Schemas from ADT.
--
module Data.JSON.Schema.Generator
    (
    -- * How to use this library
    -- $use

    -- * Genenerating JSON Schema
      Options(..), FieldType(..), defaultOptions
    , generate, generate'

    -- * Type conversion
    , JSONSchemaGen(toSchema)
    , JSONSchemaPrim(toSchemaPrim)
    , convert

    -- * Generic Schema class
    , GJSONSchemaGen(gToSchema)
    , genericToSchema
    ) where

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.JSON.Schema.Generator.Class (JSONSchemaGen(toSchema), JSONSchemaPrim(toSchemaPrim)
    , GJSONSchemaGen(gToSchema), Options(..), FieldType(..), defaultOptions, genericToSchema)
import Data.JSON.Schema.Generator.Convert (convert)
import Data.JSON.Schema.Generator.Generic ()
import Data.Proxy (Proxy)

import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A

-- | Generate a JSON Schema from a proxy value of a type.
-- This uses the default options to generate schema in json format.
--
generate :: JSONSchemaGen a
         => Proxy a     -- ^ A proxy value of the type from which a schema will be generated.
         -> ByteString
generate = A.encode . convert A.defaultOptions . toSchema defaultOptions

-- | Generate a JSON Schema from a proxy vaulue of a type.
-- This uses the specified options to generate schema in json format.
--
generate' :: JSONSchemaGen a
          => Options     -- ^ Schema generation 'Options'.
          -> A.Options   -- ^ Encoding 'A.Options' of aeson.
          -> Proxy a     -- ^ A proxy value of the type from which a schema will be generated.
          -> ByteString
generate' opts aopts = A.encode . convert aopts . toSchema opts

-- $use
-- Example:
--
-- > {-# LANGUAGE DeriveGeneric #-}
-- >
-- > import qualified Data.ByteString.Lazy.Char8 as BL
-- > import Data.JSON.Schema.Generator
-- > import Data.Proxy
-- > import GHC.Generics
-- >
-- > data User = User
-- >     { name :: String
-- >     , age  :: Int
-- >     , email :: Maybe String
-- >     } deriving Generic
-- >
-- > instance JSONSchemaGen User
-- >
-- > main :: IO ()
-- > main = BL.putStrLn $ generate (Proxy :: Proxy User)
--
-- Let's run the above script, we can get on stdout (the following json is formatted with jq):
--
-- @
--  {
--   \"required\": [
--     \"name\",
--     \"age\",
--     \"email\"
--   ],
--   \"$schema\": \"http://json-schema.org\/draft-04\/schema#\",
--   \"id\": \"Main.User\",
--   \"title\": \"Main.User\",
--   \"type\": \"object\",
--   \"properties\": {
--     \"email\": {
--       \"type\": [
--         \"string\",
--         \"null\"
--       ]
--     },
--     \"age\": {
--       \"type\": \"integer\"
--     },
--     \"name\": {
--       \"type\": \"string\"
--     }
--   }
-- }
-- @

