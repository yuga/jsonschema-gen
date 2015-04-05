{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.JSON.Schema.Generator.Class where

import Data.JSON.Schema.Generator.Generic (GJSONSchemaGen(gToSchema), Options)
import Data.JSON.Schema.Generator.Types (Schema)
import Data.Proxy (Proxy)
import GHC.Generics (Generic(from), Rep)

--------------------------------------------------------------------------------

class JSONSchemaGen a where
    toSchema :: Options -> Proxy a -> Schema
    
    default toSchema :: (Generic a, GJSONSchemaGen (Rep a)) => Options -> Proxy a -> Schema
    toSchema = genericToSchema

genericToSchema :: (Generic a, GJSONSchemaGen (Rep a)) => Options -> Proxy a -> Schema
genericToSchema opts = gToSchema opts . fmap from

