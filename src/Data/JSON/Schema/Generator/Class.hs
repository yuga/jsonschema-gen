{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.JSON.Schema.Generator.Class where

import Data.JSON.Schema.Generator.Types (Schema)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Proxy (Proxy)
import Data.Typeable (TypeRep)
import GHC.Generics (Generic(from), Rep)

--------------------------------------------------------------------------------

class JSONSchemaGen a where
    toSchema :: Options -> Proxy a -> Schema
    
    default toSchema :: (Generic a, GJSONSchemaGen (Rep a)) => Options -> Proxy a -> Schema
    toSchema = genericToSchema

class GJSONSchemaGen f where
    gToSchema :: Options -> Proxy (f a) -> Schema

genericToSchema :: (Generic a, GJSONSchemaGen (Rep a)) => Options -> Proxy a -> Schema
genericToSchema opts = gToSchema opts . fmap from

--------------------------------------------------------------------------------

-- | Options that specify how to generate schema definition automatically
-- from your datatype.
--
data Options = Options
    { baseUri :: String -- ^ shcema id prefix.
    , schemaIdSuffix :: String -- ^ schema id suffix. File extension for example.
    , refSchemaMap :: Map TypeRep String -- ^ a mapping from datatypes to schema ids.
    }
    deriving Show

-- | Default geerating 'Options':
--
-- @
-- 'Options'
-- { 'baseUri'        = ""
-- , 'schemaIdSuffix' = ""
-- , 'refSchemaMap'   = Map.empty
-- }
-- @
--
defaultOptions :: Options
defaultOptions = Options
    { baseUri = ""
    , schemaIdSuffix = ""
    , refSchemaMap = Map.empty
    }

