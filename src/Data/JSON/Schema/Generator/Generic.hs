{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

#if __GLASGOW_HASKELL__ < 710
{-# LANGUAGE OverlappingInstances #-}
#endif

module Data.JSON.Schema.Generator.Generic where

#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative (pure)
import Data.Monoid (mappend, mempty)
#endif

import Data.JSON.Schema.Generator.Types (Schema(..), SchemaChoice(..)
    , scBoolean, scInteger, scNumber, scString)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy(Proxy))
import Data.Tagged (Tagged(Tagged, unTagged))
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic(from), Rep
    , Datatype(datatypeName, moduleName), Constructor(conName), Selector(selName)
    , NoSelector
    , C1, D1, K1, M1(unM1), S1, U1, (:+:), (:*:)
    , S
    , from)

import qualified Data.Text as Text

--------------------------------------------------------------------------------

data Options = Options
    { baseUri :: String
    , schemaIdSuffix :: String
    , refSchemaMap :: Map String String
    }
    deriving Show

defaultOptions :: Options
defaultOptions = Options
    { baseUri = ""
    , schemaIdSuffix = ""
    , refSchemaMap = Map.empty
    }

data Env = Env
    { envModuleName   :: !String
    , envDatatypeName :: !String
    , envConName      :: !String
    , envSelname      :: !String
    }

initEnv :: Env
initEnv = Env "" "" "" ""

class GJSONSchemaGen f where
    gToSchema :: Options -> Proxy (f a) -> Schema

instance (Datatype d, GSCSimpleType f) => GJSONSchemaGen (D1 d f) where
    gToSchema opts pd = SCSchema
        { scId = Text.pack $ baseUri opts ++ moduleDatatypeName pd ++ schemaIdSuffix opts
        , scUsedSchema = "http://json-schema.org/draft-04/schema#"
        , scSimpleType = (simpleType opts env . fmap unM1 $ pd)
            { scTitle = Text.pack $ moduleDatatypeName pd
            }
        , scDefinitions = mempty
        }
      where
        env = initEnv { envModuleName = moduleName' pd
                      , envDatatypeName = datatypeName' pd
                      }

class GSCDatatype f where
    moduleDatatypeName :: Proxy (f a) -> String
    moduleName' :: Proxy (f a) -> String
    datatypeName' :: Proxy (f a) -> String

instance (Datatype d) => GSCDatatype (D1 d f) where
    moduleDatatypeName d = moduleName' d ++ "." ++ datatypeName' d
    moduleName' _ = moduleName (undefined :: D1 d f p)
    datatypeName' _ = datatypeName (undefined :: D1 d f p)

--------------------------------------------------------------------------------

class GSCSimpleType f where
    simpleType :: Options -> Env -> Proxy (f a) -> Schema

instance (Constructor c) => GSCSimpleType (C1 c U1) where
    simpleType _ env _ = SCConst
        { scTitle = Text.pack $ envModuleName env ++ "." ++ envDatatypeName env ++ "." ++ conname
        , scDescription = Nothing
        , scValue = Text.pack conname
        }
      where
        conname = conName (undefined :: C1 c U1 p)

instance (IsRecord f isRecord, GSCSimpleTypeS f isRecord, Constructor c) => GSCSimpleType (C1 c f) where
    simpleType opts env _ = (unTagged :: Tagged isRecord Schema -> Schema) . simpleTypeS opts env' $ (Proxy :: Proxy (f p))
      where
        env' = env { envConName = conName (undefined :: C1 c f p) }

-- there are multiple constructors
#if __GLASGOW_HASKELL__ >= 710
instance {-# OVERLAPPABLE #-} (AllNullary f allNullary, GSCSimpleTypeM f allNullary) => GSCSimpleType f where
    simpleType opts env _ = (unTagged :: Tagged allNullary Schema -> Schema) . simpleTypeM opts env $ (Proxy :: Proxy (f p))
#else
instance (AllNullary f allNullary, GSCSimpleTypeM f allNullary) => GSCSimpleType f where
    simpleType opts env _ = (unTagged :: Tagged allNullary Schema -> Schema) . simpleTypeM opts env $ (Proxy :: Proxy (f p))
#endif

class GSCSimpleTypeS f isRecord where
    simpleTypeS :: Options -> Env -> Proxy (f a) -> Tagged isRecord Schema

-- Record
instance (RecordToPairs f) => GSCSimpleTypeS f True where
    simpleTypeS opts env _ = Tagged SCObject
        { scTitle = Text.pack $ envModuleName env ++ "." ++ envDatatypeName env ++ "." ++ envConName env
        , scDescription = Nothing
        , scNullable = False
        , scProperties = recordToPairs opts False (Proxy :: Proxy (f p))
        , scRequired = map fst $ recordToPairs opts True (Proxy :: Proxy (f p))
        }

-- Product
instance (ProductToList f) => GSCSimpleTypeS f False where
    simpleTypeS opts env _ = Tagged SCArray
        { scTitle = Text.pack $ envModuleName env ++ "." ++ envDatatypeName env ++ "." ++ envConName env
        , scDescription = Nothing
        , scNullable = False
        , scItems = productToList opts (Proxy :: Proxy (f p))
        , scLowerBound = Nothing
        , scUpperBound = Nothing
        }

class GSCSimpleTypeM f allNullary  where
    simpleTypeM :: Options -> Env -> Proxy (f a) -> Tagged allNullary Schema

-- allNullary
instance (SumToEnum f) => GSCSimpleTypeM f True where
    simpleTypeM _ env _ = Tagged SCOneOf
        { scTitle = Text.pack $ envModuleName env ++ "." ++ envDatatypeName env
        , scDescription = Nothing
        , scChoices = sumToEnum env (Proxy :: Proxy (f p))
        }

-- not allNullary
instance (SumToArrayOrMap f) => GSCSimpleTypeM f False where
    simpleTypeM opts env _ = Tagged SCOneOf
        { scTitle = Text.pack $ envModuleName env ++ "." ++ envDatatypeName env
        , scDescription = Nothing
        , scChoices = sumToArrayOrMap opts env (Proxy :: Proxy (f p))
        }

--------------------------------------------------------------------------------

class SumToEnum f where
    sumToEnum :: Env -> Proxy (f a) -> [SchemaChoice]

instance (Constructor c) => SumToEnum (C1 c U1) where
    sumToEnum env _ = pure SCChoiceEnum
        { sctName = Text.pack $ conName (undefined :: C1 c U1 p)
        , sctTitle = Text.pack $ envModuleName env ++ "." ++ envDatatypeName env ++ "."
                              ++ conName (undefined :: C1 c U1 p)
        }

instance (SumToEnum a, SumToEnum b) => SumToEnum (a :+: b) where
    sumToEnum env _ = sumToEnum env a `mappend` sumToEnum env b
      where
        a = Proxy :: Proxy (a p)
        b = Proxy :: Proxy (b p)

class SumToArrayOrMap f where
    sumToArrayOrMap :: Options -> Env -> Proxy (f a) -> [SchemaChoice]

instance (Constructor c, IsRecord f isRecord, ConToArrayOrMap f isRecord)
      => SumToArrayOrMap (C1 c f) where
    sumToArrayOrMap opts env _ =
        pure . (unTagged :: Tagged isRecord SchemaChoice -> SchemaChoice) . conToArrayOrMap opts env' $ (Proxy :: Proxy (f p))
      where
        env' = env { envConName = conName (undefined :: C1 c f p) }

instance (SumToArrayOrMap a, SumToArrayOrMap b) => SumToArrayOrMap (a :+: b) where
    sumToArrayOrMap opts env _ = sumToArrayOrMap opts env a `mappend` sumToArrayOrMap opts env b
      where
        a = Proxy :: Proxy (a p)
        b = Proxy :: Proxy (b p)

class ConToArrayOrMap f isRecord where
    conToArrayOrMap :: Options -> Env -> Proxy (f a) -> Tagged isRecord SchemaChoice

instance (RecordToPairs f) => ConToArrayOrMap f True where
    conToArrayOrMap opts env _ = Tagged SCChoiceMap
        { sctName = Text.pack $ envConName env
        , sctTitle = Text.pack $ envModuleName env ++ "." ++ envDatatypeName env ++ "." ++ envConName env
        , sctMap = recordToPairs opts False (Proxy :: Proxy (f p))
        , sctRequired = map fst $ recordToPairs opts True (Proxy :: Proxy (f p))
        }

instance (RecordToPairs f) => ConToArrayOrMap f False where
    conToArrayOrMap opts env _ = Tagged SCChoiceArray
        { sctName = Text.pack $ envConName env
        , sctTitle = Text.pack $ envModuleName env ++ "." ++ envDatatypeName env ++ "." ++ envConName env
        , sctArray = map snd $ recordToPairs opts False (Proxy :: Proxy (f p))
        }

--------------------------------------------------------------------------------

class RecordToPairs f where
    recordToPairs :: Options -> Bool -> Proxy (f a) -> [(Text, Schema)]

instance RecordToPairs U1 where
    recordToPairs _ _ _ = mempty

instance (Selector s, IsNullable a, ToJSONSchemaDef a) => RecordToPairs (S1 s a) where
    recordToPairs opts notMaybe _ 
        | isEmpty   = mempty
        | otherwise = pure ( Text.pack . selName $ selector
                           , (toJSONSchemaDef opts record) { scNullable = isNullable record })
      where
        isEmpty = notMaybe && isNullable record
        record = Proxy :: Proxy (a p)
        selector = undefined :: S1 s a p

instance (RecordToPairs a, RecordToPairs b) => RecordToPairs (a :*: b) where
    recordToPairs opts notMaybe _ = recordToPairs opts notMaybe a `mappend` recordToPairs opts notMaybe b
      where
        a = Proxy :: Proxy (a p)
        b = Proxy :: Proxy (b p)

class ProductToList f where
    productToList :: Options -> Proxy (f a) -> [Schema]

instance (IsNullable a, ToJSONSchemaDef a) => ProductToList (S1 s a) where
    productToList opts _ = pure (toJSONSchemaDef opts prod) {scNullable = isNullable prod}
      where
        prod = Proxy :: Proxy (a p)

instance (ProductToList a, ProductToList b) => ProductToList (a :*: b) where
    productToList opts _ = productToList opts a `mappend` productToList opts b
      where
        a = Proxy :: Proxy (a p)
        b = Proxy :: Proxy (b p)

--------------------------------------------------------------------------------

class ToJSONSchemaDef f where
    toJSONSchemaDef :: Options -> Proxy (f a) -> Schema

#if __GLASGOW_HASKELL__ >= 710
instance {-# OVERLAPPING #-} (JSONSchemaPrim a) => ToJSONSchemaDef (K1 i (Maybe a)) where
    toJSONSchemaDef opts _  = toJSONSchemaPrim opts (Proxy :: Proxy a)

instance {-# OVERLAPPABLE #-} (JSONSchemaPrim a) => ToJSONSchemaDef (K1 i a) where
    toJSONSchemaDef opts _ = toJSONSchemaPrim opts (Proxy :: Proxy a)
#else
instance (JSONSchemaPrim a) => ToJSONSchemaDef (K1 i (Maybe a)) where
    toJSONSchemaDef opts _  = toJSONSchemaPrim opts (Proxy :: Proxy a)

instance (JSONSchemaPrim a) => ToJSONSchemaDef (K1 i a) where
    toJSONSchemaDef opts _ = toJSONSchemaPrim opts (Proxy :: Proxy a)
#endif

class JSONSchemaPrim a where
    toJSONSchemaPrim :: Options -> Proxy a -> Schema

#if __GLASGOW_HASKELL__ >= 710
instance {-# OVERLAPPING #-} JSONSchemaPrim String where
    toJSONSchemaPrim _ _ = scString

instance {-# OVERLAPPING #-} JSONSchemaPrim Text where
    toJSONSchemaPrim _ _ = scString

instance {-# OVERLAPPING #-} JSONSchemaPrim UTCTime where
    toJSONSchemaPrim _ _ = scString { scFormat = Just "date-time" }

instance {-# OVERLAPPING #-} JSONSchemaPrim Int where
    toJSONSchemaPrim _ _ = scInteger

instance {-# OVERLAPPING #-} JSONSchemaPrim Integer where
    toJSONSchemaPrim _ _ = scInteger

instance {-# OVERLAPPING #-} JSONSchemaPrim Float where
    toJSONSchemaPrim _ _ = scNumber

instance {-# OVERLAPPING #-} JSONSchemaPrim Double where
    toJSONSchemaPrim _ _ = scNumber

instance {-# OVERLAPPING #-} JSONSchemaPrim Bool where
    toJSONSchemaPrim _ _ = scBoolean

instance {-# OVERLAPPABLE #-} (Generic a, GSCDatatype (Rep a)) => JSONSchemaPrim a where
    toJSONSchemaPrim opts a = SCRef
        { scReference = Text.pack . fromMaybe defaultUri $ Map.lookup mdname (refSchemaMap opts)
        , scNullable = False
        }
      where
        mdname = moduleDatatypeName (fmap from a)
        defaultUri = baseUri opts ++ mdname ++ schemaIdSuffix opts
#else
instance JSONSchemaPrim String where
    toJSONSchemaPrim _ _ = scString

instance JSONSchemaPrim Text where
    toJSONSchemaPrim _ _ = scString

instance JSONSchemaPrim UTCTime where
    toJSONSchemaPrim _ _ = scString { scFormat = Just "date-time" }

instance JSONSchemaPrim Int where
    toJSONSchemaPrim _ _ = scInteger

instance JSONSchemaPrim Integer where
    toJSONSchemaPrim _ _ = scInteger

instance JSONSchemaPrim Float where
    toJSONSchemaPrim _ _ = scNumber

instance JSONSchemaPrim Double where
    toJSONSchemaPrim _ _ = scNumber

instance JSONSchemaPrim Bool where
    toJSONSchemaPrim _ _ = scBoolean

instance (Generic a, GSCDatatype (Rep a)) => JSONSchemaPrim a where
    toJSONSchemaPrim opts a = SCRef
        { scReference = Text.pack . fromMaybe defaultUri $ Map.lookup mdname (refSchemaMap opts)
        , scNullable = False
        }
      where
        mdname = moduleDatatypeName (fmap from a)
        defaultUri = baseUri opts ++ mdname ++ schemaIdSuffix opts
#endif

class IsNullable f where
    isNullable :: Proxy (f a) -> Bool

#if __GLASGOW_HASKELL__ >= 710
instance {-# OVERLAPPING #-} IsNullable (K1 i (Maybe a)) where
    isNullable _ = True

instance {-# OVERLAPPABLE #-} IsNullable (K1 i a) where
    isNullable _ = False
#else
instance IsNullable (K1 i (Maybe a)) where
    isNullable _ = True

instance IsNullable (K1 i a) where
    isNullable _ = False
#endif

--------------------------------------------------------------------------------

class IsRecord (f :: * -> *) isRecord | f -> isRecord

#if __GLASGOW_HASKELL__ >= 710
instance (IsRecord f isRecord) => IsRecord (f :*: g) isRecord
instance {-# OVERLAPPING #-} IsRecord (M1 S NoSelector f) False
instance {-# OVERLAPPABLE #-} (IsRecord f isRecord) => IsRecord (M1 S c f) isRecord
instance IsRecord (K1 i c) True
instance IsRecord U1 False
#else
instance (IsRecord f isRecord) => IsRecord (f :*: g) isRecord
instance IsRecord (M1 S NoSelector f) False
instance (IsRecord f isRecord) => IsRecord (M1 S c f) isRecord
instance IsRecord (K1 i c) True
instance IsRecord U1 False
#endif

--------------------------------------------------------------------------------

class AllNullary (f :: * -> *) allNullary | f -> allNullary

instance ( AllNullary a allNullaryL
         , AllNullary b allNullaryR
         , And allNullaryL allNullaryR allNullary
         ) => AllNullary (a :+: b) allNullary
instance AllNullary a allNullary => AllNullary (M1 i c a) allNullary
instance AllNullary (a :*: b) False
instance AllNullary (K1 i c) False
instance AllNullary U1 True

--------------------------------------------------------------------------------

data True
data False

class    And bool1 bool2 bool3 | bool1 bool2 -> bool3

instance And True  True  True
instance And False False False
instance And False True  False
instance And True  False False

--------------------------------------------------------------------------------

