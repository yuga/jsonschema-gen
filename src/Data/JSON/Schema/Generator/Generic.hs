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

module Data.JSON.Schema.Generator.Generic where

#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative (pure)
import Data.Monoid (mappend, mempty)
#endif

import Data.JSON.Schema.Generator.Types (Schema(..), SchemaChoice(..)
    , scBoolean, scInteger, scNumber, scString)
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
    }

defaultOptions :: Options
defaultOptions = Options { baseUri = "" }

class GJSONSchemaGen f where
    gToSchema :: Options -> Proxy (f a) -> Schema

instance (Datatype d, GSCSimpleType f) => GJSONSchemaGen (D1 d f) where
    gToSchema opts pd = SCSchema
        { scId = Text.pack $ baseUri opts ++ moduleDatatypeName pd
        , scUsedSchema = "http://json-schema.org/draft-04/schema#"
        , scSimpleType = (simpleType opts . fmap unM1 $ pd)
            { scTitle = Text.pack $ moduleDatatypeName pd
            }
        , scDefinitions = mempty
        }

class GSCDatatype f where
    moduleDatatypeName :: Proxy (f a) -> String

instance (Datatype d) => GSCDatatype (D1 d f) where
    moduleDatatypeName _ = moduleName d ++ "." ++ datatypeName d
      where
        d = undefined :: D1 d f p

--------------------------------------------------------------------------------

class GSCSimpleType f where
    simpleType :: Options -> Proxy (f a) -> Schema

-- one constructor that has no argument
instance (Constructor c) => GSCSimpleType (C1 c U1) where
    simpleType _ _ = SCConst
        { scTitle = ""
        , scDescription = Nothing
        , scValue = Text.pack . conName $ (undefined :: C1 c U1 p)
        }

instance (IsRecord f isRecord, GSCSimpleTypeS f isRecord) => GSCSimpleType (C1 c f) where
    simpleType opts _ = (unTagged :: Tagged isRecord Schema -> Schema) . simpleTypeS opts $ (Proxy :: Proxy (f p))

-- there are multiple constructors
instance (AllNullary f allNullary, GSCSimpleTypeM f allNullary) => GSCSimpleType f where
    simpleType opts _ = (unTagged :: Tagged allNullary Schema -> Schema) . simpleTypeM opts $ (Proxy :: Proxy (f p))

class GSCSimpleTypeS f isRecord where
    simpleTypeS :: Options -> Proxy (f a) -> Tagged isRecord Schema

instance (RecordToPairs f) => GSCSimpleTypeS f True where
    simpleTypeS opts _ = Tagged SCObject
        { scTitle = ""
        , scDescription = Nothing
        , scNullable = False
        , scProperties = recordToPairs opts False (Proxy :: Proxy (f p))
        , scRequired = map fst $ recordToPairs opts True (Proxy :: Proxy (f p))
        }

instance (ProductToList f) => GSCSimpleTypeS f False where
    simpleTypeS opts _ = Tagged SCArray
        { scTitle = ""
        , scDescription = Nothing
        , scNullable = False
        , scItems = productToList opts (Proxy :: Proxy (f p))
        , scLowerBound = Nothing
        , scUpperBound = Nothing
        }

class GSCSimpleTypeM f allNullary  where
    simpleTypeM :: Options -> Proxy (f a) -> Tagged allNullary Schema

instance (SumToEnum f) => GSCSimpleTypeM f True where
    simpleTypeM _ _ = Tagged SCOneOf
        { scTitle = ""
        , scDescription = Nothing
        , scNullable = False
        , scChoices = sumToEnum (Proxy :: Proxy (f p))
        }

instance (SumToArrayOrMap f) => GSCSimpleTypeM f False where
    simpleTypeM opts _ = Tagged SCOneOf
        { scTitle = ""
        , scDescription = Nothing
        , scNullable = False
        , scChoices = sumToArrayOrMap opts (Proxy :: Proxy (f p))
        }

--------------------------------------------------------------------------------

class SumToEnum f where
    sumToEnum :: Proxy (f a) -> [SchemaChoice]

instance (Constructor c) => SumToEnum (C1 c U1) where
    sumToEnum _ = pure SCChoiceEnum { scName = Text.pack $ conName (undefined :: C1 c U1 p) }

instance (SumToEnum a, SumToEnum b) => SumToEnum (a :+: b) where
    sumToEnum _ = sumToEnum a `mappend` sumToEnum b
      where
        a = Proxy :: Proxy (a p)
        b = Proxy :: Proxy (b p)

class SumToArrayOrMap f where
    sumToArrayOrMap :: Options -> Proxy (f a) -> [SchemaChoice]

instance (Constructor c, IsRecord f isRecord, ConToArrayOrMap f isRecord)
      => SumToArrayOrMap (C1 c f) where
    sumToArrayOrMap opts _ = pure scTag { sctName = Text.pack . conName $ c }
      where
        scTag = (unTagged :: Tagged isRecord SchemaChoice -> SchemaChoice) . conToArrayOrMap opts $ (Proxy :: Proxy (f p))
        c = undefined :: C1 c f p

instance (SumToArrayOrMap a, SumToArrayOrMap b) => SumToArrayOrMap (a :+: b) where
    sumToArrayOrMap opts _ = sumToArrayOrMap opts a `mappend` sumToArrayOrMap opts b
      where
        a = Proxy :: Proxy (a p)
        b = Proxy :: Proxy (b p)

class ConToArrayOrMap f isRecord where
    conToArrayOrMap :: Options -> Proxy (f a) -> Tagged isRecord SchemaChoice

instance (RecordToPairs f) => ConToArrayOrMap f True where
    conToArrayOrMap opts _ = Tagged SCChoiceMap
        { sctName = ""
        , sctMap = recordToPairs opts False (Proxy :: Proxy (f p))
        }

instance (RecordToPairs f) => ConToArrayOrMap f False where
    conToArrayOrMap opts _ = Tagged SCChoiceArray
        { sctName = ""
        , sctArray = map snd $ recordToPairs opts False (Proxy :: Proxy (f p))
        }

--------------------------------------------------------------------------------

class RecordToPairs f where
    recordToPairs :: Options -> Bool -> Proxy (f a) -> [(Text, Schema)]

instance RecordToPairs U1 where
    recordToPairs _ _ _ = mempty

instance (Selector s, IsNullable a, ToJSONSchemaDef a) => RecordToPairs (S1 s a) where
    recordToPairs opts notMaybe _ 
        | notMaybe && isNullable record = mempty
        | otherwise                     = pure (Text.pack . selName $ selector, toJSONSchemaDef opts record)
      where
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

instance (ToJSONSchemaDef' a) => ToJSONSchemaDef (K1 i a) where
    toJSONSchemaDef opts _ = toJSONSchemaDef' opts (Proxy :: Proxy a)

instance (ToJSONSchemaDef' a) => ToJSONSchemaDef (K1 i (Maybe a)) where
    toJSONSchemaDef opts _  = toJSONSchemaDef' opts (Proxy :: Proxy a)

class ToJSONSchemaDef' a where
    toJSONSchemaDef' :: Options -> Proxy a -> Schema

instance ToJSONSchemaDef' String where
    toJSONSchemaDef' _ _ = scString

instance ToJSONSchemaDef' Text where
    toJSONSchemaDef' _ _ = scString

instance ToJSONSchemaDef' UTCTime where
    toJSONSchemaDef' _ _ = scString { scFormat = Just "date-time" }

instance ToJSONSchemaDef' Int where
    toJSONSchemaDef' _ _ = scInteger

instance ToJSONSchemaDef' Integer where
    toJSONSchemaDef' _ _ = scInteger

instance ToJSONSchemaDef' Float where
    toJSONSchemaDef' _ _ = scNumber

instance ToJSONSchemaDef' Double where
    toJSONSchemaDef' _ _ = scNumber

instance ToJSONSchemaDef' Bool where
    toJSONSchemaDef' _ _ = scBoolean

instance (Generic a, GSCDatatype (Rep a)) => ToJSONSchemaDef' a where
    toJSONSchemaDef' opts a = SCRef
        { scReference = Text.pack $ baseUri opts ++ moduleDatatypeName (fmap from a)
        , scNullable = False
        }

class IsNullable f where
    isNullable :: Proxy (f a) -> Bool

instance IsNullable (K1 i (Maybe a)) where
    isNullable _ = True

instance IsNullable (K1 i a) where
    isNullable _ = False

--------------------------------------------------------------------------------

class IsRecord (f :: * -> *) isRecord | f -> isRecord

instance (IsRecord f isRecord) => IsRecord (f :*: g) isRecord
instance IsRecord (M1 S NoSelector f) False
instance (IsRecord f isRecord) => IsRecord (M1 S c f) isRecord
instance IsRecord (K1 i c) True
instance IsRecord U1 False

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

