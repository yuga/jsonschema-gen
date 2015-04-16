{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

#if __GLASGOW_HASKELL__ < 710
{-# LANGUAGE OverlappingInstances #-}
#endif

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.JSON.Schema.Generator.Generic () where

#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative (pure)
import Data.Monoid (mappend, mempty)
#endif

import Data.JSON.Schema.Generator.Class (JSONSchemaGen(..), JSONSchemaPrim(..)
    , GJSONSchemaGen(..), Options(..), PropType(..))
import Data.JSON.Schema.Generator.Types (Schema(..), SchemaChoice(..)
    , scBoolean, scInteger, scNumber, scString)
import Data.HashMap.Strict (HashMap)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Proxy (Proxy(Proxy))
import Data.Tagged (Tagged(Tagged, unTagged))
import qualified Data.Text as Text
import Data.Typeable (Typeable, typeOf)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (
      Datatype(datatypeName, moduleName), Constructor(conName), Selector(selName)
    , NoSelector
    , C1, D1, K1, M1(unM1), S1, U1, (:+:), (:*:)
    , S)

--------------------------------------------------------------------------------

data Env = Env
    { envModuleName   :: !String
    , envDatatypeName :: !String
    , envConName      :: !String
    , envSelname      :: !(Maybe String)
    }

initEnv :: Env
initEnv = Env "" "" "" Nothing

instance (Datatype d, SchemaType f) => GJSONSchemaGen (D1 d f) where
    gToSchema opts pd = SCSchema
        { scId = Text.pack $ baseUri opts ++ modName ++ "." ++ typName ++ schemaIdSuffix opts
        , scUsedSchema = "http://json-schema.org/draft-04/schema#"
        , scSchemaType = (simpleType opts env . fmap unM1 $ pd)
            { scTitle = Text.pack $ modName ++ "." ++ typName
            }
        , scDefinitions = mempty
        }
      where
        modName = moduleName (undefined :: D1 d f p)
        typName = datatypeName (undefined :: D1 d f p)
        env = initEnv { envModuleName = modName
                      , envDatatypeName = typName
                      }

--------------------------------------------------------------------------------

class SchemaType f where
    simpleType :: Options -> Env -> Proxy (f a) -> Schema

instance (Constructor c) => SchemaType (C1 c U1) where
    simpleType _ env _ = SCConst
        { scTitle = Text.pack $ envModuleName env ++ "." ++ envDatatypeName env ++ "." ++ conname
        , scDescription = Nothing
        , scValue = Text.pack conname
        }
      where
        conname = conName (undefined :: C1 c U1 p)

instance (IsRecord f isRecord, SchemaTypeS f isRecord, Constructor c) => SchemaType (C1 c f) where
    simpleType opts env _ = (unTagged :: Tagged isRecord Schema -> Schema) . simpleTypeS opts env' $ (Proxy :: Proxy (f p))
      where
        env' = env { envConName = conName (undefined :: C1 c f p) }

-- there are multiple constructors
#if __GLASGOW_HASKELL__ >= 710
instance {-# OVERLAPPABLE #-} (AllNullary f allNullary, SchemaTypeM f allNullary) => SchemaType f where
    simpleType opts env _ = (unTagged :: Tagged allNullary Schema -> Schema) . simpleTypeM opts env $ (Proxy :: Proxy (f p))
#else
instance (AllNullary f allNullary, SchemaTypeM f allNullary) => SchemaType f where
    simpleType opts env _ = (unTagged :: Tagged allNullary Schema -> Schema) . simpleTypeM opts env $ (Proxy :: Proxy (f p))
#endif

class SchemaTypeS f isRecord where
    simpleTypeS :: Options -> Env -> Proxy (f a) -> Tagged isRecord Schema

-- Record
instance (RecordToPairs f) => SchemaTypeS f True where
    simpleTypeS opts env _ = Tagged SCObject
        { scTitle = Text.pack $ envModuleName env ++ "." ++ envDatatypeName env ++ "." ++ envConName env
        , scDescription = Nothing
        , scNullable = False
        , scProperties = recordToPairs opts env False (Proxy :: Proxy (f p))
        , scPatternProps = []
        , scRequired = map fst $ recordToPairs opts env True (Proxy :: Proxy (f p))
        }

-- Product
instance (ProductToList f) => SchemaTypeS f False where
    simpleTypeS opts env _ = Tagged SCArray
        { scTitle = Text.pack $ envModuleName env ++ "." ++ envDatatypeName env ++ "." ++ envConName env
        , scDescription = Nothing
        , scNullable = False
        , scItems = productToList opts env (Proxy :: Proxy (f p))
        , scLowerBound = Nothing
        , scUpperBound = Nothing
        }

class SchemaTypeM f allNullary  where
    simpleTypeM :: Options -> Env -> Proxy (f a) -> Tagged allNullary Schema

-- allNullary
instance (SumToEnum f) => SchemaTypeM f True where
    simpleTypeM _ env _ = Tagged SCOneOf
        { scTitle = Text.pack $ envModuleName env ++ "." ++ envDatatypeName env
        , scDescription = Nothing
        , scNullable = False
        , scChoices = sumToEnum env (Proxy :: Proxy (f p))
        }

-- not allNullary
instance (SumToArrayOrMap f) => SchemaTypeM f False where
    simpleTypeM opts env _ = Tagged SCOneOf
        { scTitle = Text.pack $ envModuleName env ++ "." ++ envDatatypeName env
        , scDescription = Nothing
        , scNullable = False
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
        , sctMap = recordToPairs opts env False (Proxy :: Proxy (f p))
        , sctRequired = map fst $ recordToPairs opts env True (Proxy :: Proxy (f p))
        }

instance (RecordToPairs f) => ConToArrayOrMap f False where
    conToArrayOrMap opts env _ = Tagged SCChoiceArray
        { sctName = Text.pack $ envConName env
        , sctTitle = Text.pack $ envModuleName env ++ "." ++ envDatatypeName env ++ "." ++ envConName env
        , sctArray = map snd $ recordToPairs opts env False (Proxy :: Proxy (f p))
        }

--------------------------------------------------------------------------------

class RecordToPairs f where
    recordToPairs :: Options -> Env -> Bool -> Proxy (f a) -> [(Text, Schema)]

instance RecordToPairs U1 where
    recordToPairs _ _ _ _ = mempty

instance (Selector s, IsNullable a, ToJSONSchemaDef a) => RecordToPairs (S1 s a) where
    recordToPairs opts env notMaybe _ 
        | isEmpty   = mempty
        | otherwise = pure ( Text.pack selname
                           , (toJSONSchemaDef opts env' field) { scNullable = isNullable field })
      where
        isEmpty = notMaybe && isNullable field
        field = Proxy :: Proxy (a p)
        selector = undefined :: S1 s a p
        selname = selName selector
        env' = env { envSelname = Just selname }

instance (RecordToPairs a, RecordToPairs b) => RecordToPairs (a :*: b) where
    recordToPairs opts env notMaybe _ =
        recordToPairs opts env notMaybe a `mappend` recordToPairs opts env notMaybe b
      where
        a = Proxy :: Proxy (a p)
        b = Proxy :: Proxy (b p)

class ProductToList f where
    productToList :: Options -> Env -> Proxy (f a) -> [Schema]

instance (IsNullable a, ToJSONSchemaDef a) => ProductToList (S1 s a) where
    productToList opts env _ = pure (toJSONSchemaDef opts env prod) {scNullable = isNullable prod}
      where
        prod = Proxy :: Proxy (a p)

instance (ProductToList a, ProductToList b) => ProductToList (a :*: b) where
    productToList opts env _ = productToList opts env a `mappend` productToList opts env b
      where
        a = Proxy :: Proxy (a p)
        b = Proxy :: Proxy (b p)

--------------------------------------------------------------------------------

class ToJSONSchemaDef f where
    toJSONSchemaDef :: Options -> Env -> Proxy (f a) -> Schema

#if __GLASGOW_HASKELL__ >= 710
instance {-# OVERLAPPING #-} (JSONSchemaPrim a) => ToJSONSchemaDef (K1 i (Maybe a)) where
    toJSONSchemaDef opts env _  = case fieldType opts env of
        Just (PropType p) -> toSchemaPrim opts p
        Nothing -> toSchemaPrim opts (Proxy :: Proxy a)

instance {-# OVERLAPPABLE #-} (JSONSchemaPrim a) => ToJSONSchemaDef (K1 i a) where
    toJSONSchemaDef opts env _ = case fieldType opts env of
        Just (PropType p) -> toSchemaPrim opts p
        Nothing -> toSchemaPrim opts (Proxy :: Proxy a)
#else
instance (JSONSchemaPrim a) => ToJSONSchemaDef (K1 i (Maybe a)) where
    toJSONSchemaDef opts env _  = case fieldType opts env of
        Just (PropType p) -> toSchemaPrim opts p
        Nothing -> toSchemaPrim opts (Proxy :: Proxy a)

instance (JSONSchemaPrim a) => ToJSONSchemaDef (K1 i a) where
    toJSONSchemaDef opts env _ = case fieldType opts env of
        Just (PropType p) -> toSchemaPrim opts p
        Nothing -> toSchemaPrim opts (Proxy :: Proxy a)
#endif

fieldType :: Options -> Env -> Maybe PropType
fieldType opts env = do
    selname <- envSelname env
    Map.lookup selname $ fieldTypeMap opts

--------------------------------------------------------------------------------

#if __GLASGOW_HASKELL__ >= 710
instance {-# OVERLAPPING #-} JSONSchemaPrim String where
    toSchemaPrim _ _ = scString

instance {-# OVERLAPPING #-} JSONSchemaPrim Text where
    toSchemaPrim _ _ = scString

instance {-# OVERLAPPING #-} JSONSchemaPrim UTCTime where
    toSchemaPrim _ _ = scString { scFormat = Just "date-time" }

instance {-# OVERLAPPING #-} JSONSchemaPrim Int where
    toSchemaPrim _ _ = scInteger

instance {-# OVERLAPPING #-} JSONSchemaPrim Integer where
    toSchemaPrim _ _ = scInteger

instance {-# OVERLAPPING #-} JSONSchemaPrim Float where
    toSchemaPrim _ _ = scNumber

instance {-# OVERLAPPING #-} JSONSchemaPrim Double where
    toSchemaPrim _ _ = scNumber

instance {-# OVERLAPPING #-} JSONSchemaPrim Bool where
    toSchemaPrim _ _ = scBoolean

instance {-# OVERLAPS #-} (JSONSchemaPrim a) => JSONSchemaPrim [a] where
    toSchemaPrim opts _ = SCArray
        { scTitle = ""
        , scDescription = Nothing
        , scNullable = False
        , scItems = [toSchemaPrim opts (Proxy :: Proxy a)]
        , scLowerBound = Nothing
        , scUpperBound = Nothing
        }

instance {-# OVERLAPS #-} (JSONSchemaPrim a) => JSONSchemaPrim (Map String a) where
    toSchemaPrim opts _ = SCObject
        { scTitle = ""
        , scDescription = Nothing
        , scNullable = False
        , scProperties = []
        , scPatternProps = [(".*", toSchemaPrim opts (Proxy :: Proxy a))]
        , scRequired = []
        }

instance {-# OVERLAPS #-} (JSONSchemaPrim a) => JSONSchemaPrim (HashMap String a) where
    toSchemaPrim opts _ = SCObject
        { scTitle = ""
        , scDescription = Nothing
        , scNullable = False
        , scProperties = []
        , scPatternProps = [(".*", toSchemaPrim opts (Proxy :: Proxy a))]
        , scRequired = []
        }

instance {-# OVERLAPPABLE #-} (Typeable a, JSONSchemaGen a) => JSONSchemaPrim a where
    toSchemaPrim opts a = SCRef
        { scReference = maybe (scId $ toSchema opts a) Text.pack $ Map.lookup (typeOf (undefined :: a)) (typeRefMap opts)
        , scNullable = False
        }
#else
instance JSONSchemaPrim String where
    toSchemaPrim _ _ = scString

instance JSONSchemaPrim Text where
    toSchemaPrim _ _ = scString

instance JSONSchemaPrim UTCTime where
    toSchemaPrim _ _ = scString { scFormat = Just "date-time" }

instance JSONSchemaPrim Int where
    toSchemaPrim _ _ = scInteger

instance JSONSchemaPrim Integer where
    toSchemaPrim _ _ = scInteger

instance JSONSchemaPrim Float where
    toSchemaPrim _ _ = scNumber

instance JSONSchemaPrim Double where
    toSchemaPrim _ _ = scNumber

instance JSONSchemaPrim Bool where
    toSchemaPrim _ _ = scBoolean

instance (JSONSchemaPrim a) => JSONSchemaPrim [a] where
    toSchemaPrim opts _ = SCArray
        { scTitle = ""
        , scDescription = Nothing
        , scNullable = False
        , scItems = [toSchemaPrim opts (Proxy :: Proxy a)]
        , scLowerBound = Nothing
        , scUpperBound = Nothing
        }

instance (JSONSchemaPrim a) => JSONSchemaPrim (Map String a) where
    toSchemaPrim opts _ = SCObject
        { scTitle = ""
        , scDescription = Nothing
        , scNullable = False
        , scProperties = []
        , scPatternProps = [(".*", toSchemaPrim opts (Proxy :: Proxy a))]
        , scRequired = []
        }

instance (JSONSchemaPrim a) => JSONSchemaPrim (HashMap String a) where
    toSchemaPrim opts _ = SCObject
        { scTitle = ""
        , scDescription = Nothing
        , scNullable = False
        , scProperties = []
        , scPatternProps = [(".*", toSchemaPrim opts (Proxy :: Proxy a))]
        , scRequired = []
        }

instance (Typeable a, JSONSchemaGen a) => JSONSchemaPrim a where
    toSchemaPrim opts a = SCRef
        { scReference = maybe (scId $ toSchema opts a) Text.pack $ Map.lookup (typeOf (undefined :: a)) (typeRefMap opts)
        , scNullable = False
        }
#endif

--------------------------------------------------------------------------------

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

