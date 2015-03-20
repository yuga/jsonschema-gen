{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Data.JSON.Schema.Generator.Generate
    ( convert
    , convertWithOptions
    ) where

import Control.Applicative ((<*>))
import Control.Arrow (second)
import Data.JSON.Schema.Generator.Types
import Data.Text (Text)

import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Scientific as S
import qualified Data.Vector as Vector

import Debug.Trace (trace)

--------------------------------------------------------------------------------

convert :: Schema -> A.Value
convert = convertWithOptions A.defaultOptions

convertWithOptions :: A.Options -> Schema -> A.Value
convertWithOptions = convert' False

convert' :: Bool -> A.Options -> Schema -> A.Value
convert' = (((A.Object . HashMap.fromList) .) .) . convertToList

convertToList :: Bool -> A.Options -> Schema -> [(Text,A.Value)]
convertToList inArray opts s = foldr1 (++) $
    [ jsId
    , jsSchema
    , jsSimpleType          opts
    , jsTitle
    , jsDescription
    , jsReference
    , jsType        inArray opts
    , jsLowerBound
    , jsUpperBound
    , jsRequired
    , jsValue
    , jsProperties          opts
    , jsDefinitions         opts
    ] <*> [s]

--------------------------------------------------------------------------------

jsId :: Schema -> [(Text,A.Value)]
jsId (SCSchema {scId = i}) = [("id", string i)]
jsId _ = []

jsSchema :: Schema -> [(Text,A.Value)]
jsSchema SCSchema {scUsedSchema = s} = [("$schema", string s)]
jsSchema _ = []

jsSimpleType :: A.Options -> Schema -> [(Text,A.Value)]
jsSimpleType opts SCSchema {scSimpleType = s} = convertToList False opts s
jsSimpleType _ _ = []

jsTitle :: Schema -> [(Text,A.Value)]
jsTitle SCConst  {scTitle = t } = [("title", string t)]
jsTitle SCObject {scTitle = t } = [("title", string t)]
jsTitle SCArray  {scTitle = t } = [("title", string t)]
jsTitle SCOneOf  {scTitle = t } = [("title", string t)]
jsTitle _ = []

jsDescription :: Schema -> [(Text,A.Value)]
jsDescription SCString {scDescription = (Just d) } = [("description", string d)]
jsDescription SCNumber {scDescription = (Just d) } = [("description", string d)]
jsDescription SCConst  {scDescription = (Just d) } = [("description", string d)]
jsDescription SCObject {scDescription = (Just d) } = [("description", string d)]
jsDescription SCArray  {scDescription = (Just d) } = [("description", string d)]
jsDescription SCOneOf  {scDescription = (Just d) } = [("description", string d)]
jsDescription _ = []

jsReference :: Schema -> [(Text,A.Value)]
jsReference SCRef {scReference = r} = [("$ref", string r)]
jsReference _ = []

jsType :: Bool -> A.Options -> Schema -> [(Text,A.Value)]
jsType (needsNull -> f) (f -> True) SCString {scNullable = True } = [("type", array ["string", "null" :: Text])]
jsType (needsNull -> f) (f -> _   ) SCString {scNullable = _    } = [("type", string "string")]
jsType (needsNull -> f) (f -> True) SCNumber {scNullable = True } = [("type", array ["number", "null" :: Text])]
jsType (needsNull -> f) (f -> _   ) SCNumber {scNullable = _    } = [("type", string "number")]
jsType (needsNull -> f) (f -> True) SCObject {scNullable = True } = [("type", array ["object", "null" :: Text])]
jsType (needsNull -> f) (f -> _   ) SCObject {scNullable = _    } = [("type", string "object")]
jsType (needsNull -> f) (f -> True) SCArray  {scNullable = True } = [("type", array ["array",  "null" :: Text])]
jsType (needsNull -> f) (f -> _   ) SCArray  {scNullable = _    } = [("type", string "array")]
jsType (needsNull -> f) (f -> True) SCOneOf  {scNullable = True } = [("type", array ["object", "null" :: Text])]
jsType (needsNull -> f) (f -> _   ) SCOneOf  {scNullable = _    } = [("type", string "object")]
jsType _ _ _ = []

needsNull :: Bool -> A.Options -> Bool
needsNull True  _    = True
needsNull False opts = not (A.omitNothingFields opts)

jsLowerBound :: Schema -> [(Text,A.Value)]
jsLowerBound SCString {scLowerBound = (Just n)} = [("minLength", number n)]
jsLowerBound SCNumber {scLowerBound = (Just n)} = [("mininum",   number n)]
jsLowerBound SCArray  {scLowerBound = (Just n)} = [("minItems",  number n)]
jsLowerBound _ = []

jsUpperBound :: Schema -> [(Text,A.Value)]
jsUpperBound SCString {scUpperBound = (Just n)} = [("maxLength", number n)]
jsUpperBound SCNumber {scUpperBound = (Just n)} = [("maximum",   number n)]
jsUpperBound SCArray  {scUpperBound = (Just n)} = [("maxItems",  number n)]
jsUpperBound _ = []

jsRequired :: Schema -> [(Text,A.Value)]
jsRequired SCObject {scRequired = r} = [("required", array r)]
jsRequired _ = []

jsValue :: Schema -> [(Text,A.Value)]
jsValue SCConst {scValue = v} = [("enum", array [v])]
jsValue _ = []

jsProperties :: A.Options -> Schema -> [(Text,A.Value)]
jsProperties opts SCObject {scProperties = p} = [("properties", toObject False opts p)]
jsProperties opts SCOneOf  {scChoices = t}    = choices opts t
jsProperties _ _ = []

jsDefinitions :: A.Options -> Schema -> [(Text,A.Value)]
jsDefinitions opts SCSchema {scDefinitions = d} = [("definitions", toObject False opts d)]
jsDefinitions _ _ = []

--------------------------------------------------------------------------------

array :: A.ToJSON a => [a] -> A.Value
array = A.Array . Vector.fromList . map A.toJSON

emptyArray :: A.Value
emptyArray = A.emptyArray

string :: Text -> A.Value
string = A.String

number :: Integer -> A.Value
number n = A.Number $ S.scientific n 0

object :: [A.Pair] -> A.Value
object = A.object

false :: A.Value
false = A.Bool False

toObject :: Bool -> A.Options -> [(Text,Schema)] -> A.Value
toObject inArray opts = object . map (second (convert' inArray opts))

--------------------------------------------------------------------------------

choices :: A.Options -> [SchemaChoice] -> [(Text,A.Value)]
choices opts cs
    | isEnum         = [ ("enum", array $ foldr conAsEnum [] cs) ]
    | length cs == 1 = [ ("type", "object")
                       , ("properties", head . foldr (conAsObject opts) [] $ cs)
                       ]
    | otherwise      = [ ("type", "object")
                       , ("properties", object [("oneOf", array $ foldr (conAsObject opts) [] cs)])
                       ]
  where
    isEnum = A.allNullaryToStringTag opts && all enumerable cs

enumerable :: SchemaChoice -> Bool
enumerable (SCChoiceEnum _) = True
enumerable _                = False

conAsEnum :: SchemaChoice -> [Text] -> [Text]
conAsEnum (SCChoiceEnum tag) ac = tag : ac
conAsEnum s ac = trace ("conAsEnum could not handle: " ++ show s) $ ac

conAsObject :: A.Options -> SchemaChoice -> [A.Value] -> [A.Value]
conAsObject opts (SCChoiceEnum   tag)    ac = objTag   opts tag    : ac
conAsObject opts (SCChoiceArray  tag ar) ac = objArray opts tag ar : ac
conAsObject opts (SCChoiceMap    tag mp) ac = objMap   opts tag mp : ac

objTag :: A.Options -> Text -> A.Value
objTag _ tag = object
    [ ("tag", object [("enum", array [tag])])
    , ("contents", object [ ("type", "array")
                          , ("items", emptyArray)
                          , ("additionalItems", false)
                          ]
      )
    ]

objArray :: A.Options -> Text -> [Schema] -> A.Value
objArray opts tag ar = object
    [ ("tag", object [("enum", array [tag])])
    , ("contents", object [ ("type", "array")
                          , ("items", array . map (convert' True opts) $ ar)
                          , ("additionalItems", false)
                          ]
      )
    ]

objMap :: A.Options -> Text -> [(Text,Schema)] -> A.Value
objMap opts tag mp = object $
    ( "tag", object [("enum", array [tag])])
    : foldr (\(n,v) ac' -> (n,convert' False opts v):ac') [] mp

