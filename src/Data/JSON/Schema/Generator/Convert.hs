{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Data.JSON.Schema.Generator.Convert
    ( convert
    ) where

#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative ((<*>))
#endif

import Data.JSON.Schema.Generator.Types (Schema(..), SchemaChoice(..))
import Data.Text (Text, pack)

import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Vector as Vector

--------------------------------------------------------------------------------

convert :: A.Options -> Schema -> A.Value
convert = convert' False

convert' :: Bool -> A.Options -> Schema -> A.Value
convert' = (((A.Object . HashMap.fromList) .) .) . convertToList

convertToList :: Bool -> A.Options -> Schema -> [(Text,A.Value)]
convertToList inArray opts s = foldr1 (++) $
    [ jsId
    , jsSchema
    , jsSimpleType           opts
    , jsTitle
    , jsDescription
    , jsReference
    , jsType         inArray opts
    , jsFormat
    , jsLowerBound
    , jsUpperBound
    , jsValue
    , jsItems                opts
    , jsProperties           opts
    , jsPatternProps         opts
    , jsOneOf                opts
    , jsRequired             opts
    , jsDefinitions          opts
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
jsTitle SCConst  {scTitle = "" } = []
jsTitle SCConst  {scTitle = t  } = [("title", string t)]
jsTitle SCObject {scTitle = "" } = []
jsTitle SCObject {scTitle = t  } = [("title", string t)]
jsTitle SCArray  {scTitle = "" } = []
jsTitle SCArray  {scTitle = t  } = [("title", string t)]
jsTitle SCOneOf  {scTitle = "" } = []
jsTitle SCOneOf  {scTitle = t  } = [("title", string t)]
jsTitle _ = []

jsDescription :: Schema -> [(Text,A.Value)]
jsDescription SCString  {scDescription = (Just d) } = [("description", string d)]
jsDescription SCInteger {scDescription = (Just d) } = [("description", string d)]
jsDescription SCNumber  {scDescription = (Just d) } = [("description", string d)]
jsDescription SCBoolean {scDescription = (Just d) } = [("description", string d)]
jsDescription SCConst   {scDescription = (Just d) } = [("description", string d)]
jsDescription SCObject  {scDescription = (Just d) } = [("description", string d)]
jsDescription SCArray   {scDescription = (Just d) } = [("description", string d)]
jsDescription SCOneOf   {scDescription = (Just d) } = [("description", string d)]
jsDescription _ = []

jsReference :: Schema -> [(Text,A.Value)]
jsReference SCRef {scReference = r} = [("$ref", string r)]
jsReference _ = []

jsType :: Bool -> A.Options -> Schema -> [(Text,A.Value)]
jsType (needsNull -> f) (f -> True) SCString  {scNullable = True } = [("type", array ["string", "null" :: Text])]
jsType (needsNull -> f) (f -> _   ) SCString  {scNullable = _    } = [("type", string "string")]
jsType (needsNull -> f) (f -> True) SCInteger {scNullable = True } = [("type", array ["integer", "null" :: Text])]
jsType (needsNull -> f) (f -> _   ) SCInteger {scNullable = _    } = [("type", string "integer")]
jsType (needsNull -> f) (f -> True) SCNumber  {scNullable = True } = [("type", array ["number", "null" :: Text])]
jsType (needsNull -> f) (f -> _   ) SCNumber  {scNullable = _    } = [("type", string "number")]
jsType (needsNull -> f) (f -> True) SCBoolean {scNullable = True } = [("type", array ["boolean", "null" :: Text])]
jsType (needsNull -> f) (f -> _   ) SCBoolean {scNullable = _    } = [("type", string "boolean")]
jsType (needsNull -> f) (f -> True) SCObject  {scNullable = True } = [("type", array ["object", "null" :: Text])]
jsType (needsNull -> f) (f -> _   ) SCObject  {scNullable = _    } = [("type", string "object")]
jsType (needsNull -> f) (f -> True) SCArray   {scNullable = True } = [("type", array ["array",  "null" :: Text])]
jsType (needsNull -> f) (f -> _   ) SCArray   {scNullable = _    } = [("type", string "array")]
jsType _ _ _ = []

needsNull :: Bool -> A.Options -> Bool
needsNull True  _    = True
needsNull False opts = not (A.omitNothingFields opts)

jsFormat :: Schema -> [(Text,A.Value)]
jsFormat SCString {scFormat = Just f} = [("format", string f)]
jsFormat _ = []

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

jsValue :: Schema -> [(Text,A.Value)]
jsValue SCConst {scValue = v} = [("enum", array [v])]
jsValue _ = []

jsItems :: A.Options -> Schema -> [(Text,A.Value)]
jsItems opts SCArray {scItems = items} = [("items", array . map (convert' True opts) $ items)]
jsItems _ _ = []

jsProperties :: A.Options -> Schema -> [(Text,A.Value)]
jsProperties opts SCObject {scProperties = p} = [("properties", object $ toMap opts p)]
jsProperties _ _ = []

jsPatternProps :: A.Options -> Schema -> [(Text,A.Value)]
jsPatternProps _    SCObject {scPatternProps = []} = []
jsPatternProps opts SCObject {scPatternProps = p } = [("patternProperties", object $ toMap opts p)]
jsPatternProps _ _ = []

jsOneOf :: A.Options -> Schema -> [(Text,A.Value)]
jsOneOf opts SCOneOf {scChoices = t} = choices opts t
jsOneOf _ _ = []

jsRequired :: A.Options -> Schema -> [(Text,A.Value)]
jsRequired       A.Options {A.omitNothingFields = True}  SCObject {scRequired = r  } = [("required", array r)]
jsRequired opts@(A.Options {A.omitNothingFields = _   }) SCObject {scProperties = p} = [("required", array . map fst $ toMap opts p)]
jsRequired _ _ = []

jsDefinitions :: A.Options -> Schema -> [(Text,A.Value)]
jsDefinitions _    SCSchema {scDefinitions = []} = []
jsDefinitions opts SCSchema {scDefinitions = d } = [("definitions", object $ toMap opts d)]
jsDefinitions _ _ = []

--------------------------------------------------------------------------------

array :: A.ToJSON a => [a] -> A.Value
array = A.Array . Vector.fromList . map A.toJSON

string :: Text -> A.Value
string = A.String

number :: Integer -> A.Value
number = A.Number . fromInteger

object :: [A.Pair] -> A.Value
object = A.object

false :: A.Value
false = A.Bool False

--------------------------------------------------------------------------------

choices :: A.Options -> [SchemaChoice] -> [(Text,A.Value)]
choices opts cs
    | isEnum         = [ ("enum", array $ map consAsEnum cs) ]
    | length cs == 1 = [ ("type", "object")
                       , ("properties", head $ map (conAsObject opts) cs)
                       ]
    | otherwise      = [ ("oneOf", array $ map (conAsObject opts) cs) ]
  where
    isEnum = A.allNullaryToStringTag opts && all enumerable cs

enumerable :: SchemaChoice -> Bool
enumerable (SCChoiceEnum _ _) = True
enumerable _                  = False

consAsEnum :: SchemaChoice -> Text
consAsEnum (SCChoiceEnum tag _) = tag
consAsEnum s = error ("conAsEnum could not handle: " ++ show s)

conAsObject :: A.Options -> SchemaChoice -> A.Value
conAsObject opts sc
    | isArray opts = object [ ("type", "array")
                            , ("title", string $ sctTitle sc)
                            , ("items" , conAsObject' opts sc)
                            , ("minItems", number 2)
                            , ("maxItems", number 2)
                            , ("additionalItems", false)
                            ]
    | otherwise    = object [ ("type", "object")
                            , ("title", string $ sctTitle sc)
                            , ("properties", conAsObject' opts sc)
                            , ("additionalProperties", false)]

isArray :: A.Options -> Bool
isArray (A.sumEncoding -> A.TwoElemArray) = True
isArray _ = False

conAsObject' :: A.Options -> SchemaChoice -> A.Value
conAsObject' opts@(A.Options {A.sumEncoding = A.TaggedObject tFld cFld}) sc = conAsTag   opts (pack tFld) (pack cFld) sc
conAsObject' opts@(A.Options {A.sumEncoding = A.TwoElemArray          }) sc = conAsArray opts sc
conAsObject' opts@(A.Options {A.sumEncoding = A.ObjectWithSingleField }) sc = conAsMap   opts sc

conAsTag :: A.Options -> Text -> Text ->  SchemaChoice -> A.Value
conAsTag opts tFld cFld (SCChoiceEnum  tag _)      = object [(tFld, object [("enum", array [tag])]), (cFld, conToArray opts [])]
conAsTag opts tFld cFld (SCChoiceArray tag _ ar)   = object [(tFld, object [("enum", array [tag])]), (cFld, conToArray opts ar)]
conAsTag opts tFld _    (SCChoiceMap   tag _ mp _) = object ((tFld, object [("enum", array [tag])]) : toMap opts mp)

conAsArray :: A.Options -> SchemaChoice -> A.Value
conAsArray opts (SCChoiceEnum  tag _)       = array [object [("enum", array [tag])], conToArray  opts []]
conAsArray opts (SCChoiceArray tag _ ar)    = array [object [("enum", array [tag])], conToArray  opts ar]
conAsArray opts (SCChoiceMap   tag _ mp rq) = array [object [("enum", array [tag])], conToObject opts mp rq]

conAsMap :: A.Options -> SchemaChoice -> A.Value
conAsMap opts (SCChoiceEnum  tag _)       = object [(tag, conToArray  opts [])]
conAsMap opts (SCChoiceArray tag _ ar)    = object [(tag, conToArray  opts ar)]
conAsMap opts (SCChoiceMap   tag _ mp rq) = object [(tag, conToObject opts mp rq)]

conToArray :: A.Options -> [Schema] -> A.Value
conToArray opts ar = object
    [ ("type", "array")
    , ("items", array . map (convert' True opts) $ ar)
    , ("minItems", A.toJSON $ length ar)
    , ("maxItems", A.toJSON $ length ar)
    , ("additionalItems", false)
    ]

conToObject :: A.Options -> [(Text,Schema)] -> [Text] -> A.Value
conToObject opts mp rq = object
    [ ("type", "object")
    , ("properties", object $ toMap opts mp)
    , ("required", required opts)
    , ("additionalProperties", false)
    ]
  where
    required A.Options {A.omitNothingFields = True} = array rq
    required A.Options {A.omitNothingFields = _   } = array . map fst $ toMap opts mp

toMap :: A.Options -> [(Text,Schema)] -> [(Text,A.Value)]
toMap opts mp = map (\(n,v) -> (n,convert' False opts v)) mp -- ++ [("additionalProperties", false)]

