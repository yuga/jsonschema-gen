module Data.JSON.Schema.Generator.Types
    ( Schema (..)
    , SchemaChoice (..)
    , scString
    , scInteger
    , scNumber
    , scBoolean
    ) where

import Data.Text (Text)

--------------------------------------------------------------------------------

data Schema =
      SCSchema
        { scId          :: !Text
        , scUsedSchema  :: !Text
        , scSimpleType  :: !Schema
        , scDefinitions :: ![(Text, Schema)]
        }
    | SCString
        { scDescription :: !(Maybe Text)
        , scNullable    :: !Bool
        , scFormat      :: !(Maybe Text)
        , scLowerBound  :: !(Maybe Integer)
        , scUpperBound  :: !(Maybe Integer)
        }
    | SCInteger
        { scDescription :: !(Maybe Text)
        , scNullable    :: !Bool
        , scLowerBound  :: !(Maybe Integer)
        , scUpperBound  :: !(Maybe Integer)
        }
    | SCNumber
        { scDescription :: !(Maybe Text)
        , scNullable    :: !Bool
        , scLowerBound  :: !(Maybe Integer)
        , scUpperBound  :: !(Maybe Integer)
        }
    | SCBoolean
        { scDescription :: !(Maybe Text)
        , scNullable    :: !Bool
        }
    | SCConst
        { scTitle       :: !Text
        , scDescription :: !(Maybe Text)
        , scValue       :: !Text
        }
    | SCObject
        { scTitle       :: !Text
        , scDescription :: !(Maybe Text)
        , scNullable    :: !Bool
        , scProperties  :: ![(Text, Schema)]
        , scRequired    :: ![Text]
        }
    | SCArray
        { scTitle       :: !Text
        , scDescription :: !(Maybe Text)
        , scNullable    :: !Bool
        , scItems       :: ![Schema]
        , scLowerBound  :: !(Maybe Integer)
        , scUpperBound  :: !(Maybe Integer)
        }
    | SCOneOf
        { scTitle       :: !Text
        , scDescription :: !(Maybe Text)
        , scChoices     :: ![SchemaChoice]
        }
    | SCRef
        { scReference   :: !Text
        , scNullable    :: !Bool
        }
    | SCNull
    deriving (Show)

data SchemaChoice =
      SCChoiceEnum -- [*]; e.g. "test": {"enum": ["xxx", "yyy", "zzz"]}
        { sctName  :: !Text
        , sctTitle :: !Text
        }
    | SCChoiceArray -- non record of [* -> .. -> *]; e.g. "test": [{"tag": "xxx", "contents": []},...] or "test": [{"xxx": [],},...]
        { sctName   :: !Text
        , sctTitle  :: !Text
        , sctArray  :: ![Schema]
        }
    | SCChoiceMap -- record of [* -> .. -> *] e.g. "test": [{"tag": "xxx", "contents": {"aaa": "yyy",...}},...] or "test": [{"xxx": []},...]
        { sctName     :: !Text
        , sctTitle    :: !Text
        , sctMap      :: ![(Text, Schema)]
        , sctRequired :: ![Text]
        }
    deriving (Show)

scString :: Schema
scString = SCString
    { scDescription = Nothing
    , scNullable = False
    , scFormat = Nothing
    , scLowerBound = Nothing
    , scUpperBound = Nothing
    }

scInteger :: Schema
scInteger = SCInteger
    { scDescription = Nothing
    , scNullable = False
    , scLowerBound = Nothing
    , scUpperBound = Nothing
    }

scNumber :: Schema
scNumber = SCNumber
    { scDescription = Nothing
    , scNullable = False
    , scLowerBound = Nothing
    , scUpperBound = Nothing
    }

scBoolean :: Schema
scBoolean = SCBoolean
    { scDescription = Nothing
    , scNullable = False
    }

