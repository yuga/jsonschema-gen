{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Control.Applicative ((<*>), pure)
import Control.Arrow ((&&&))
import Control.Monad (forM_)
import qualified Data.Aeson as A
import qualified Data.Aeson.TH as A
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.JSON.Schema.Generator as G
import qualified Data.List as List
import Data.Map (fromList)
import Data.Monoid ((<>))
import Data.Proxy (Proxy(Proxy))
import Data.Typeable (typeOf)
import GHC.Generics
import System.Exit (ExitCode(ExitSuccess), exitWith)
import System.IO (Handle, IOMode(WriteMode), hClose, hPutStr, hPutStrLn, withFile)
import System.Process (CreateProcess(..), CmdSpec(RawCommand), StdStream(CreatePipe, Inherit)
    , createProcess, system, waitForProcess)

import Types
import Values

--
-- Instances
--

instance G.JSONSchemaGen RecordType1
instance G.JSONSchemaGen RecordType2
instance G.JSONSchemaGen ProductType1
instance G.JSONSchemaGen ProductType2
instance G.JSONSchemaGen UnitType1
instance G.JSONSchemaGen UnitType2
instance G.JSONSchemaGen MixType1

--instance G.JSONSchemaPrim UnitType2 where
--    toSchemaPrim opts _ = G.scSchemaType . G.toSchema opts $ (Proxy :: Proxy UnitType2)

--
-- TestData
--

data TestDatum =
    forall a. (Generic a, A.GToJSON (Rep a), SchemaName (Rep a))
        => TestDatum { tdName :: String
                     , tdValue :: a
                     }

testDatum :: (Generic a, A.GToJSON (Rep a), SchemaName (Rep a)) => String -> a -> TestDatum
testDatum name p = TestDatum name p

testData :: [TestDatum]
testData =
    [ TestDatum "recordData11"  recordType11
    , testDatum "recordData12"  recordType22
    , testDatum "productData11" productData11
    , testDatum "productData12" productData12
    , testDatum "unitData1"     unitData1
    , testDatum "unitData2"     unitData2
    , testDatum "unitData3"     unitData3
    , testDatum "mixData11"     mixData11
    , testDatum "mixData12"     mixData12
    , testDatum "mixData13"     mixData13
    ]

--
-- Encoder
--

aesonOptions :: Bool -> Bool -> A.SumEncoding -> A.Options
aesonOptions allNullary omitNothing sumEncoding = A.defaultOptions
    { A.allNullaryToStringTag = allNullary
    , A.omitNothingFields = omitNothing
    , A.sumEncoding = sumEncoding
    }

optPatterns :: [A.Options]
optPatterns =
    [ aesonOptions True True
    , aesonOptions True False
    , aesonOptions False True
    , aesonOptions False False
    ]
    <*>
    [ A.defaultTaggedObject
    , A.ObjectWithSingleField
    , A.TwoElemArray
    ]

encode :: (Generic a, A.GToJSON (Rep a)) => A.Options -> a -> BL.ByteString
encode opt a = A.encode (A.genericToJSON opt a)

--
-- Print values as json in python
--

optToStr :: String -> A.Options -> String
optToStr symbol A.Options { A.allNullaryToStringTag = a, A.omitNothingFields = b, A.sumEncoding = c } =
    "# " ++ symbol ++ " (allNullaryToStringTag: " ++ show a
                   ++ ", omitNothingFields: " ++ show b
                   ++ ", sumEncoding: " ++ showC c ++ ")"

showC :: A.SumEncoding -> String
showC A.TwoElemArray = "array"
showC A.ObjectWithSingleField = "object"
showC _ = "tag"

pairsOptSymbol :: [A.Options] -> String -> [(A.Options, String)]
pairsOptSymbol opts name = go opts (1 :: Int)
  where
    go []          _ = []
    go (opt:opts') n = (opt, name ++ "_" ++ show n) : go opts' (n + 1)

printValueAsJson :: (Generic a, A.GToJSON (Rep a)) => Handle -> [A.Options] -> String -> a -> IO ()
printValueAsJson h opts name value =
    forM_ (pairsOptSymbol opts name) . uncurry $ \opt symbol -> do
        hPutStrLn  h $ optToStr symbol opt
        hPutStr    h $ symbol ++ " = json.loads('"
        BL.hPutStr h $ Main.encode opt value
        hPutStrLn  h "')"
        hPutStrLn  h ""

printValueAsJsonInPython :: FilePath -> IO ()
printValueAsJsonInPython path = do
    withFile path WriteMode $ \h -> do
        hPutStrLn h "# -*- coding: utf-8 -*-"
        hPutStrLn h "import json"
        hPutStrLn h ""
        forM_ testData $ \(TestDatum name value) ->
            printValueAsJson h optPatterns name value

--
-- Print type definitions as schema in individualy json files
--

printTypeAsSchema :: (Generic a, G.JSONSchemaGen a, SchemaName (Rep a))
                  => FilePath -> G.Options -> [A.Options] -> Proxy a -> IO ()
printTypeAsSchema dir opts aoptss a = do
    forM_ aoptss $ \aopts -> do
        let fa = fmap from a
        let filename = schemaName opts aopts fa
        let suffix = "." ++ schemaSuffix opts aopts fa
        let path = dir ++ "/" ++ filename
        let opts' = opts { G.schemaIdSuffix = suffix }
        withFile path WriteMode $ \h -> do
            BL.hPutStrLn h $ G.generate' opts' aopts a

class SchemaName f where
    schemaName :: G.Options -> A.Options -> Proxy (f a) -> FilePath
    schemaSuffix :: G.Options -> A.Options -> Proxy (f a) -> String

instance (Datatype d) => SchemaName (D1 d f) where
    schemaName opts aopts p = modName ++ "." ++ typName ++ "." ++ schemaSuffix opts aopts p
      where
        modName = moduleName (undefined :: D1 d f p)
        typName = datatypeName (undefined :: D1 d f p)
    schemaSuffix opts A.Options { A.allNullaryToStringTag = a, A.omitNothingFields = b, A.sumEncoding = c } _ =
        show a ++ "." ++ show b ++ "." ++ showC c ++ G.schemaIdSuffix opts

schemaOptions :: G.Options
schemaOptions = G.defaultOptions
    { G.baseUri = "https://github.com/yuga/jsonschema-gen/tests/"
    , G.schemaIdSuffix = ".json"
    , G.typeRefMap = fromList
        [ (typeOf (undefined :: RecordType2),  "https://github.com/yuga/jsonschema-gen/tests/Types.RecordType2.True.False.tag.json")
        , (typeOf (undefined :: ProductType2), "https://github.com/yuga/jsonschema-gen/tests/Types.ProductType2.True.False.tag.json")
        ]
    }

schemaOptions' :: G.Options
schemaOptions'= schemaOptions
    { G.typeRefMap = fromList
        [ (typeOf (undefined :: RecordType2),  "https://github.com/yuga/jsonschema-gen/tests/Types.RecordType2.True.False.tag.json")
        , (typeOf (undefined :: ProductType2), "https://github.com/yuga/jsonschema-gen/tests/Types.ProductType2.True.False.tag.json")
        , (typeOf (undefined :: UnitType2),    "https://github.com/yuga/jsonschema-gen/tests/Types.UnitType2.True.False.tag.json")
        ]
    , G.fieldTypeMap = fromList  [("recordField1A", G.FieldType (Proxy :: Proxy UnitType2))]
    }

printTypeAsSchemaInJson :: FilePath -> IO ()
printTypeAsSchemaInJson dir = do
    printTypeAsSchema dir schemaOptions' optPatterns (Proxy :: Proxy RecordType1)
    printTypeAsSchema dir schemaOptions  optPatterns (Proxy :: Proxy RecordType2)
    printTypeAsSchema dir schemaOptions  optPatterns (Proxy :: Proxy ProductType1)
    printTypeAsSchema dir schemaOptions  optPatterns (Proxy :: Proxy ProductType2)
    printTypeAsSchema dir schemaOptions  optPatterns (Proxy :: Proxy UnitType1)
    printTypeAsSchema dir schemaOptions  optPatterns (Proxy :: Proxy UnitType2)
    printTypeAsSchema dir schemaOptions  optPatterns (Proxy :: Proxy MixType1)

--
-- Print jsonschema validator in python
--

convertToPythonLoadingSchema :: (Generic a, SchemaName (Rep a)) => G.Options -> [A.Options] -> Proxy a -> ([String], [String])
convertToPythonLoadingSchema opts aoptss a =
    let fa = fmap from a
        toLoader aopts =
            let filename = schemaName opts aopts fa
                symbol = "schema_" ++ map dotToLowline filename
            in (symbol ++ " = json.load(codecs.open(schemaPath + '" ++ filename ++ "', 'r', 'utf-8'))")
        toStore aopts =
            let filename = schemaName opts aopts fa
                symbol = "schema_" ++ map dotToLowline filename
            in ("'" ++ G.baseUri opts ++ filename ++ "' : " ++ symbol)
    in (map toLoader &&& map toStore) aoptss

dotToLowline :: Char -> Char
dotToLowline '.' = '_'
dotToLowline c   = c

printLoadSchemas :: Handle -> IO ()
printLoadSchemas h = do
    let (loader, store) = convertToPythonLoadingSchema schemaOptions' optPatterns (Proxy :: Proxy RecordType1)
                       <> convertToPythonLoadingSchema schemaOptions  optPatterns (Proxy :: Proxy RecordType2)
                       <> convertToPythonLoadingSchema schemaOptions  optPatterns (Proxy :: Proxy ProductType1)
                       <> convertToPythonLoadingSchema schemaOptions  optPatterns (Proxy :: Proxy ProductType2)
                       <> convertToPythonLoadingSchema schemaOptions  optPatterns (Proxy :: Proxy UnitType1)
                       <> convertToPythonLoadingSchema schemaOptions  optPatterns (Proxy :: Proxy UnitType2)
                       <> convertToPythonLoadingSchema schemaOptions  optPatterns (Proxy :: Proxy MixType1)
    hPutStrLn h "schemaPath = os.path.dirname(os.path.realpath(__file__)) + '/'"
    mapM_ (hPutStrLn h) loader
    hPutStrLn h ""
    mapM_ (hPutStrLn h . concat) . chunk 2 $ beginmap : (List.intersperse comma store) ++ [endofmap]
  where
    chunk n = takeWhile (not . null) . map (take n) . iterate (drop n)
    beginmap = "selfStore = { "
    comma    = "            , "
    endofmap = "            }"

printValidate :: Handle -> [A.Options] -> IO ()
printValidate h aoptss = do
    hPutStrLn h "def mkValidator(schema):"
    hPutStrLn h "    resolver = jsonschema.RefResolver(schema[u'id'], schema, store=selfStore)"
    hPutStrLn h "    validator = jsonschema.Draft4Validator(schema, resolver=resolver)"
    hPutStrLn h "    return validator"
    hPutStrLn h ""
    forM_ testData $ \(TestDatum name value) ->
        forM_ (pairsOptSymbol aoptss name) . uncurry $ \aopts dataSymbol -> do
            let schemaFilename = schemaName schemaOptions aopts (fmap from . pure $ value)
            let schemaSymbol  = map dotToLowline schemaFilename
            hPutStrLn h $ "mkValidator(" ++ "schema_" ++ schemaSymbol ++ ").validate(jsondata." ++ dataSymbol ++ ")"

printValidatorInPython :: FilePath -> IO ()
printValidatorInPython path = do
    withFile path WriteMode $ \h -> do
        hPutStrLn h "# -*- coding: utf-8 -*-"
        hPutStrLn h "import codecs"
        hPutStrLn h "import json"
        hPutStrLn h "import jsondata"
        hPutStrLn h "import jsonschema"
        hPutStrLn h "import os"
        hPutStrLn h ""
        printLoadSchemas h
        hPutStrLn h ""
        printValidate h optPatterns

--
-- Run Test
--

pythonProcess :: FilePath -> CreateProcess
pythonProcess dir =
    CreateProcess
        { cmdspec       = RawCommand "python" [dir ++ "/jsonvalidator.py"]
        , cwd           = Nothing
        , env           = Nothing
        , std_in        = CreatePipe
        , std_out       = Inherit
        , std_err       = Inherit
        , close_fds     = False
        , create_group  = False
#if MIN_VERSION_process(1,2,0)
        , delegate_ctlc = True
#endif
        }

runTest :: FilePath -> IO ()
runTest dir = do
    handles <- createProcess $ pythonProcess dir
    case handles of
        (Just hIn, _, _, hP) -> do
            hClose hIn
            ec <- waitForProcess hP
            exitWith ec
        _ -> fail $ "Failed to launch python"

--
-- Main
--

main :: IO ()
main = do
    let dir = "tests"
    printValueAsJsonInPython (dir ++ "/jsondata.py")
    printTypeAsSchemaInJson (dir)
    printValidatorInPython (dir ++ "/jsonvalidator.py")
    ec <- system "python --version"
    case ec of
        ExitSuccess -> runTest dir
        _ -> putStrLn "If you have 'python' in your PATH, this test runs jsonvalidator.py"

