{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Applicative ((<*>), pure)
import Control.Arrow ((&&&))
import Control.Monad (forM, forM_)
import Control.Monad.Trans.State
import qualified Data.Aeson as A
import qualified Data.Aeson.TH as A
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.JSON.Schema.Generator as G
import qualified Data.JSON.Schema.Generator.Generic as G
import qualified Data.List as List
import Data.Map (fromList)
import Data.Monoid ((<>))
import Data.Proxy (Proxy(Proxy))
import GHC.Generics
import System.Exit (ExitCode(ExitSuccess), exitWith)
import System.IO (Handle, IOMode(WriteMode), hClose, hPutStr, hPutStrLn, withFile)
import System.Process (CreateProcess(..), CmdSpec(RawCommand), StdStream(CreatePipe, Inherit)
    , createProcess, system, waitForProcess)

import Types
import Values

--
-- TestData
--

data TestDatum =
    forall a. (Generic a, A.GToJSON (Rep a), SchemaName (Rep a))
        => TestDatum { tdName :: String
                     , tdValue :: a
                     }

testData :: [TestDatum]
testData =
    [ TestDatum "recordData11"  recordType11
    , TestDatum "recordData12"  recordType22
    , TestDatum "productData11" productData11
    , TestDatum "productData12" productData12
    , TestDatum "unitData1"     unitData1
    , TestDatum "unitData2"     unitData2
    , TestDatum "unitData3"     unitData3
    , TestDatum "mixData11"     mixData11
    , TestDatum "mixData12"     mixData12
    , TestDatum "mixData13"     mixData13
    ]

--
-- Encoder
--

taggedObjectOption :: A.Options
taggedObjectOption = A.defaultOptions
    { A.sumEncoding = A.defaultTaggedObject
    }

objectOption :: A.Options
objectOption = A.defaultOptions
    { A.sumEncoding = A.ObjectWithSingleField
    }

arrayOption :: A.Options
arrayOption = A.defaultOptions
    { A.sumEncoding = A.TwoElemArray
    }

updateOptions :: Bool -> Bool -> A.Options -> A.Options
updateOptions allNullary omitNothing opts = opts
    { A.allNullaryToStringTag = allNullary
    , A.omitNothingFields = omitNothing
    }

optPatterns :: [A.Options]
optPatterns =
    [ updateOptions True True
    , updateOptions True False
    , updateOptions False True
    , updateOptions False False
    ]
    <*>
    [ taggedObjectOption
    , objectOption
    , arrayOption
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
pairsOptSymbol opts name =
    fst . flip runState (0 :: Int) $ forM opts $ \opt -> do
        n <- fmap (+ 1) get
        put $! n
        return (opt, name ++ "_" ++ show n)

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

printTypeAsSchema :: (Generic a, G.GJSONSchemaGen (Rep a), SchemaName (Rep a)) => FilePath -> [A.Options] -> Proxy a -> IO ()
printTypeAsSchema dir opts a = do
    forM_ opts $ \opt -> do
        let filename = schemaName opt (fmap from a)
        let path = dir ++ "/" ++ filename
        let suffix = dropWhile (not . (== '.')) filename
        let schemaOptions' = schemaOptions { G.schemaIdSuffix = suffix }
        withFile path WriteMode $ \h -> do
            BL.hPutStrLn h $ G.generateWithOptions schemaOptions' opt a

schemaOptions :: G.Options
schemaOptions = G.defaultOptions
    { G.baseUri = "https://github.com/yuga/jsonschema-gen/tests/"
    , G.schemaIdSuffix = ".json"
    , G.refSchemaMap = fromList [ ("Types.RecordType2" , "https://github.com/yuga/jsonschema-gen/tests/Types.RecordType2.True.False.tag.json")
                                , ("Types.ProductType2", "https://github.com/yuga/jsonschema-gen/tests/Types.ProductTypw2.True.False.tag.json")
                                ]
    }

class SchemaName f where
    schemaName :: A.Options -> Proxy (f a) -> FilePath

instance (Datatype d) => SchemaName (D1 d f) where
    schemaName A.Options { A.allNullaryToStringTag = a, A.omitNothingFields = b, A.sumEncoding = c } p =
        G.moduleDatatypeName p ++ "." ++ show a ++ "." ++ show b ++ "." ++ showC c ++ G.schemaIdSuffix schemaOptions

printTypeAsSchemaInJson :: FilePath -> IO ()
printTypeAsSchemaInJson dir = do
    printTypeAsSchema dir optPatterns (Proxy :: Proxy RecordType1)
    printTypeAsSchema dir optPatterns (Proxy :: Proxy RecordType2)
    printTypeAsSchema dir optPatterns (Proxy :: Proxy ProductType1)
    printTypeAsSchema dir optPatterns (Proxy :: Proxy ProductType2)
    printTypeAsSchema dir optPatterns (Proxy :: Proxy UnitType1)
    printTypeAsSchema dir optPatterns (Proxy :: Proxy MixType1)

--
-- Print jsonschema validator in python
--

convertToPythonLoadingSchema :: (Generic a, SchemaName (Rep a)) => [A.Options] -> Proxy a -> ([String], [String])
convertToPythonLoadingSchema opts a =
    let fa = fmap from a
        toLoader opt ac =
            let filename = schemaName opt fa
                symbol = "schema_" ++ map dotToLowline filename
            in (symbol ++ " = json.load(codecs.open(schemaPath + '" ++ filename ++ "', 'r', 'utf-8'))") : ac
        toStore opt ac =
            let filename = schemaName opt fa
                symbol = "schema_" ++ map dotToLowline filename
            in ("'" ++ G.baseUri schemaOptions ++ filename ++ "' : " ++ symbol) : ac
    in (foldr toLoader [] &&& foldr toStore []) opts

dotToLowline :: Char -> Char
dotToLowline '.' = '_'
dotToLowline c   = c

printLoadSchemas :: Handle -> IO ()
printLoadSchemas h = do
    let (loader, store) = convertToPythonLoadingSchema optPatterns (Proxy :: Proxy RecordType1)
                       <> convertToPythonLoadingSchema optPatterns (Proxy :: Proxy RecordType2)
                       <> convertToPythonLoadingSchema optPatterns (Proxy :: Proxy ProductType1)
                       <> convertToPythonLoadingSchema optPatterns (Proxy :: Proxy ProductType2)
                       <> convertToPythonLoadingSchema optPatterns (Proxy :: Proxy UnitType1)
                       <> convertToPythonLoadingSchema optPatterns (Proxy :: Proxy MixType1)
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
printValidate h opts = do
    hPutStrLn h "def mkValidator(schema):"
    hPutStrLn h "    resolver = jsonschema.RefResolver(schema[u'id'], schema, store=selfStore)"
    hPutStrLn h "    validator = jsonschema.Draft4Validator(schema, resolver=resolver)"
    hPutStrLn h "    return validator"
    hPutStrLn h ""
    forM_ testData $ \(TestDatum name value) ->
        forM_ (pairsOptSymbol opts name) . uncurry $ \opt dataSymbol -> do
            let schemaFilename = schemaName opt (fmap from . pure $ value)
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

