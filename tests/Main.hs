{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Applicative ((<*>))
import Control.Monad (forM_)
import Control.Monad.Trans.State
import Control.Monad.IO.Class
import qualified Data.Aeson as A
import qualified Data.Aeson.TH as A
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.JSON.Schema.Generator as G
import Data.Proxy (Proxy(Proxy))
import GHC.Generics

import Types
import Values

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

printOpt :: String -> A.Options -> IO ()
printOpt symbol (A.Options { A.allNullaryToStringTag = a, A.omitNothingFields = b, A.sumEncoding = c }) =
    putStrLn $ "# " ++ symbol ++ " (allNullaryToStringTag: " ++ show a
                              ++ ", omitNothingFields: " ++ show b
                              ++ ", sumEncoding: " ++ showC c ++ ")"
  where
    showC A.TwoElemArray = "array"
    showC A.ObjectWithSingleField = "object"
    showC _ = "tag"

printImport :: IO ()
printImport = do
    putStrLn "import json"
    putStrLn ""

printValueAsJson :: (Generic a, A.GToJSON (Rep a)) => [A.Options] -> String -> a -> IO ()
printValueAsJson opts name a = do
    _ <- flip runStateT (0 :: Int) $ forM_ opts $ \opt -> do
        n <- get
        put $! (n + 1 `mod` 12)
        liftIO $ do
            let symbol = name ++ "_" ++ show (n + 1)
            printOpt symbol opt
            putStr $ symbol ++ " = json.loads('"
            BL.putStr (Main.encode opt a)
            putStrLn "')"
            putStrLn ""
    return ()

printValueAsJsonInPy :: IO ()
printValueAsJsonInPy = do
    putStrLn "-*- coding: utf-8 -*-"
    printImport
    printValueAsJson optPatterns "recordType11"  recordType11
    printValueAsJson optPatterns "recordType12"  recordType22
    printValueAsJson optPatterns "productData11" productData11
    printValueAsJson optPatterns "productData12" productData12
    printValueAsJson optPatterns "unitData1"     unitData1
    printValueAsJson optPatterns "unitData2"     unitData2
    printValueAsJson optPatterns "unitData3"     unitData3
    printValueAsJson optPatterns "mixData11"     mixData11
    printValueAsJson optPatterns "mixData12"     mixData12
    printValueAsJson optPatterns "mixData13"     mixData13

--
-- Print type definitions as schema in individualy json files
--

printTypeAsSchema :: (Generic a, G.GJSONSchemaGen (Rep a)) => [A.Options] -> Proxy a -> IO ()
printTypeAsSchema opts a = return ()

printTypeAsSchemaInJson :: IO ()
printTypeAsSchemaInJson = do
    printTypeAsSchema optPatterns (Proxy :: Proxy RecordType1)
    --printTypeAsSchema optPatterns (Proxy :: Proxy RecordType2)

--
-- Main
--

main :: IO ()
main = do
    printValueAsJsonInPy
    printTypeAsSchemaInJson

