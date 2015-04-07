{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Values where

import qualified Data.Aeson.TH as A
import Types

--
-- Value
--

recordType11 :: RecordType1
recordType11 =
    RecordData11
    { recordField11 = "field11"
    , recordField12 = 12
    , recordField13 = 1.3
    , recordField14 = Nothing
    , recordField15 =
        RecordData21
        { recordField21 = "field21"
        , recordField22 = Nothing
        }
    , recordField16 = ["recordField16-1", "recordField17-2"]
    , recordField17 = [17]
    , recordField18 = []
    , recordField19 = Just 19
    }

recordType22 :: RecordType2
recordType22 =
    RecordData22
    { recordField21 = "field21"
    , recordField22 = Nothing
    , recordField23 = 23
    }

productData11 :: ProductType1
productData11 = ProductData11 "param11" 12 1.3 Nothing (ProductData21 "param21" Nothing)

productData12 :: ProductType2
productData12 = ProductData22 "param21" Nothing 23

unitData1 :: UnitType1
unitData1 = UnitData1

unitData2 :: UnitType1
unitData2 = UnitData2

unitData3 :: UnitType1
unitData3 = UnitData3

mixData11 :: MixType1
mixData11 = MixData11

mixData12 :: MixType1
mixData12 = MixData12 "param31" Nothing

mixData13 :: MixType1
mixData13 =
    MixData13
    { recordField31 = "field31"
    , recordField32 = Nothing
    , recordField33 = 33
    }

--
-- Test Print
--

$(A.deriveJSON A.defaultOptions ''RecordType2)
$(A.deriveJSON A.defaultOptions ''ProductType2)

