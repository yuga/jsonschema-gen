{-# LANGUAGE DeriveGeneric #-}

import qualified Data.ByteString.Lazy.Char8 as BL
import Data.JSON.Schema.Generator
import Data.Proxy
import GHC.Generics

data User = User
    { name :: String
    , age  :: Int
    , email :: Maybe String
    } deriving Generic

instance JSONSchemaGen User

main :: IO ()
main = BL.putStrLn $ generate (Proxy :: Proxy User)
