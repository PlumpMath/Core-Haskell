{-# LANGUAGE OverloadedStrings #-}

module SyntaxConfig (getConfig, SyntaxConfig(..), updatedNames) where

import Data.Aeson (FromJSON, parseJSON, Value(..), (.:), decode)
import Control.Applicative ((<*>), (<$>))
import Control.Monad (liftM, mzero)
import qualified Data.Vector as V (toList)
import qualified Data.ByteString.Lazy as B (readFile)
--import qualified Data.ByteString.Lazy.Char8 as BS

data SyntaxConfig = SyntaxConfig
 { names :: [String]
 , symbols :: [String]
 } deriving Show

updatedNames :: [String] -> SyntaxConfig -> SyntaxConfig
updatedNames ns con = con { names = ns ++ (names con) }


instance FromJSON SyntaxConfig where
    parseJSON (Object v) = SyntaxConfig <$>
                            liftM V.toList (v .: "names") <*>
                            liftM V.toList (v .: "symbols")
 -- A non-Object value is of the wrong type, so fail.
    parseJSON _          = mzero

getConfig :: IO SyntaxConfig
getConfig = do
    config <- liftM decode (B.readFile "syntax_config.json") 
    case  config of
        Just c -> return c
        Nothing -> return (error 
            ("Core-Haskell Error: The syntax_config.json file is corrput, please reset it."))

--main :: IO ()
--main = do
--    c <- getConfig
--    print c        