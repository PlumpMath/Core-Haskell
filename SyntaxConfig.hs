{-# LANGUAGE OverloadedStrings #-}

module SyntaxConfig (
    SyntaxConfig,
    getNames,
    getSymbols,
    enableTypeSig,
    enableFunBind,
    enablePParen,
    enablePList,
    enablePLit,
    enablePInfixApp,
    enablePWildCard,
    enableListComp,
    enableEnumFrom,
    getTypes,
    enableFrac,
    enableString,
    enableGuardedRhss,
    enableTuple,
    updatedNames,
    updatedTypes,
    getConfig
    )where
import Data.Aeson (FromJSON, parseJSON, Value(..), (.:), decode)
import Control.Applicative ((<*>), (<$>))
import Control.Monad (liftM, mzero)
import qualified Data.Vector as V (toList)
import qualified Data.ByteString.Lazy as B (readFile)

-- Data name format:
-- TypeSig(Haskell.Src.Ext) -> typeSig(SyntaxConfig) -> type_sig(json)
data SyntaxConfig = SyntaxConfig
 { names :: [String]
 , symbols :: [String]
 -- H7-Haskell.pdf
 , funBind :: Bool
 , pParen :: Bool
 , pList :: Bool
 , pLit :: Bool
 , pInfixApp :: Bool
 , pWildCard :: Bool
 , listComp :: Bool
 , enumFrom :: Bool
 , typeSig :: Bool
 , types :: [String]
-- core-to-full.pdf
 , frac :: Bool
 , string :: Bool
 , guardedRhss :: Bool
 , tuple :: Bool

 } deriving Show

updatedNames :: [String] -> SyntaxConfig -> SyntaxConfig
updatedNames ns con = con { names = ns ++ (names con) }

updatedTypes :: [String] -> SyntaxConfig -> SyntaxConfig
updatedTypes ts con = con { types = ts ++ (types con) }

getNames :: SyntaxConfig -> [String]
getSymbols :: SyntaxConfig -> [String]
enableTypeSig :: SyntaxConfig -> Bool
enableFunBind :: SyntaxConfig -> Bool
enablePParen :: SyntaxConfig -> Bool
enablePList :: SyntaxConfig -> Bool
enablePLit :: SyntaxConfig -> Bool
enablePInfixApp :: SyntaxConfig -> Bool
enablePWildCard :: SyntaxConfig -> Bool
enableListComp :: SyntaxConfig -> Bool
enableEnumFrom :: SyntaxConfig -> Bool
getTypes :: SyntaxConfig -> [String]
enableFrac :: SyntaxConfig -> Bool
enableString :: SyntaxConfig -> Bool
enableGuardedRhss :: SyntaxConfig -> Bool
enableTuple :: SyntaxConfig -> Bool

getNames con = names con
getSymbols con = symbols con
enableTypeSig con = typeSig con
enableFunBind con = funBind con
enablePParen con = pParen con
enablePList con = pList con
enablePLit con = pLit con
enablePInfixApp con = pInfixApp con
enablePWildCard con = pWildCard con
enableListComp con = listComp con
enableEnumFrom con = SyntaxConfig.enumFrom con
getTypes con = types con
enableFrac con = frac con
enableString con = SyntaxConfig.string con
enableGuardedRhss con = guardedRhss con
enableTuple con = tuple con

instance FromJSON SyntaxConfig where
    parseJSON (Object v) = 
        SyntaxConfig 
            <$> liftM V.toList (v .: "names")
            <*> liftM V.toList (v .: "symbols")
            <*> v .: "fun_bind"
            <*> v .: "p_paren"
            <*> v .: "p_list"
            <*> v .: "p_lit"
            <*> v .: "p_infix_app"
            <*> v .: "p_wild_card"
            <*> v .: "list_comp"
            <*> v .: "enum_from"
            <*> v .: "type_sig"
            <*> liftM V.toList (v .: "types")
            <*> v .: "frac"
            <*> v .: "string"
            <*> v .: "guarded_rhss"
            <*> v .: "tuple"
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