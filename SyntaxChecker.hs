module SyntaxChecker (isCoreModule, isCoreExpression, getModule, getNames, CoreError) where

import Language.Haskell.Exts
import Language.Haskell.Exts.SrcLoc

-- Error handling
data CoreError = NotAllowed String SrcLoc | 
                 OnlyAllowed String SrcLoc |
                 NotAllowedHeader String |
                 OnlyAllowedHeader String
instance Show CoreError where
    show (NotAllowed err src) = "Core-Haskell Error: You't can't use " ++ err ++ 
                                " (line:" ++ show (startLine src) ++ ")\n"
    show (NotAllowedHeader err) = "Core-Haskell Error: You't can't use " ++ err ++ 
                                 "\n"
    show (OnlyAllowed err src) = "Core-Haskell Error: You can only use " ++ err ++ 
                                " (line:" ++ show (startLine src) ++ ")\n"
    show (OnlyAllowedHeader err) = "Core-Haskell Error: You can only use " ++ err ++ 
                                 "\n"

appendErrors :: Either CoreError Bool -> Either [CoreError] Bool -> Either [CoreError] Bool
(Left err) `appendErrors` (Left errs) =  Left (err:errs)
(Right r)  `appendErrors` (Left []) = Right r
(Right _)  `appendErrors` lerrs = lerrs
(Left err) `appendErrors` (Right _) = Left [err]

mergeError :: [Either CoreError Bool] -> Either [CoreError] Bool
mergeError = foldr appendErrors (Right True) 
mergeErrors :: (a -> Either [CoreError] Bool) -> [a] -> Either [CoreError] Bool
mergeErrors f =  foldr (\x errs-> errs `addErrors` f x) (Right True)  

addErrors :: Either [CoreError] Bool -> Either [CoreError] Bool -> Either [CoreError] Bool
(Left errs1) `addErrors` (Left errs2) =  Left (errs2 ++ errs1)
(Right r)    `addErrors` (Left []) = Right r
(Left [])    `addErrors` (Right r) = Right r
(Right _)    `addErrors` lerrs = lerrs
lerrs        `addErrors` (Right _) = lerrs

-- Disable module Name, default is Main even if you omit it
isCoreModuleName :: ModuleName -> Either CoreError Bool
isCoreModuleName (ModuleName mn)
    | mn == "Main" = Right True
    | otherwise = Left (OnlyAllowed " Module" 
        SrcLoc { srcFilename  = "Unknown", srcLine  = 1, srcColumn  =1})

-- Disable all module pragma
isCoreModulePragma :: [ModulePragma] -> Either CoreError Bool
isCoreModulePragma [] = Right True
isCoreModulePragma _ = Left (NotAllowedHeader "ModulePragma")

-- Disable all warning text
isCoreWarningText :: Maybe WarningText -> Either CoreError Bool
isCoreWarningText Nothing = Right True
isCoreWarningText _ = Left (NotAllowedHeader "WarningText")

-- Disable all export specification
isCoreExportSpec :: Maybe [ExportSpec] -> Either CoreError Bool
isCoreExportSpec Nothing = Right True
--todo?
isCoreExportSpec (Just [EVar (UnQual (Ident n))]) | n == "main" = Right True
isCoreExportSpec _ = Left (NotAllowedHeader "ExportSpec")

-- Disable all import declaration
isCoreImportDecl ::  [ImportDecl] -> Either CoreError Bool
isCoreImportDecl [] = Right True
isCoreImportDecl _ = Left (NotAllowedHeader "ImportDecl")

-- A top-level declaration
-- FunBind example: f n = n + 1
-- d: the declaration, ns: the names of top-level definition
isCoreDecl :: [String] -> [Decl] -> Either [CoreError] Bool
isCoreDecl ns = mergeErrors (isCoreDecl' ns) 

isCoreDecl' :: [String] -> Decl -> Either [CoreError] Bool
isCoreDecl' _ (TypeDecl src _ _ _) = Left [NotAllowed "TypeDecl" src]
isCoreDecl' _ (TypeFamDecl src _ _ _) = Left [NotAllowed "TypeFamDecl" src]
isCoreDecl' _ (DataDecl src _ _ _ _ _ _) = Left [NotAllowed "DataDecl" src]
isCoreDecl' _ (GDataDecl src _ _ _ _ _ _ _) = Left [NotAllowed "GDataDecl" src]
isCoreDecl' _ (DataFamDecl src _ _ _ _) = Left [NotAllowed "DataFamDecl" src]
isCoreDecl' _ (TypeInsDecl src _ _) = Left [NotAllowed "TypeInsDecl" src]
isCoreDecl' _ (DataInsDecl src _ _ _ _) = Left [NotAllowed "DataInsDecl" src]
isCoreDecl' _ (GDataInsDecl src _ _ _ _ _) = Left [NotAllowed "GDataInsDecl" src]
isCoreDecl' _ (ClassDecl src _ _ _ _ _) = Left [NotAllowed "ClassDecl" src]
isCoreDecl' _ (InstDecl src _ _ _ _) = Left [NotAllowed "InstDecl" src]
isCoreDecl' _ (DerivDecl src _ _ _) = Left [NotAllowed "DerivDecl" src]
isCoreDecl' _ (InfixDecl src _ _ _) = Left [NotAllowed "InfixDecl" src]
isCoreDecl' _ (DefaultDecl src _) = Left [NotAllowed "DefaultDecl" src]
isCoreDecl' _ (SpliceDecl src _) = Left [NotAllowed "SpliceDecl" src]
isCoreDecl' _ (TypeSig src _ _) = Left [NotAllowed "TypeSig" src]
-- todo?
isCoreDecl' _ (FunBind _) = Left [NotAllowedHeader "FunBind" ]
isCoreDecl' ns' (PatBind src p t r b) = mergeError 
    [isCorePat ns' src p, isCoreType src t, isCoreBinds src b] 
    `addErrors` isCoreRhs ns' src r
isCoreDecl' _ (ForImp src _ _ _ _ _) = Left [NotAllowed "ForImp" src]
isCoreDecl' _ (ForExp src _ _ _ _) = Left [NotAllowed "ForExp" src]
isCoreDecl' _ (RulePragmaDecl src _) = Left [NotAllowed "RulePragmaDecl" src]
isCoreDecl' _ (DeprPragmaDecl src _) = Left [NotAllowed "DeprPragmaDecl" src]
isCoreDecl' _ (WarnPragmaDecl src _) = Left [NotAllowed "WarnPragmaDecl" src]
isCoreDecl' _ (InlineSig src _ _ _) = Left [NotAllowed "InlineSig" src]
isCoreDecl' _ (InlineConlikeSig src _ _) = Left [NotAllowed "InlineConlikeSig" src]
isCoreDecl' _ (SpecSig src _ _ _) = Left [NotAllowed "SpecSig" src]
isCoreDecl' _ (SpecInlineSig src _ _ _ _) = Left [NotAllowed "SpecInlineSig" src]
isCoreDecl' _ (InstSig src _ _ _) = Left [NotAllowed "InstSig" src]
isCoreDecl' _ (AnnPragma src _) = Left [NotAllowed "AnnPragma" src]

-- A pattern, to be matched against a value. 
isCorePat :: [String] -> SrcLoc -> Pat -> Either CoreError Bool
isCorePat ns src (PVar n) = isCoreName ns src n
isCorePat _ src (PLit _) = Left (NotAllowed "PLit" src)
isCorePat _ src (PNeg _) = Left (NotAllowed "PNeg" src)
isCorePat _ src (PNPlusK {}) = Left (NotAllowed "PNPlusK" src)
isCorePat _ src (PInfixApp {}) = Left (NotAllowed "PInfixApp" src)
isCorePat _ src (PApp {}) = Left (NotAllowed "PApp" src)
isCorePat _ src (PTuple {}) = Left (NotAllowed "PTuple" src)
isCorePat _ src (PList _) = Left (NotAllowed "PList" src)
isCorePat _ src (PParen _) = Left (NotAllowed "PParen" src)
isCorePat _ src (PRec {}) = Left (NotAllowed "PRec" src)
isCorePat _ src (PAsPat {}) = Left (NotAllowed "PAsPat" src)
isCorePat _ src (PWildCard) = Left (NotAllowed "PWildCard" src)
isCorePat _ src (PIrrPat _) = Left (NotAllowed "PIrrPat" src)
isCorePat _ src (PatTypeSig {}) = Left (NotAllowed "PatTypeSig" src)
isCorePat _ src (PViewPat {}) = Left (NotAllowed "PViewPat" src)
isCorePat _ src (PRPat _) = Left (NotAllowed "PRPat" src)
isCorePat _ src (PXTag {}) = Left (NotAllowed "PXTag" src)
isCorePat _ src (PXETag {}) = Left (NotAllowed "PXETag" src)
isCorePat _ src (PXPcdata _) = Left (NotAllowed "PXPcdata" src)
isCorePat _ src (PXPatTag _) = Left (NotAllowed "PXPatTag" src)
isCorePat _ src (PXRPats _) = Left (NotAllowed "PXRPats" src)
isCorePat _ src (PExplTypeArg {}) = Left (NotAllowed "PExplTypeArg" src)
isCorePat _ src (PQuasiQuote {}) = Left (NotAllowed "PQuasiQuote" src)
isCorePat _ src (PBangPat _) = Left (NotAllowed "PBangPat" src)

isCoreType :: SrcLoc -> Maybe Type -> Either CoreError Bool
isCoreType _ Nothing = Right True
isCoreType src (Just(TyForall {})) = Left (NotAllowed "TyForall" src)
isCoreType src (Just(TyFun {})) = Left (NotAllowed "TyFun" src)
isCoreType src (Just(TyTuple {})) = Left (NotAllowed "TyTuple" src)
isCoreType src (Just(TyList _)) = Left (NotAllowed "TyList" src)
isCoreType src (Just(TyApp {})) = Left (NotAllowed "TyApp" src)
isCoreType src (Just(TyVar _)) = Left (NotAllowed "TyVar" src)
isCoreType src (Just(TyCon _)) = Left (NotAllowed "TyCon" src)
isCoreType src (Just(TyParen _)) = Left (NotAllowed "TyParen" src)
isCoreType src (Just(TyInfix {})) = Left (NotAllowed "TyInfix" src)
isCoreType src (Just(TyKind {})) = Left (NotAllowed "TyKind" src)

isCoreRhs :: [String] -> SrcLoc -> Rhs -> Either [CoreError] Bool
isCoreRhs ns src (UnGuardedRhs expr) = isCoreExp ns src expr
isCoreRhs _ src (GuardedRhss _) = Left [NotAllowed "GuardedRhss" src]

isCoreBinds :: SrcLoc -> Binds -> Either CoreError Bool
isCoreBinds _ (BDecls []) = Right True
isCoreBinds _ (IPBinds []) = Right True
isCoreBinds src (BDecls _) = Left (NotAllowed "BDecls" src)
isCoreBinds src (IPBinds _) = Left (NotAllowed "IPBinds" src)

isCoreExp :: [String] -> SrcLoc -> Exp -> Either [CoreError] Bool
isCoreExp ns src (Var qn) = isCoreQName ns src qn `appendErrors` Right True
isCoreExp _ src (IPVar _) = Left [NotAllowed "IPVar" src]
isCoreExp ns src (Con qn) = isCoreQName ns src qn `appendErrors` Right True
isCoreExp _ src (Lit l) = isCoreLiterial src l `appendErrors` Right True
isCoreExp ns src (InfixApp expr1 qop expr2) = isCoreQOp ns src qop `appendErrors` 
    isCoreExp ns src expr1 `addErrors` isCoreExp ns src expr2 
isCoreExp ns src (App expr1 expr2) = mergeErrors (isCoreExp ns src) [expr1, expr2]
isCoreExp ns src (NegApp expr) = isCoreExp ns src expr
isCoreExp ns src (Lambda _ ps expr) = 
    mergeError (map (isCorePat (ns ++ vs) src) ps) `addErrors`
    isCoreExp (ns ++ vs) src expr
    where vs = map getNameFromPat ps
isCoreExp _ src (Let {}) = Left [NotAllowed "Let" src]
isCoreExp ns src (If e1 e2 e3) = mergeErrors (isCoreExp ns src) [e1, e2, e3]
isCoreExp _ src (Case {}) = Left [NotAllowed "Case" src]
isCoreExp _ src (Do _) = Left [NotAllowed "Do" src]
isCoreExp _ src (MDo _) = Left [NotAllowed "MDo" src]
isCoreExp _ src (Tuple {}) = Left [NotAllowed "Tuple" src]
isCoreExp _ src (TupleSection {}) = Left [NotAllowed "TupleSection" src]
isCoreExp ns src (List exprs) = mergeErrors (isCoreExp ns src) exprs
isCoreExp ns src (Paren expr) = isCoreExp ns src expr
isCoreExp ns src (LeftSection expr qop) = isCoreQOp ns src qop `appendErrors`
    isCoreExp ns src expr
isCoreExp ns src (RightSection qop expr) = isCoreQOp ns src qop `appendErrors` 
    isCoreExp ns src expr
isCoreExp _ src (RecConstr {}) = Left [NotAllowed "RecConstr" src]
isCoreExp _ src (RecUpdate {}) = Left [NotAllowed "RecUpdate" src]
isCoreExp _ src (EnumFrom _) = Left [NotAllowed "EnumFrom" src]
isCoreExp _ src (EnumFromTo {}) = Left [NotAllowed "EnumFromTo" src]
isCoreExp _ src (EnumFromThen {}) = Left [NotAllowed "EnumFromThen" src]
isCoreExp _ src (EnumFromThenTo {}) = Left [NotAllowed "EnumFromThenTo" src]
isCoreExp _ src (ListComp {}) = Left [NotAllowed "ListComp" src]
isCoreExp _ src (ParComp {}) = Left [NotAllowed "ParComp" src]
isCoreExp _ src (ExpTypeSig {}) = Left [NotAllowed "ExpTypeSig" src]
isCoreExp _ src (VarQuote _) = Left [NotAllowed "VarQuote" src]
isCoreExp _ src (TypQuote _) = Left [NotAllowed "TypQuote" src]
isCoreExp _ src (BracketExp _) = Left [NotAllowed "BracketExp" src]
isCoreExp _ src (SpliceExp _) = Left [NotAllowed "SpliceExp" src]
isCoreExp _ src (QuasiQuote {}) = Left [NotAllowed "QuasiQuote" src]
isCoreExp _ src (XTag {}) = Left [NotAllowed "XTag" src]
isCoreExp _ src (XETag {}) = Left [NotAllowed "XETag" src]
isCoreExp _ src (XPcdata _) = Left [NotAllowed "XPcdata" src]
isCoreExp _ src (XExpTag _) = Left [NotAllowed "XExpTag" src]
isCoreExp _ src (XChildTag {}) = Left [NotAllowed "XChildTag" src]
isCoreExp _ src (CorePragma {}) = Left [NotAllowed "CorePragma" src]
isCoreExp _ src (SCCPragma {}) = Left [NotAllowed "SCCPragma" src]
isCoreExp _ src (GenPragma {}) = Left [NotAllowed "GenPragma" src]
isCoreExp _ src (Proc {}) = Left [NotAllowed "Proc" src]
isCoreExp _ src (LeftArrApp {}) = Left [NotAllowed "LeftArrApp" src]
isCoreExp _ src (RightArrApp {}) = Left [NotAllowed "RightArrApp" src]
isCoreExp _ src (LeftArrHighApp {}) = Left [NotAllowed "LeftArrHighApp" src]
isCoreExp _ src (RightArrHighApp {}) = Left [NotAllowed "RightArrHighApp" src]

isCoreQOp :: [String] -> SrcLoc -> QOp -> Either CoreError Bool
isCoreQOp ns src (QVarOp qn) = isCoreQName ns src qn
isCoreQOp ns src (QConOp qn) = isCoreQName ns src qn

-- main function
isCoreQName :: [String] -> SrcLoc -> QName -> Either CoreError Bool
isCoreQName _ src (Qual {}) = Left (NotAllowed "Qual" src)
isCoreQName ns src (UnQual n) = isCoreName ns src n
isCoreQName _ src (Special s) = isCoreSpecialCon src s

isCoreSpecialCon :: SrcLoc -> SpecialCon -> Either CoreError Bool
isCoreSpecialCon src UnitCon = Left (NotAllowed "UnitCon" src)
isCoreSpecialCon src ListCon = Left (NotAllowed "ListCon" src)
isCoreSpecialCon src FunCon = Left (NotAllowed "FunCon" src)
isCoreSpecialCon src (TupleCon {}) = Left (NotAllowed "TupleCon" src)
isCoreSpecialCon _ Cons = Right True
isCoreSpecialCon src UnboxedSingleCon = Left (NotAllowed "UnboxedSingleCon" src)

-- literal Values of this type hold the abstract value of the literal, 
-- not the precise string representation used. For example, 10, 0o12 and 0xa have the same representation. 
isCoreLiterial :: SrcLoc -> Literal -> Either CoreError Bool
isCoreLiterial _ (Char _) = Right True
isCoreLiterial _ (String _) = Right True
isCoreLiterial _ (Int _) = Right True
isCoreLiterial src (Frac _) = Left (NotAllowed "Frac" src)
isCoreLiterial src (PrimInt _) = Left (NotAllowed "PrimInt" src)
isCoreLiterial src (PrimWord _) = Left (NotAllowed "PrimWord" src)
isCoreLiterial src (PrimFloat _) = Left (NotAllowed "PrimFloat" src)
isCoreLiterial src (PrimDouble _) = Left (NotAllowed "PrimDouble" src)
isCoreLiterial src (PrimChar _) = Left (NotAllowed "PrimChar" src)
isCoreLiterial src (PrimString _) = Left (NotAllowed "PrimString" src)

isCoreName :: [String] -> SrcLoc -> Name -> Either CoreError Bool
isCoreName ns src (Ident i) = if i `elem` ns 
    ++ ["div", "mod", "not", "head", "tail", "False", "True"] 
    -- H1-simple.pdf
    ++ ["null", "length", "elem"]
    -- H2-map_filter.pdf
    ++ ["map"]
    -- H3-foldr.pdf
    ++ ["foldr", "foldl", "and", "all", "any", "elem", "sum", "filter"]
    -- H4-ListUtils
    ++ ["drop", "take", "dropWhile", "zip", "zipWith"]
    -- H6-Accumulators
    ++ ["max", "min"]
    then Right True else Left (NotAllowed (show i) src)
isCoreName ns src (Symbol s) = if s `elem`
    ["+", "-", "*", "&&", "||", "==", "/=", "<=", ">=", "<", ">"] ++ ns
    then Right True else Left (NotAllowed (show s) src)


-- Get name from top level definitions
getNames :: Module -> [String]
getNames (Module _ _ _ _ _ _ ds) = map getName ds
        where
        getName (PatBind _ p _ _ _) = getNameFromPat p
        getName _ = []

getNameFromPat :: Pat -> String
getNameFromPat (PVar (Ident  n)) = n
getNameFromPat (PVar (Symbol n)) = n
getNameFromPat _ = ""




-- Check the whole module/file
isCoreModule :: [String] -> Module -> Either [CoreError] Bool
isCoreModule ns (Module _ mn mp wt es im ds) = mergeError 
    [isCoreModuleName  mn, isCoreModulePragma mp,
    isCoreWarningText wt, isCoreExportSpec   es,
    isCoreImportDecl  im] 
    `addErrors` isCoreDecl  ns ds

-- Check single expression whithin interpreter
-- Can't do any definiton by default: let where a=x <- etc
isCoreExpression :: [String] -> String -> Either [CoreError] Bool
isCoreExpression ns expr = isCoreExp ns 
    SrcLoc { srcFilename  = "Unknown", srcLine  = 1, srcColumn  =1}
    (fromParseResult (parseExp expr))


getModule :: String -> IO Module
getModule mPath = do
    src <- readFile mPath
    --src <- readFile "./hello_world.hs"
    return . fromParseResult $ parseModule src

 --below just used for test the parse result of haskell-src-ext
--printAlldecl :: Module -> IO()
--printAlldecl (Module _ _ _ _ _ _ ds) = mapM_  print ds

--main :: IO ()
--main = do
--  m <- getModule "Hello.hs"
--  print m
--  printAlldecl m
