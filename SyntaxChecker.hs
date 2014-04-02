module SyntaxChecker (isCoreModule, isCoreExpression, getModule, getNames, CoreError) where

import Language.Haskell.Exts
import Language.Haskell.Exts.SrcLoc

-- Error handling
data CoreError = NotAllowed String Int | 
                 OnlyAllowed String Int |
                 NotAllowedHeader String |
                 OnlyAllowedHeader String
instance Show CoreError where
    show (NotAllowed err line) = "Core-Haskell Error: You't can't use " ++ err ++ 
                                 " (line:" ++ show line ++ ")\n"
    show (NotAllowedHeader err) = "Core-Haskell Error: You't can't use " ++ err ++ 
                                 "\n"
    show (OnlyAllowed err line) = "Core-Haskell Error: You can only use " ++ err ++ 
                                 " (line:" ++ show line ++ ")\n"
    show (OnlyAllowedHeader err) = "Core-Haskell Error: You can only use " ++ err ++ 
                                 "\n"

appendErrors :: Either CoreError Bool -> Either [CoreError] Bool -> Either [CoreError] Bool
(Left err) `appendErrors` (Left errs) =  Left (errs ++ [err])
(Right r)  `appendErrors` (Left []) = Right r
(Right _)  `appendErrors` lerrs = lerrs
(Left err) `appendErrors` (Right _) = Left [err]

mergeError :: [Either CoreError Bool] -> Either [CoreError] Bool
mergeError = foldr appendErrors (Right True) 
mergeErrors :: (a -> Either [CoreError] Bool) -> [a] -> Either [CoreError] Bool
mergeErrors f =  foldr (\x errs-> errs `addErrors` f x) (Right True)  

addErrors :: Either [CoreError] Bool -> Either [CoreError] Bool -> Either [CoreError] Bool
(Left errs1) `addErrors` (Left errs2) =  Left (errs1 ++ errs2)
(Right r)    `addErrors` (Left []) = Right r
(Left [])    `addErrors` (Right r) = Right r
(Right _)    `addErrors` lerrs = lerrs
lerrs        `addErrors` (Right _) = lerrs

-- Disable module Name, default is Main even if you omit it
isCoreModuleName :: ModuleName -> Either CoreError Bool
isCoreModuleName (ModuleName mn)
    | mn == "Main" = Right True
    | otherwise = Left (OnlyAllowed "Main" 1)

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
isCoreDecl' _ (TypeDecl src _ _ _) = Left [NotAllowed "TypeDecl" (startLine src)]
isCoreDecl' _ (TypeFamDecl src _ _ _) = Left [NotAllowed "TypeFamDecl" (startLine src)]
isCoreDecl' _ (DataDecl src _ _ _ _ _ _) = Left [NotAllowed "DataDecl" (startLine src)]
isCoreDecl' _ (GDataDecl src _ _ _ _ _ _ _) = Left [NotAllowed "GDataDecl" (startLine src)]
isCoreDecl' _ (DataFamDecl src _ _ _ _) = Left [NotAllowed "DataFamDecl" (startLine src)]
isCoreDecl' _ (TypeInsDecl src _ _) = Left [NotAllowed "TypeInsDecl" (startLine src)]
isCoreDecl' _ (DataInsDecl src _ _ _ _) = Left [NotAllowed "DataInsDecl" (startLine src)]
isCoreDecl' _ (GDataInsDecl src _ _ _ _ _) = Left [NotAllowed "GDataInsDecl" (startLine src)]
isCoreDecl' _ (ClassDecl src _ _ _ _ _) = Left [NotAllowed "ClassDecl" (startLine src)]
isCoreDecl' _ (InstDecl src _ _ _ _) = Left [NotAllowed "InstDecl" (startLine src)]
isCoreDecl' _ (DerivDecl src _ _ _) = Left [NotAllowed "DerivDecl" (startLine src)]
isCoreDecl' _ (InfixDecl src _ _ _) = Left [NotAllowed "InfixDecl" (startLine src)]
isCoreDecl' _ (DefaultDecl src _) = Left [NotAllowed "DefaultDecl" (startLine src)]
isCoreDecl' _ (SpliceDecl src _) = Left [NotAllowed "SpliceDecl" (startLine src)]
isCoreDecl' _ (TypeSig src _ _) = Left [NotAllowed "TypeSig" (startLine src)]
-- todo?
isCoreDecl' _ (FunBind _) = Left [NotAllowedHeader "FunBind" ]
isCoreDecl' ns' (PatBind src p t r b) = mergeError 
    [isCorePat ns' src p, isCoreType src t, isCoreBinds src b] 
    `addErrors` isCoreRhs ns' src r
isCoreDecl' _ (ForImp src _ _ _ _ _) = Left [NotAllowed "ForImp" (startLine src)]
isCoreDecl' _ (ForExp src _ _ _ _) = Left [NotAllowed "ForExp" (startLine src)]
isCoreDecl' _ (RulePragmaDecl src _) = Left [NotAllowed "RulePragmaDecl" (startLine src)]
isCoreDecl' _ (DeprPragmaDecl src _) = Left [NotAllowed "DeprPragmaDecl" (startLine src)]
isCoreDecl' _ (WarnPragmaDecl src _) = Left [NotAllowed "WarnPragmaDecl" (startLine src)]
isCoreDecl' _ (InlineSig src _ _ _) = Left [NotAllowed "InlineSig" (startLine src)]
isCoreDecl' _ (InlineConlikeSig src _ _) = Left [NotAllowed "InlineConlikeSig" (startLine src)]
isCoreDecl' _ (SpecSig src _ _ _) = Left [NotAllowed "SpecSig" (startLine src)]
isCoreDecl' _ (SpecInlineSig src _ _ _ _) = Left [NotAllowed "SpecInlineSig" (startLine src)]
isCoreDecl' _ (InstSig src _ _ _) = Left [NotAllowed "InstSig" (startLine src)]
isCoreDecl' _ (AnnPragma src _) = Left [NotAllowed "AnnPragma" (startLine src)]

-- A pattern, to be matched against a value. 
isCorePat :: [String] -> SrcLoc -> Pat -> Either CoreError Bool
isCorePat ns src (PVar n) = isCoreName ns src n
isCorePat _ src (PLit _) = Left (NotAllowed "PLit" (startLine src))
isCorePat _ src (PNeg _) = Left (NotAllowed "PNeg" (startLine src))
isCorePat _ src (PNPlusK {}) = Left (NotAllowed "PNPlusK" (startLine src))
isCorePat _ src (PInfixApp {}) = Left (NotAllowed "PInfixApp" (startLine src))
isCorePat _ src (PApp {}) = Left (NotAllowed "PApp" (startLine src))
isCorePat _ src (PTuple {}) = Left (NotAllowed "PTuple" (startLine src))
isCorePat _ src (PList _) = Left (NotAllowed "PList" (startLine src))
isCorePat _ src (PParen _) = Left (NotAllowed "PParen" (startLine src))
isCorePat _ src (PRec {}) = Left (NotAllowed "PRec" (startLine src))
isCorePat _ src (PAsPat {}) = Left (NotAllowed "PAsPat" (startLine src))
isCorePat _ src (PWildCard) = Left (NotAllowed "PWildCard" (startLine src))
isCorePat _ src (PIrrPat _) = Left (NotAllowed "PIrrPat" (startLine src))
isCorePat _ src (PatTypeSig {}) = Left (NotAllowed "PatTypeSig" (startLine src))
isCorePat _ src (PViewPat {}) = Left (NotAllowed "PViewPat" (startLine src))
isCorePat _ src (PRPat _) = Left (NotAllowed "PRPat" (startLine src))
isCorePat _ src (PXTag {}) = Left (NotAllowed "PXTag" (startLine src))
isCorePat _ src (PXETag {}) = Left (NotAllowed "PXETag" (startLine src))
isCorePat _ src (PXPcdata _) = Left (NotAllowed "PXPcdata" (startLine src))
isCorePat _ src (PXPatTag _) = Left (NotAllowed "PXPatTag" (startLine src))
isCorePat _ src (PXRPats _) = Left (NotAllowed "PXRPats" (startLine src))
isCorePat _ src (PExplTypeArg {}) = Left (NotAllowed "PExplTypeArg" (startLine src))
isCorePat _ src (PQuasiQuote {}) = Left (NotAllowed "PQuasiQuote" (startLine src))
isCorePat _ src (PBangPat _) = Left (NotAllowed "PBangPat" (startLine src))

isCoreType :: SrcLoc -> Maybe Type -> Either CoreError Bool
isCoreType _ Nothing = Right True
isCoreType src (Just(TyForall {})) = Left (NotAllowed "TyForall" (startLine src))
isCoreType src (Just(TyFun {})) = Left (NotAllowed "TyFun" (startLine src))
isCoreType src (Just(TyTuple {})) = Left (NotAllowed "TyTuple" (startLine src))
isCoreType src (Just(TyList _)) = Left (NotAllowed "TyList" (startLine src))
isCoreType src (Just(TyApp {})) = Left (NotAllowed "TyApp" (startLine src))
isCoreType src (Just(TyVar _)) = Left (NotAllowed "TyVar" (startLine src))
isCoreType src (Just(TyCon _)) = Left (NotAllowed "TyCon" (startLine src))
isCoreType src (Just(TyParen _)) = Left (NotAllowed "TyParen" (startLine src))
isCoreType src (Just(TyInfix {})) = Left (NotAllowed "TyInfix" (startLine src))
isCoreType src (Just(TyKind {})) = Left (NotAllowed "TyKind" (startLine src))

isCoreRhs :: [String] -> SrcLoc -> Rhs -> Either [CoreError] Bool
isCoreRhs ns src (UnGuardedRhs expr) = isCoreExp ns src expr
isCoreRhs _ src (GuardedRhss _) = Left [NotAllowed "GuardedRhss" (startLine src)]

isCoreBinds :: SrcLoc -> Binds -> Either CoreError Bool
isCoreBinds _ (BDecls []) = Right True
isCoreBinds _ (IPBinds []) = Right True
isCoreBinds src (BDecls _) = Left (NotAllowed "BDecls" (startLine src))
isCoreBinds src (IPBinds _) = Left (NotAllowed "IPBinds" (startLine src))

isCoreExp :: [String] -> SrcLoc -> Exp -> Either [CoreError] Bool
isCoreExp ns src (Var qn) = isCoreQName ns src qn `appendErrors` Right True
isCoreExp _ src (IPVar _) = Left [NotAllowed "IPVar" (startLine src)]
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
isCoreExp _ src (Let {}) = Left [NotAllowed "Let" (startLine src)]
isCoreExp ns src (If e1 e2 e3) = mergeErrors (isCoreExp ns src) [e1, e2, e3]
isCoreExp _ src (Case {}) = Left [NotAllowed "Case" (startLine src)]
isCoreExp _ src (Do _) = Left [NotAllowed "Do" (startLine src)]
isCoreExp _ src (MDo _) = Left [NotAllowed "MDo" (startLine src)]
isCoreExp _ src (Tuple {}) = Left [NotAllowed "Tuple" (startLine src)]
isCoreExp _ src (TupleSection {}) = Left [NotAllowed "TupleSection" (startLine src)]
isCoreExp ns src (List exprs) = mergeErrors (isCoreExp ns src) exprs
isCoreExp ns src (Paren expr) = isCoreExp ns src expr
isCoreExp ns src (LeftSection expr qop) = isCoreQOp ns src qop `appendErrors`
    isCoreExp ns src expr
isCoreExp ns src (RightSection qop expr) = isCoreQOp ns src qop `appendErrors` 
    isCoreExp ns src expr
isCoreExp _ src (RecConstr {}) = Left [NotAllowed "RecConstr" (startLine src)]
isCoreExp _ src (RecUpdate {}) = Left [NotAllowed "RecUpdate" (startLine src)]
isCoreExp _ src (EnumFrom _) = Left [NotAllowed "EnumFrom" (startLine src)]
isCoreExp _ src (EnumFromTo {}) = Left [NotAllowed "EnumFromTo" (startLine src)]
isCoreExp _ src (EnumFromThen {}) = Left [NotAllowed "EnumFromThen" (startLine src)]
isCoreExp _ src (EnumFromThenTo {}) = Left [NotAllowed "EnumFromThenTo" (startLine src)]
isCoreExp _ src (ListComp {}) = Left [NotAllowed "ListComp" (startLine src)]
isCoreExp _ src (ParComp {}) = Left [NotAllowed "ParComp" (startLine src)]
isCoreExp _ src (ExpTypeSig {}) = Left [NotAllowed "ExpTypeSig" (startLine src)]
isCoreExp _ src (VarQuote _) = Left [NotAllowed "VarQuote" (startLine src)]
isCoreExp _ src (TypQuote _) = Left [NotAllowed "TypQuote" (startLine src)]
isCoreExp _ src (BracketExp _) = Left [NotAllowed "BracketExp" (startLine src)]
isCoreExp _ src (SpliceExp _) = Left [NotAllowed "SpliceExp" (startLine src)]
isCoreExp _ src (QuasiQuote {}) = Left [NotAllowed "QuasiQuote" (startLine src)]
isCoreExp _ src (XTag {}) = Left [NotAllowed "XTag" (startLine src)]
isCoreExp _ src (XETag {}) = Left [NotAllowed "XETag" (startLine src)]
isCoreExp _ src (XPcdata _) = Left [NotAllowed "XPcdata" (startLine src)]
isCoreExp _ src (XExpTag _) = Left [NotAllowed "XExpTag" (startLine src)]
isCoreExp _ src (XChildTag {}) = Left [NotAllowed "XChildTag" (startLine src)]
isCoreExp _ src (CorePragma {}) = Left [NotAllowed "CorePragma" (startLine src)]
isCoreExp _ src (SCCPragma {}) = Left [NotAllowed "SCCPragma" (startLine src)]
isCoreExp _ src (GenPragma {}) = Left [NotAllowed "GenPragma" (startLine src)]
isCoreExp _ src (Proc {}) = Left [NotAllowed "Proc" (startLine src)]
isCoreExp _ src (LeftArrApp {}) = Left [NotAllowed "LeftArrApp" (startLine src)]
isCoreExp _ src (RightArrApp {}) = Left [NotAllowed "RightArrApp" (startLine src)]
isCoreExp _ src (LeftArrHighApp {}) = Left [NotAllowed "LeftArrHighApp" (startLine src)]
isCoreExp _ src (RightArrHighApp {}) = Left [NotAllowed "RightArrHighApp" (startLine src)]

isCoreQOp :: [String] -> SrcLoc -> QOp -> Either CoreError Bool
isCoreQOp ns src (QVarOp qn) = isCoreQName ns src qn
isCoreQOp ns src (QConOp qn) = isCoreQName ns src qn

-- main function
isCoreQName :: [String] -> SrcLoc -> QName -> Either CoreError Bool
isCoreQName _ src (Qual {}) = Left (NotAllowed "Qual" (startLine src))
isCoreQName ns src (UnQual n) = isCoreName ns src n
isCoreQName _ src (Special s) = isCoreSpecialCon src s

isCoreSpecialCon :: SrcLoc -> SpecialCon -> Either CoreError Bool
isCoreSpecialCon src UnitCon = Left (NotAllowed "UnitCon" (startLine src))
isCoreSpecialCon src ListCon = Left (NotAllowed "ListCon" (startLine src))
isCoreSpecialCon src FunCon = Left (NotAllowed "FunCon" (startLine src))
isCoreSpecialCon src (TupleCon {}) = Left (NotAllowed "TupleCon" (startLine src))
isCoreSpecialCon _ Cons = Right True
isCoreSpecialCon src UnboxedSingleCon = Left (NotAllowed "UnboxedSingleCon" (startLine src))

-- literal Values of this type hold the abstract value of the literal, 
-- not the precise string representation used. For example, 10, 0o12 and 0xa have the same representation. 
isCoreLiterial :: SrcLoc -> Literal -> Either CoreError Bool
isCoreLiterial _ (Char _) = Right True
isCoreLiterial _ (String _) = Right True
isCoreLiterial _ (Int _) = Right True
isCoreLiterial src (Frac _) = Left (NotAllowed "Frac" (startLine src))
isCoreLiterial src (PrimInt _) = Left (NotAllowed "PrimInt" (startLine src))
isCoreLiterial src (PrimWord _) = Left (NotAllowed "PrimWord" (startLine src))
isCoreLiterial src (PrimFloat _) = Left (NotAllowed "PrimFloat" (startLine src))
isCoreLiterial src (PrimDouble _) = Left (NotAllowed "PrimDouble" (startLine src))
isCoreLiterial src (PrimChar _) = Left (NotAllowed "PrimChar" (startLine src))
isCoreLiterial src (PrimString _) = Left (NotAllowed "PrimString" (startLine src))

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
    then Right True else Left (NotAllowed (show i) (startLine src))
isCoreName ns src (Symbol s) = if s `elem`
    ["+", "-", "*", "&&", "||", "==", "/=", "<=", ">=", "<", ">"] ++ ns
    then Right True else Left (NotAllowed (show s) (startLine src))


-- Get name from top level definitions
getNames :: Module -> [String]
getNames (Module _ _ _ _ _ _ ds) = map getName ds
        where
        getName (PatBind _ p _ _ _) = getNameFromPat p

getNameFromPat :: Pat -> String
getNameFromPat (PVar (Ident  n)) = n
getNameFromPat (PVar (Symbol n)) = n




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
