module SyntaxChecker (
    isCoreModule, 
    isCoreExpression, 
    getModule, 
    getNamesFromTop, 
    CoreError) where

import SyntaxConfig
import Language.Haskell.Exts
import Language.Haskell.Exts.SrcLoc (startLine)


-- Error handling
data CoreError = NotAllowed String SrcLoc | 
                 OnlyAllowed String SrcLoc |
                 NotAllowedHeader String |
                 OnlyAllowedHeader String

instance Show CoreError where
    show (NotAllowed err src) = "\nCore-Haskell Error: You't can't use " ++ err
        ++ " (line:" ++ show (startLine src) ++ ")"
    show (NotAllowedHeader err) = "\nCore-Haskell Error: You't can't use " ++ err
    show (OnlyAllowed err src) = "\nCore-Haskell Error: You can only use " ++ err
        ++ " (line:" ++ show (startLine src) ++ ")"
    show (OnlyAllowedHeader err) = "\nCore-Haskell Error: You can only use " ++ err

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

singletonErrors :: Either CoreError Bool -> Either [CoreError] Bool
singletonErrors (Right r) = Right r
singletonErrors (Left err) = Left [err]
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
isCoreDecl :: SyntaxConfig -> [Decl] -> Either [CoreError] Bool
isCoreDecl con = mergeErrors isCoreDecl' where
    isCoreDecl' :: Decl -> Either [CoreError] Bool
    isCoreDecl' (TypeDecl src _ _ _) = Left [NotAllowed "TypeDecl" src]
    isCoreDecl' (TypeFamDecl src _ _ _) = Left [NotAllowed "TypeFamDecl" src]
    isCoreDecl' (DataDecl src _ _ _ _ _ _) = Left [NotAllowed "DataDecl" src]
    isCoreDecl' (GDataDecl src _ _ _ _ _ _ _) = Left [NotAllowed "GDataDecl" src]
    isCoreDecl' (DataFamDecl src _ _ _ _) = Left [NotAllowed "DataFamDecl" src]
    isCoreDecl' (TypeInsDecl src _ _) = Left [NotAllowed "TypeInsDecl" src]
    isCoreDecl' (DataInsDecl src _ _ _ _) = Left [NotAllowed "DataInsDecl" src]
    isCoreDecl' (GDataInsDecl src _ _ _ _ _) = Left [NotAllowed "GDataInsDecl" src]
    isCoreDecl' (ClassDecl src _ _ _ _ _) = Left [NotAllowed "ClassDecl" src]
    isCoreDecl' (InstDecl src _ _ _ _) = Left [NotAllowed "InstDecl" src]
    isCoreDecl' (DerivDecl src _ _ _) = Left [NotAllowed "DerivDecl" src]
    isCoreDecl' (InfixDecl src _ _ _) = Left [NotAllowed "InfixDecl" src]
    isCoreDecl' (DefaultDecl src _) = Left [NotAllowed "DefaultDecl" src]
    isCoreDecl' (SpliceDecl src _) = Left [NotAllowed "SpliceDecl" src]
    isCoreDecl' (TypeSig src ns t) = if enableTypeSig con
        then (isCoreType con' src t)
        `addErrors` (mergeError(map (isCoreName con src) ns))
        else Left [NotAllowed "TypeSig" src]
        where con' = updatedTypes (getTypesFromType t) con
    isCoreDecl' (FunBind ms) = mergeErrors (isCoreMatch con) ms
    isCoreDecl' (PatBind src p t r b) = appendErrors 
        (isCoreBinds con src b)
        (isCoreTypeResult src t
        `addErrors` isCorePat con src p
        `addErrors` isCoreRhs con src r)
    isCoreDecl' (ForImp src _ _ _ _ _) = Left [NotAllowed "ForImp" src]
    isCoreDecl' (ForExp src _ _ _ _) = Left [NotAllowed "ForExp" src]
    isCoreDecl' (RulePragmaDecl src _) = Left [NotAllowed "RulePragmaDecl" src]
    isCoreDecl' (DeprPragmaDecl src _) = Left [NotAllowed "DeprPragmaDecl" src]
    isCoreDecl' (WarnPragmaDecl src _) = Left [NotAllowed "WarnPragmaDecl" src]
    isCoreDecl' (InlineSig src _ _ _) = Left [NotAllowed "InlineSig" src]
    isCoreDecl' (InlineConlikeSig src _ _) = Left [NotAllowed "InlineConlikeSig" src]
    isCoreDecl' (SpecSig src _ _ _) = Left [NotAllowed "SpecSig" src]
    isCoreDecl' (SpecInlineSig src _ _ _ _) = Left [NotAllowed "SpecInlineSig" src]
    isCoreDecl' (InstSig src _ _ _) = Left [NotAllowed "InstSig" src]
    isCoreDecl' (AnnPragma src _) = Left [NotAllowed "AnnPragma" src]
    isCoreTypeResult src mt = case mt of
        Nothing -> Right True
        Just jt  -> isCoreType con src jt

-- Clauses of a function binding. 
isCoreMatch :: SyntaxConfig -> Match -> Either [CoreError] Bool
isCoreMatch con (Match src _ ps t r b) = if (enableFunBind con) 
    then appendErrors (isCoreBinds con src b)
        (isCoreTypeResult t
        -- update names with local variable
        `addErrors` mergeErrors (isCorePat con' src) ps
        `addErrors` isCoreRhs con' src r)
    -- If you want to enable FunBind, you must enable Match, 
    -- so throuw error here is enough
    else Left [NotAllowed "FunBind" src]
    where con' = updatedNames (concatMap getNameFromPat ps) con
          isCoreTypeResult mt = case mt of
            Nothing -> Right True
            Just jt  -> isCoreType con src jt

-- A pattern, to be matched against a value. 
isCorePat :: SyntaxConfig -> SrcLoc -> Pat -> Either [CoreError] Bool
isCorePat con src (PVar n) = singletonErrors (isCoreName con src n)
isCorePat con src (PLit l) = if enablePLit con 
    then singletonErrors (isCoreLiterial con src l)
    else Left [(NotAllowed "PLit" src)]
isCorePat _ src (PNeg _) = Left [(NotAllowed "PNeg" src)]
isCorePat _ src (PNPlusK {}) = Left [(NotAllowed "PNPlusK" src)]
isCorePat con src (PInfixApp p1 qn p2) = if enablePInfixApp con
    then appendErrors (isCoreQName con src qn)  
        (isCorePat con src p1 `addErrors` isCorePat con src p2)
    else Left [(NotAllowed "PInfixApp" src)]
isCorePat _ src (PApp {}) = Left [(NotAllowed "PApp" src)]
isCorePat _ src (PTuple {}) = Left [(NotAllowed "PTuple" src)]
isCorePat con src (PList ps) = if enablePLit con 
    then mergeErrors (isCorePat con src) ps
    else Left [(NotAllowed "PList" src)]
isCorePat con src (PParen p) = if enablePParen con 
    then isCorePat con src p 
    else Left [(NotAllowed "PParen" src)] 
isCorePat _ src (PRec {}) = Left [(NotAllowed "PRec" src)]
isCorePat _ src (PAsPat {}) = Left [(NotAllowed "PAsPat" src)]
isCorePat con src (PWildCard) = if enablePWildCard con
    then Right True
    else Left [(NotAllowed "PWildCard" src)]
isCorePat _ src (PIrrPat _) = Left [(NotAllowed "PIrrPat" src)]
isCorePat _ src (PatTypeSig {}) = Left [(NotAllowed "PatTypeSig" src)]
isCorePat _ src (PViewPat {}) = Left [(NotAllowed "PViewPat" src)]
isCorePat _ src (PRPat _) = Left [(NotAllowed "PRPat" src)]
isCorePat _ src (PXTag {}) = Left [(NotAllowed "PXTag" src)]
isCorePat _ src (PXETag {}) = Left [(NotAllowed "PXETag" src)]
isCorePat _ src (PXPcdata _) = Left [(NotAllowed "PXPcdata" src)]
isCorePat _ src (PXPatTag _) = Left [(NotAllowed "PXPatTag" src)]
isCorePat _ src (PXRPats _) = Left [(NotAllowed "PXRPats" src)]
isCorePat _ src (PExplTypeArg {}) = Left [(NotAllowed "PExplTypeArg" src)]
isCorePat _ src (PQuasiQuote {}) = Left [(NotAllowed "PQuasiQuote" src)]
isCorePat _ src (PBangPat _) = Left [(NotAllowed "PBangPat" src)]

isCoreType :: SyntaxConfig -> SrcLoc -> Type -> Either [CoreError] Bool
--isCoreType _ _ Nothing = Right True
isCoreType con src (TyForall Nothing as t ) = 
    (mergeErrors (isCoreAsst con src) as)
    `addErrors` isCoreType con src t
isCoreType con src (TyFun t1 t2) = 
    isCoreType con' src t1 `addErrors` isCoreType con' src t2
    where con' = updatedTypes (getTypesFromType t1) con
-- ignore the difference between boxed or unboxed tuple
isCoreType con src (TyTuple _ ts) = (mergeErrors (isCoreType con src) ts)
isCoreType con src (TyList t) = isCoreType con src t
isCoreType con src (TyApp {}) = Left [(NotAllowed "TyApp" src)]
-- add types to names list, because Language.Haskell.Exts
-- use Name to constuct Type.
isCoreType con src (TyVar n) = singletonErrors (isCoreName con' src n) 
    where con' = updatedNames (getTypes con) con
isCoreType con src (TyCon qn) = singletonErrors (isCoreQName con' src qn) 
    where con' = updatedNames (getTypes con) con
isCoreType con src (TyParen _) = Left [(NotAllowed "TyParen" src)]
isCoreType con src (TyInfix {}) = Left [(NotAllowed "TyInfix" src)]
isCoreType con src (TyKind {}) = Left [(NotAllowed "TyKind" src)]

isCoreAsst :: SyntaxConfig -> SrcLoc -> Asst -> Either [CoreError] Bool
isCoreAsst con src (ClassA qn ts) = appendErrors (isCoreQName con' src qn)
    (mergeErrors (isCoreType con src) ts)
    where con' = updatedNames (getTypes con) con
isCoreAsst con src (InfixA {}) = Left [NotAllowed "InfixA" src]
isCoreAsst con src (IParam {}) = Left [NotAllowed "IParam" src] 
isCoreAsst con src (EqualP {}) = Left [NotAllowed "EqualP" src]

isCoreRhs :: SyntaxConfig -> SrcLoc -> Rhs -> Either [CoreError] Bool
isCoreRhs con src (UnGuardedRhs expr) = isCoreExp con src expr
isCoreRhs con src (GuardedRhss rhs) = if enableGuardedRhss con
    then mergeErrors (isCoreGuardedRhs src) rhs
    else Left [NotAllowed "GuardedRhss" src]
    where isCoreGuardedRhs _ (GuardedRhs src' ss e) =
            (mergeErrors (isCoreStmt con src') ss)
            `addErrors` isCoreExp con src' e

isCoreBinds :: SyntaxConfig -> SrcLoc -> Binds -> Either CoreError Bool
isCoreBinds _ _ (BDecls []) = Right True
isCoreBinds _ _ (IPBinds []) = Right True
isCoreBinds _ src (BDecls _) = Left (NotAllowed "BDecls" src)
isCoreBinds _ src (IPBinds _) = Left (NotAllowed "IPBinds" src)

-- Haskell expressions. 
isCoreExp :: SyntaxConfig -> SrcLoc -> Exp -> Either [CoreError] Bool
isCoreExp con src (Var qn) = singletonErrors (isCoreQName con src qn)
isCoreExp _ src (IPVar _) = Left [NotAllowed "IPVar" src]
isCoreExp con src (Con qn) = singletonErrors (isCoreQName con src qn)
isCoreExp con src (Lit l) = singletonErrors (isCoreLiterial con src l)
isCoreExp con src (InfixApp expr1 qop expr2) = isCoreQOp con src qop `appendErrors` 
    isCoreExp con src expr1 `addErrors` isCoreExp con src expr2 
isCoreExp con src (App expr1 expr2) = mergeErrors (isCoreExp con src) [expr1, expr2]
isCoreExp con src (NegApp expr) = isCoreExp con src expr
isCoreExp con src (Lambda _ ps expr) = 
    -- update names with local variable 
    mergeErrors (isCorePat con' src) ps `addErrors`
    isCoreExp con' src expr
    where con' = updatedNames (concatMap getNameFromPat ps) con
isCoreExp _ src (Let {}) = Left [NotAllowed "Let" src]
isCoreExp con src (If e1 e2 e3) = mergeErrors (isCoreExp con src) [e1, e2, e3]
isCoreExp _ src (Case {}) = Left [NotAllowed "Case" src]
isCoreExp _ src (Do _) = Left [NotAllowed "Do" src]
isCoreExp _ src (MDo _) = Left [NotAllowed "MDo" src]
isCoreExp con src (Tuple _ es) = if enableTuple con
    then mergeErrors (isCoreExp con src) es
    else Left [NotAllowed "Tuple" src]
isCoreExp _ src (TupleSection {}) = Left [NotAllowed "TupleSection" src]
isCoreExp con src (List exprs) = mergeErrors (isCoreExp con src) exprs
isCoreExp con src (Paren expr) = isCoreExp con src expr
isCoreExp con src (LeftSection expr qop) = isCoreQOp con src qop `appendErrors`
    isCoreExp con src expr
isCoreExp con src (RightSection qop expr) = isCoreQOp con src qop `appendErrors` 
    isCoreExp con src expr
isCoreExp _ src (RecConstr {}) = Left [NotAllowed "RecConstr" src]
isCoreExp _ src (RecUpdate {}) = Left [NotAllowed "RecUpdate" src]
isCoreExp con src (EnumFrom e) = if enableEnumFrom con 
    then isCoreExp con src e
    else Left [NotAllowed "EnumFrom" src]
isCoreExp con src (EnumFromTo e1 e2) = if enableEnumFrom con 
    then isCoreExp con src e1 `addErrors` isCoreExp con src e2
    else Left [NotAllowed "EnumFromTo" src]
isCoreExp con src (EnumFromThen e1 e2) = if enableEnumFrom con
    then isCoreExp con src e1 `addErrors` isCoreExp con src e2
    else Left [NotAllowed "EnumFromThen" src]
isCoreExp con src (EnumFromThenTo e1 e2 e3) = if enableEnumFrom con 
    then isCoreExp con src e1 
    `addErrors` isCoreExp con src e2
    `addErrors` isCoreExp con src e3
    else Left [NotAllowed "EnumFromThenTo" src]
isCoreExp con src (ListComp e qss) = if enableListComp con 
    then isCoreExp con' src e
        `addErrors` mergeErrors (isCoreQualStmt con' src) qss
    else Left [NotAllowed "ListComp" src]
    where con' = updatedNames (concatMap getNameFromQualStmt qss) con
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

--A general transqual in a list comprehension, 
--which could potentially be a transform of the kind enabled by TransformListComp. 
isCoreQualStmt :: SyntaxConfig -> SrcLoc -> QualStmt -> Either [CoreError] Bool
isCoreQualStmt con src (QualStmt s) = isCoreStmt con src s
isCoreQualStmt _ src (ThenTrans _) = Left [NotAllowed "ThenTrans" src]
isCoreQualStmt _ src (ThenBy {}) = Left [NotAllowed "ThenBy" src]
isCoreQualStmt _ src (GroupBy _) = Left [NotAllowed "GroupBy" src]
isCoreQualStmt _ src (GroupUsing _) = Left [NotAllowed "GroupUsing" src]
isCoreQualStmt _ src (GroupByUsing {}) = Left [NotAllowed "GroupByUsing" src]
--isCoreQualStmt
--A statement, representing both a stmt in a do-expression, 
--an ordinary qual in a list comprehension, as well as a stmt in a pattern guard. 
isCoreStmt :: SyntaxConfig -> SrcLoc -> Stmt -> Either [CoreError] Bool
isCoreStmt con _ (Generator src p e) = isCorePat con src p 
    `addErrors` isCoreExp con src e
isCoreStmt con src (Qualifier e) = isCoreExp con src e
isCoreStmt con src (LetStmt ls) = singletonErrors (isCoreBinds con src ls)
isCoreStmt con src (RecStmt rs) = mergeErrors (isCoreStmt con src) rs

isCoreQOp :: SyntaxConfig -> SrcLoc -> QOp -> Either CoreError Bool
isCoreQOp con src (QVarOp qn) = isCoreQName con src qn
isCoreQOp con src (QConOp qn) = isCoreQName con src qn

-- main function
isCoreQName :: SyntaxConfig -> SrcLoc -> QName -> Either CoreError Bool
isCoreQName _ src (Qual {}) = Left (NotAllowed "Qual" src)
isCoreQName con src (UnQual n) = isCoreName con src n
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
isCoreLiterial :: SyntaxConfig -> SrcLoc -> Literal -> Either CoreError Bool
isCoreLiterial con src (Char _) = if enableString con
    then Right True
    else Left (NotAllowed "Char" src)
isCoreLiterial con src (String _) = if enableString con
    then Right True
    else Left (NotAllowed "String" src)
isCoreLiterial _ _ (Int _) = Right True
isCoreLiterial con src (Frac _) = if enableFrac con
    then Right True
    else Left (NotAllowed "Frac" src)
isCoreLiterial _ src (PrimInt _) = Left (NotAllowed "PrimInt" src)
isCoreLiterial _ src (PrimWord _) = Left (NotAllowed "PrimWord" src)
isCoreLiterial _ src (PrimFloat _) = Left (NotAllowed "PrimFloat" src)
isCoreLiterial _ src (PrimDouble _) = Left (NotAllowed "PrimDouble" src)
isCoreLiterial _ src (PrimChar _) = Left (NotAllowed "PrimChar" src)
isCoreLiterial _ src (PrimString _) = Left (NotAllowed "PrimString" src)

isCoreName :: SyntaxConfig -> SrcLoc -> Name -> Either CoreError Bool
isCoreName con src (Ident i) = if i `elem` (getNames con) 
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
isCoreName con src (Symbol s) = if s `elem` (getSymbols con) ++
    ["+", "-", "*", "&&", "||", "==", "/=", "<=", ">=", "<", ">"]
    then Right True else Left (NotAllowed (show s) src)


-- Get name from top level definitions
-- todo check again
getNamesFromTop :: Module -> [String]
getNamesFromTop (Module _ _ _ _ _ _ ds) = 
    concatMap getNameFromTop ds where
        getNameFromTop (PatBind _ p _ _ _) = getNameFromPat p
        getNameFromTop (FunBind ms) = 
            map (\(Match _ n _ _ _ _)-> getNameFromName n) ms
        getNameFromTop _ = []

getNameFromPat :: Pat -> [String]
getNameFromPat (PVar n) = [getNameFromName n]
getNameFromPat (PList ps) = concatMap getNameFromPat ps
getNameFromPat (PParen p) = getNameFromPat p
getNameFromPat (PInfixApp p1 _ p2) = getNameFromPat p1 ++ getNameFromPat p2
getNameFromPat _ = []

getNameFromQualStmt :: QualStmt -> [String]
getNameFromQualStmt (QualStmt(Generator _ p _)) = getNameFromPat p
getNameFromQualStmt _ = []

getNameFromName :: Name -> String
getNameFromName (Ident n) = n
getNameFromName (Symbol n) = n

getTypesFromType :: Type -> [String]
getTypesFromType (TyForall _ as _) = concatMap getTypesFromAsset as where
    getTypesFromAsset (ClassA _ ts) = concatMap getTypesFromType ts
getTypesFromType (TyTuple _ ts) = concatMap getTypesFromType ts 
getTypesFromType (TyList t) = getTypesFromType t
getTypesFromType (TyCon(UnQual n)) = [getNameFromName n]
getTypesFromType (TyVar n) = [getNameFromName n]
getTypesFromType _ = []
-- Check the whole module/file
isCoreModule :: SyntaxConfig -> Module -> Either [CoreError] Bool
isCoreModule con (Module _ mn mp wt es im ds) = mergeError 
    [isCoreModuleName  mn, isCoreModulePragma mp,
    isCoreWarningText wt, isCoreExportSpec   es,
    isCoreImportDecl  im] 
    `addErrors` isCoreDecl  con ds

-- Check single expression whithin interpreter
-- Can't do any definiton by default: let where a=x <- etc
isCoreExpression :: SyntaxConfig -> String -> Either [CoreError] Bool
isCoreExpression con expr = isCoreExp con 
    SrcLoc { srcFilename  = "Unknown", srcLine  = 1, srcColumn  =1}
    (fromParseResult (parseExp expr))


getModule :: String -> IO Module
getModule mPath = do
    src <- readFile mPath
    --src <- readFile "./hello_world.hs"
    return . fromParseResult $ parseModule src

 --below just used for test the parse result of haskell-src-ext
printAlldecl :: Module -> IO()
printAlldecl (Module _ _ _ _ _ _ ds) = mapM_  print ds

main :: IO () 
main = do
  m <- getModule "Hello.hs"
  printAlldecl m