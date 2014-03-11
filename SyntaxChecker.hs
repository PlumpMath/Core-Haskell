module SyntaxChecker (isCoreModule, getModule) where

import Language.Haskell.Exts


-- Disable module Name, default is Main even if you omit it
isCoreModuleName :: ModuleName -> Bool
isCoreModuleName (ModuleName mn) 
	| mn == "Main" = True
	| otherwise = False

-- Disable all module pragma
isCoreModulePragma :: [ModulePragma] -> Bool
isCoreModulePragma [] = True
isCoreModulePragma _ = error "You can't use module pragma." 


-- Disable all warning text
isCoreWarningText :: Maybe WarningText -> Bool
isCoreWarningText Nothing = True
isCoreWarningText _ = error "You can't use warning text."

-- Disable all export specification
isCoreExportSpec :: Maybe [ExportSpec] -> Bool
isCoreExportSpec Nothing = True
isCoreExportSpec (Just [EVar (UnQual (Ident n))]) | n == "main" = True
isCoreExportSpec _ = error "You can't use export specification."

-- Disable all import declaration
isCoreImportDecl ::  [ImportDecl] -> Bool
isCoreImportDecl [] = True
isCoreImportDecl _ = error "You can't use import declaration."

-- Check top-level declaration
-- todo FunBind 
-- todo match a list of data type using pattern matching
-- d: the declaration, ns: the name defined in current file
isCoreDecl :: [Decl] -> Bool
isCoreDecl ds = all (isCoreDecl' names) ds
	  where 
	  	names = getNames ds
	  	isCoreDecl' ns (PatBind _ p t r b) = 
	  		isCorePat ns p && isCoreType  t &&
	  		isCoreRhs ns r && isCoreBinds b
	  	isCoreDecl' _ _ = error "You can only use pattern binding." 



isCorePat :: [String] -> Pat -> Bool
isCorePat ns (PVar n) = isCoreName ns n
isCorePat _  _ = error "You can't use pattern matching."

isCoreType :: Maybe Type -> Bool
isCoreType Nothing = True
isCoreType _ = error "You can't use type signature in pattern matching."

isCoreRhs :: [String] -> Rhs -> Bool
isCoreRhs ns (UnGuardedRhs e)= isCoreExp ns e
isCoreRhs _ (GuardedRhss _) = error "You can't use guard."


isCoreExp :: [String] -> Exp -> Bool
isCoreExp ns (Var qn) = isCoreQName ns qn
isCoreExp ns (Con qn) = isCoreQName ns qn
isCoreExp _ (Lit l) = isCoreLiterial l
isCoreExp ns (InfixApp e1 qop e2) = isCoreExp ns e1 && isCoreExp ns e2 && isCoreQOp qop
isCoreExp ns (App e1 e2) = isCoreExp ns e1 && isCoreExp ns e2
isCoreExp ns (NegApp e) = isCoreExp ns e
isCoreExp ns (Lambda _ ps e) = all (isCorePat ns) ps && isCoreExp ns e
isCoreExp ns (List es) = all (isCoreExp ns) es
isCoreExp ns (Paren e) = isCoreExp ns e
isCoreExp ns (If e1 e2 e3) = isCoreExp ns e1 && isCoreExp ns e2 && isCoreExp ns e3 
isCoreExp _ _ = error (
	"You can only use lambda expression," ++
	"parenthesis, list, if and operator defined in core-haskell." )

isCoreQOp :: QOp -> Bool
isCoreQOp (QVarOp qn) = isCoreQName [] qn
isCoreQOp (QConOp qn) = isCoreQName [] qn

isCoreBinds :: Binds -> Bool
isCoreBinds (BDecls []) = True
isCoreBinds (IPBinds []) = True
isCoreBinds _ = error "You can't use let or where clause" 



{-
core function
-}

isCoreQName :: [String] -> QName -> Bool
isCoreQName _ (Qual _ _) = error "You can't import module"
isCoreQName qns (UnQual n) = isCoreName qns n
isCoreQName _ (Special s) = isCoreSpecialCon s

isCoreName :: [String] -> Name -> Bool
isCoreName ns (Ident s) = s `elem` ["div", "mod", "not", "head", "tail", "False", "True"] ++ ns
	||  error ("You can't use " ++ show s)
isCoreName ns (Symbol s) = s `elem` 
	["+", "-", "*", "&&", "||", "==", "/=", "<=", ">=", "<", ">"] ++ ns
		||  error ("You can't use " ++ show s)

isCoreSpecialCon :: SpecialCon -> Bool
isCoreSpecialCon (Cons) = True
isCoreSpecialCon _ = error "You can't use () [] -> , #,# # #"

isCoreLiterial :: Literal -> Bool
isCoreLiterial (Char _) = True
isCoreLiterial (String _) = True
isCoreLiterial (Int _) = True
isCoreLiterial _ = error "You can only use Char String Int"

-- Get name of definition recursively, including symbol and identifier
getNames :: [Decl] -> [String]
getNames = concatMap getName
	where
		getName (PatBind _ p _ r _) = getNameFromPat p : getNameFromRhs r
			where 
				getNameFromPat (PVar (Ident  n)) = n
				getNameFromPat (PVar (Symbol n)) = n
				getNameFromRhs (UnGuardedRhs e) = getNameFromExp e
					where
						getNameFromExp (InfixApp e1 _ e2) = getNameFromExp e1 ++ getNameFromExp e2
						getNameFromExp (App e1 e2) = getNameFromExp e1 ++ getNameFromExp e2
						getNameFromExp (NegApp e1) = getNameFromExp e1
						getNameFromExp (Lambda _ ps e1) = getNameFromExp e1 ++ map getNameFromPat ps
						getNameFromExp (List es) = concatMap getNameFromExp es
						getNameFromExp (Paren e1) = getNameFromExp e1
						getNameFromExp (If e1 e2 e3) = getNameFromExp e1 ++ getNameFromExp e2 ++ getNameFromExp e3
						getNameFromExp _ = []

isCoreModule :: Module -> Bool
isCoreModule (Module _ mn mp wt es im d) = 
	isCoreModuleName  mn && isCoreModulePragma mp &&
	isCoreWarningText wt && isCoreExportSpec   es &&
	isCoreImportDecl  im && isCoreDecl         d


getModule :: String -> IO Module
getModule mPath = do
    src <- readFile mPath
    --src <- readFile "./hello_world.hs"
    return . fromParseResult $ parseModule src
--decl :: Module -> [String]
--decl (Module _ _ _ _ _ _ ds) = map show ds
--main :: IO ()
--main = do

--	m <- getModule
--	--print m
--	mapM_ putStrLn (decl m)		
--	print (isCoreModule m)

	
