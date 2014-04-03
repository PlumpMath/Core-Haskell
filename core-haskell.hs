module Main where

import Data.List (isSuffixOf)
import Language.Haskell.Interpreter
import SyntaxChecker
import System.Console.Haskeline
import System.Environment (getArgs)
import SyntaxConfig

main :: IO ()
main = do
    fPath <- getFile
    con <- getConfig
    case fPath of
        "" -> run con fPath
        _  -> do
            m <- getModule fPath
            let con' = updatedNames (getNamesFromTop m) con
            case isCoreModule con' m of
                Right _ -> run con' fPath
                Left errs -> error (concatMap show errs)

run :: SyntaxConfig -> String -> IO ()
run con fPath = runInputT defaultSettings loop where
        loop :: InputT IO ()
        loop = do
            minput <- getInputLine "Prelude> "
            case minput of
                Nothing -> return ()
                Just "quit" -> return ()
                Just input -> do
                    rst <- liftIO (evaluate con fPath input)
                    outputStrLn $ "Result: " ++ rst
                    loop

getFile :: IO String
getFile = do
    args <- getArgs
    case args of
        [] -> return ""
        [fPath] -> return
            (if ".hs" `isSuffixOf` fPath then fPath
            else error "This is not a vaild haskell source file.")
        _ -> return (error "You can't supply more than one file.")

evaluate :: SyntaxConfig -> String -> String -> IO String
evaluate con fPath expr = do
    r <- runInterpreter evaluate'
    case r of
        Left  err -> return (show err)
        Right rst -> return rst
    where
    evaluate' :: InterpreterT IO String
    evaluate' = do
        -- you can't end with ...hs\ or ...hs/
        case fPath of
            "" -> evalExpr expr
            _  -> do
                loadModules [fPath]
                -- todo handle custom module name, instead of harded coded Main
                setTopLevelModules ["Main"]
                --liftIO (putStrLn ("Module Loaded: " ++ fPath))
                evalExpr expr
        where evalExpr expression =  
                case isCoreExpression con expr of 
                    Right _ -> do
                        setImportsQ [("Prelude", Nothing)]
                        result <- eval expression
                        return (show result)
                    Left errs -> return (concat (map show errs))