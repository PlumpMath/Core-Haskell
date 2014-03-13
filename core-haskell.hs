module Main where

import Data.List
import Language.Haskell.Interpreter
import SyntaxChecker
import System.Console.Haskeline
import System.Environment (getArgs)

main :: IO ()
main = do
    fPath <- getFile

    case fPath of
        "" -> run [] fPath
        _  -> do
                m <- getModule fPath
                let ns = getNames m
                if isCoreModule ns m then run ns fPath
                else error "This is not a vaild Core Haskell Source file."

run :: [String] -> String -> IO ()
run ns fPath = runInputT defaultSettings (loop fPath) where
        loop :: String -> InputT IO ()
        loop fp = do
            minput <- getInputLine "Prelude> "
            case minput of
                Nothing -> return ()
                Just "quit" -> return ()
                Just input -> do
                    rst <- liftIO (evaluate ns fp input)
                    outputStrLn $ "Result: " ++ rst
                    loop fp

getFile :: IO String
getFile = do
    args <- getArgs
    case args of
        [] -> return ""
        [fPath] -> return
            (if ".hs" `isSuffixOf` fPath then fPath
            else error "This is not a vaild haskell source file.")
        _ -> return (error "You can't supply more than one file.")

evaluate :: [String] -> String -> String -> IO String
evaluate ns fPath expr = do
    r <- runInterpreter (evaluate' ns fPath expr)
    case r of
        Left  err -> return (show err)
        Right rst -> return rst


evaluate' :: [String] -> String -> String -> InterpreterT IO String
evaluate' ns fPath expr = do
    -- you can't end with ...hs\ or ...hs/
    let mPath = take (length fPath -3) fPath
    case mPath of
        "" -> evalExpr expr
        _  -> do
            loadFile mPath
            --liftIO (putStrLn ("Module Loaded: " ++ mPath))
            evalExpr expr
        where
            -- Only test with current directory
            loadFile mPath = do
                loadModules [mPath ++ ".hs"]
                -- todo handle custom module name, instead of harded coded Main
                setTopLevelModules ["Main"]
                --getModuleExports mPath
            evalExpr expression = 
                if isCoreExpression ns expr then do
                    setImportsQ [("Prelude", Nothing)]
                    result <- eval expression
                    return (show result)
                else error "This is not a vaild haskell statement"
