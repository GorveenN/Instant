{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module LlvmMain where

import           System.Environment             ( getArgs )
import           System.Exit                    ( exitFailure
                                                , exitSuccess
                                                )
import           System.IO                      ( hPutStrLn
                                                , stderr
                                                )
import           System.FilePath
import           System.Process

import           ParInstant
import           ErrM

import qualified Compiler.Llvm                 as LLVM
import           Compiler.Common                ( format )


runFile p f = readFile f >>= run p f

run p n s =
    let ts = myLexer s
    in
        case p ts of
            Bad s -> do
                hPutStrLn stderr $ "Parse Failed\n" ++ s
                exitFailure
            Ok tree -> do
                let result = LLVM.compile tree
                case result of
                    Left exc -> do
                        hPutStrLn stderr $ show exc
                        exitFailure
                    Right instr -> do
                        let dotbc = replaceExtension n "bc"
                        let dotll = replaceExtension n "ll"
                        writeFile dotll $ unlines instr
                        callCommand $ format
                            "llvm-as -o {} {} 1>/dev/null 2>/dev/null"
                            (dotbc, dotll)
                        exitSuccess

main :: IO ()
main = do
    args <- getArgs
    mapM_ (runFile pProgram) args
