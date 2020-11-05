{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module JvmMain where

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

import qualified Compiler.Jvm                  as JVM
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
                let result = JVM.compile tree $ takeBaseName n
                case result of
                    Left exc -> do
                        hPutStrLn stderr $ show exc
                        exitFailure
                    Right instr -> do
                        let dotj = replaceExtension n "j"
                        let dir  = takeDirectory n
                        writeFile dotj $ unlines instr
                        callCommand $ format
                            "java -jar lib/jasmin.jar -d {} {} 1>/dev/null 2>/dev/null"
                            (dir, dotj)
                        exitSuccess

main :: IO ()
main = do
    args <- getArgs
    mapM_ (runFile pProgram) args
