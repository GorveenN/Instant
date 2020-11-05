{-# LANGUAGE FlexibleContexts #-}

module Compiler.Jvm where
import           AbsInstant
import           Control.Monad.Except
import           Control.Monad.Identity
import           Control.Monad.State
import qualified Data.Map                      as Map

import           Compiler.Common


data Store = Store { _dict   :: Map.Map Ident Integer, _free   :: Integer } deriving (Show)

insertIfAbsent :: Ident -> S Integer
insertIfAbsent n = do
    vStore <- gets _dict
    case (Map.lookup n vStore) of
        Just v  -> return v
        Nothing -> do
            freePosition <- gets _free
            modify
                (\x -> x { _dict = Map.insert n freePosition vStore
                         , _free = freePosition + 1
                         }
                )
            return freePosition



type S = State Store
type ES a = ExceptT (Exception a) (State Store)

compile :: Show a => Program a -> String -> Either (Exception a) [String]
compile prg name = runIdentity
    (evalStateT (runExceptT (compileProgram prg name))
                (Store { _dict = Map.empty, _free = 1 })
    )


compileProgram :: Show a => Program a -> String -> ES a [String]
compileProgram (Prog _ stmts) name = do
    instr <- mapM compileStmt stmts
    let strings    = concat $ map snd instr
    let stackDepth = maximum $ 0 : map fst instr
    localsNum <- gets _free
    let program =
            prelude name
                ++ [ ".limit stack " ++ show stackDepth
                   , ".limit locals " ++ show localsNum
                   ]
                ++ strings
                ++ postlude
    return program
  where
    prelude n =
        [ ".class  public " ++ n
        , ".super  java/lang/Object"
        , ".method public <init>()V"
        , "aload_0"
        , "invokespecial java/lang/Object/<init>()V"
        , "return"
        , ".end method"
        , ".method public static main([Ljava/lang/String;)V"
        ]

    postlude = ["return", ".end method"]


compileStmt :: Show a => Stmt a -> ES a (Integer, [String])
compileStmt (SAss _ n e) = do
    (d, s) <- compileExp e
    ident  <- lift $ insertIfAbsent n
    let instr =
            s ++ [(if ident <= 3 then "istore_" else "istore ") ++ (show ident)]
    return (d, instr)
compileStmt (SExp _ e) = do
    (d, s) <- compileExp e
    let instr = if d > 1
            then s ++ [printStream, "swap", invokeVirtual]
            else printStream : s ++ [invokeVirtual]
    return ((max d 2), instr)
  where
    printStream   = "getstatic java/lang/System/out Ljava/io/PrintStream;"
    invokeVirtual = "invokevirtual java/io/PrintStream/println(I)V"


compileExp :: Show a => Exp a -> ES a (Integer, [String])
compileExp (ExpAdd _ e1 e2) = _compileExpCommutative e1 e2 ["iadd"]
compileExp (ExpSub _ e1 e2) = _compileExpNonCommutative e1 e2 ["isub"]
compileExp (ExpMul _ e1 e2) = _compileExpCommutative e1 e2 ["imul"]
compileExp (ExpDiv _ e1 e2) = _compileExpNonCommutative e1 e2 ["idiv"]
compileExp (ExpVar p n    ) = do
    variableStore <- gets _dict
    location      <- case (Map.lookup n variableStore) of
        Just v  -> return v
        Nothing -> throwError $ UnboundVariable p
    return
        (1, [(if location <= 3 then "iload_" else "iload ") ++ (show location)])
compileExp (ExpLit p e) = do
    instr <- cmd e
    return (1, [instr ++ show e])
  where
    cmd e | e <= 5        = return "iconst_"
          | e <= int8MAX  = return "bipush "
          | e <= int16MAX = return "sipush "
          | e <= int32MAX = return "ldc "
          | otherwise     = throwError $ LiteralOverflow p e

_compileExpCommutative
    :: Show a => Exp a -> Exp a -> [String] -> ES a (Integer, [String])
_compileExpCommutative e1 e2 str = do
    (d1, s1) <- compileExp e1
    (d2, s2) <- compileExp e2
    return (_stackSize d1 d2, (if d1 > d2 then s1 ++ s2 else s2 ++ s1) ++ str)

_compileExpNonCommutative
    :: Show a => Exp a -> Exp a -> [String] -> ES a (Integer, [String])
_compileExpNonCommutative e1 e2 str = do
    (d1, s1) <- compileExp e1
    (d2, s2) <- compileExp e2
    return
        ( _stackSize d1 d2
        , (if d1 >= d2 then s1 ++ s2 else s2 ++ s1 ++ ["swap"]) ++ str
        )

_stackSize :: Integer -> Integer -> Integer
_stackSize d1 d2 = max (max d1 d2) ((min d1 d2) + 1)
