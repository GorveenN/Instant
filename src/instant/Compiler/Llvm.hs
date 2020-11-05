{-# LANGUAGE OverloadedStrings #-}
module Compiler.Llvm where
import           Control.Monad.Except
import           Control.Monad.Identity
import           Control.Monad.State
import qualified Data.Set                      as Set

import           AbsInstant
import           Compiler.Common

type S = State Store
type ES a  r = ExceptT (Exception a) (State Store) r
type Instructions = [String]

data Store  = Store { _namespace   :: Set.Set Ident, _free   :: Integer } deriving (Show)

nextIdentifier :: S String
nextIdentifier = do
    free <- gets _free
    modify (\x -> x { _free = free + 1 })
    return $ makeIdentifier $ show free
    where makeIdentifier n = "%" ++ n

addIdent :: Ident -> S ()
addIdent n = do
    namespace <- gets _namespace
    modify (\x -> x { _namespace = Set.insert n namespace })


compile :: Show a => Program a -> Either (Exception a) Instructions
compile prg =
    runIdentity
    (evalStateT (runExceptT (compileProgram prg))
                (Store { _namespace = Set.empty, _free = 1 })
    )


compileProgram :: Show a => Program a -> ES a Instructions
compileProgram (Prog _ stmts) = do
    instr <- concat <$> mapM compileStmt stmts
    let program = prelude ++ instr ++ postlude
    return program
  where
    prelude =
        [ "@dnl = internal constant [4 x i8] c\"%d\\0A\\00\""
        , "declare i32 @printf(i8*, ...)"
        , "define void @printInt(i32 %x) {"
        , "%t0 = getelementptr [4 x i8], [4 x i8]* @dnl, i32 0, i32 0"
        , "call i32 (i8*, ...) @printf(i8* %t0, i32 %x)"
        , "ret void"
        , "}"
        , "define i32 @main() {"
        ]

    postlude = ["ret i32 0", "}"]

compileStmt :: Show a => Stmt a -> ES a Instructions
compileStmt (SAss _ ident@(Ident name) e) = do
    namespace <- gets _namespace
    (handle, instr) <- compileExp e
    let assignInstr = [ if Set.member ident namespace then [] else (alloc name)
           , assign handle name
           ]
    modify (\x->x{_namespace = Set.insert ident namespace})
    return
        $  instr ++ assignInstr
  where
    alloc name = format "%{} = alloca i32" (name)
    assign handle name = format "store i32 {}, i32* %{}" (handle, name)
compileStmt (SExp _ e) = do
    (handle, instr) <- compileExp e
    return $ instr ++ [format "call void @printInt(i32 {})" [handle]]


type Handle = String
type Operation = String

compileExp :: Show a => Exp a -> ES a (Handle, Instructions)
compileExp (ExpAdd _ e1 e2             ) = _compileExp "add" e1 e2
compileExp (ExpSub _ e1 e2             ) = _compileExp "sub" e1 e2
compileExp (ExpMul _ e1 e2             ) = _compileExp "mul" e1 e2
compileExp (ExpDiv _ e1 e2             ) = _compileExp "udiv" e1 e2
compileExp (ExpVar p ident@(Ident name)) = do
    namespace <- gets _namespace
    when (Set.notMember ident namespace) (throwError $ UnboundVariable p)
    id <- lift nextIdentifier
    return $ (id, [buildInstr name])
    where buildInstr name = format "load i32, i32* %{}" (name)
compileExp (ExpLit p e) = do
    when (not $ testOverflow e) (throwError $ LiteralOverflow p e)
    return $ (show e, [])
    where testOverflow e = e <= int32MAX

_compileExp
    :: Show a => Operation -> Exp a -> Exp a -> ES a (Handle, Instructions)
_compileExp op e1 e2 = do
    (a1, i1) <- compileExp e1
    (a2, i2) <- compileExp e2
    id       <- lift nextIdentifier
    return (id, i1 ++ i2 ++ [buildInstr a1 a2])
    where buildInstr n1 n2 = format "{} i32 {}, {}" (op, n1, n2)
