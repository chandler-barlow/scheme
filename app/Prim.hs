module Prim where

import LispVal

mkF :: ([LispVal] -> Eval LispVal) -> LispVal
mkF = Fun . IFunc

primEnv :: Prim
primEnv =
  [ ("+", mkF $ binopFold (numOp (+)) (Number 0)),
    ("*", mkF $ binopFold (numOp (*)) (Number 1)),
    ("++", mkF $ binopFold (strOp (<>)) (String "")),
    ("-", mkF $ binop $ numOp (-)),
    ("<", mkF $ binop $ numCmp (<)),
    ("<=", mkF $ binop $ numCmp (<=)),
    (">", mkF $ binop $ numCmp (>)),
    (">=", mkF $ binop $ numCmp (>=)),
    ("==", mkF $ binop $ numCmp (==)),
    ("even?", mkF $ unop $ numBool even),
    ("odd?", mkF $ unop $ numBool odd),
    ("pos?", mkF $ unop $ numBool (< 0)),
    ("neg?", mkF $ unop $ numBool (> 0)),
    ("eq?", mkF $ binop eqCmd),
    ("bl-eq?", mkF $ binop $ eqOp (==)),
    ("and", mkF $ binopFold (eqOp (&&)) (Bool True)),
    ("or", mkF $ binopFold (eqOp (||)) (Bool False)),
    ("cons", mkF Prim.cons),
    ("cdr", mkF Prim.cdr),
    ("car", mkF Prim.car),
    ("file?", mkF $ unop fileExists),
    ("slurp", mkF $ unop slurp)
  ]

type Binary = LispVal -> LispVal -> Eval LispVal

type Prim = [(T.Text, LispVal)]

type Unary = LispVal -> Eval LispVal

type Binary = LispVal -> LispVal -> Eval LispVal

unop :: Unary -> [LispVal] -> Eval LispVal
unop op [x] = op x
unop _ args = throw $ NumArgs 1 args

binop :: Binary -> [LispVal] -> Eval LispVal
binop op [x, y] = op x y
binop _ args = throw $ NumArgs 2 args

numOp :: (Integer -> Integer -> Integer) -> LispVal -> LispVal -> Eval LispVal
binopFold :: Binary -> LispVal -> [LispVal] -> Eval LispVal
binopFold op farg args = case args of
  [a, b] -> op a b
  (a : as) -> foldM op farg args
  [] -> throw $ NumArgs 2 args

fileExists :: LispVal -> Eval LispVal
fileExists (Atom atom) = fileExists $ String atom
fileExists (String txt) = Bool <$> liftIO (doesFileExist $ T.unpack txt)
fileExists val = throw $ TypeMismatch "expects str, got: " val

slurp :: LispVal -> Eval LispVal
