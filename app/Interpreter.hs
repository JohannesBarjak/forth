{-# LANGUAGE OverloadedLists, ViewPatterns #-}
module Interpreter
    ( Env
    , Stack
    , initWords
    , runInterpreter
    ) where

import Control.Monad.Trans.Except (throwE)
import Data.Map qualified as M
import Data.Vector as V (Vector, cons, uncons)
import System.IO (putChar)

import Ast

type Stack = [Int]
type Def   = Vector RunTimeVls

data RunTimeVls
    = Fun  (ExceptT Text (State Stack) ())
    | Proc (ExceptT Text (State Stack) (IO ()))
    | Num  Int
    | Txt  Text

type Env = Map BNode Def

initWords :: Env
initWords = [ (Word "+",    [Fun $ binOp (+)]),        (Word "-",    [Fun $ binOp (-)])
            , (Word "*",    [Fun $ binOp (*)]),        (Word "EMIT", [Proc  emit])
            , (Word ".",    [Proc  dot]),              (Word "CR",   [Proc  cr])
            , (Word "SWAP", [Fun   Interpreter.swap]), (Word "DUP",  [Fun   dup])
            , (Word "DROP", [Fun   Interpreter.drop]), (Word "ROT",  [Fun   rot])
            ]

runInterpreter :: Ast -> Env -> Stack -> IO (Either Text (Stack, Env))
runInterpreter ast env stack = runExceptT $ interpret ast env stack

interpret :: Ast -> Env -> Stack -> ExceptT Text IO (Stack, Env)
interpret []             env s = pure (s, env)

interpret (Def w ds:rs)  env s = do
    def <- hoistEither $ toDef ds env
    interpret rs (M.insert w def env) s

interpret (BNode w:ws)         env s = run w env s >>= interpret ws env
interpret (Str   w:ws)         env s = putTextLn w >> interpret ws env s

toDef :: Ast -> Env -> Either Text Def
toDef []     _                              = Right empty
toDef (Str   w:ws) env                            = cons (Txt w) <$> toDef ws env
toDef (BNode w:ws) env @ (M.lookup w -> Just def) = (def <>) <$> toDef ws env
toDef (BNode w:ws) env                            = (cons . Num) `appIfNum` w <*> toDef ws env
toDef (Def _ _:_)  _                              = undefined

run :: BNode -> Env -> Stack -> ExceptT Text IO Stack
run w (M.lookup w -> Just def) s = run' def s
run w _                        s = hoistEither $ (:s) `appIfNum` w

run' :: Def -> Stack -> ExceptT Text IO Stack
run' (V.uncons -> Just (v, vs)) s = uncurry runWithSE $ case v of
        (Fun  f) -> apply f s
        (Proc f) -> first (>>= lift) $ apply f s
        (Num  n) -> (pure (), n:s)
        (Txt  t) -> (putTextLn t, s)
        where runWithSE eff ns = eff >> run' vs ns
run' _                          s = pure s

cr :: ExceptT Text (State Stack) (IO ())
cr = pure . putChar $ '\n'

dot :: ExceptT Text (State Stack) (IO ())
dot = putText . show <$> pop

emit :: ExceptT Text (State Stack) (IO ())
emit = putChar . chr <$> pop

swap :: ExceptT Text (State Stack) ()
swap = popN 2 >>= cat . rotate 1

dup :: ExceptT Text (State Stack) ()
dup = pop >>= cat . replicate 2

drop :: ExceptT Text (State Stack) ()
drop = void pop

rot :: ExceptT Text (State Stack) ()
rot = popN 3 >>= cat . rotate 2

binOp :: (Int -> Int -> Int) -> ExceptT Text (State Stack) ()
binOp op = liftA2 (flip op) pop pop >>= push

cat :: Stack -> ExceptT Text (State Stack) ()
cat = modify . (++)

push :: Int -> ExceptT Text (State Stack) ()
push = modify . (:)

popN :: Int -> ExceptT Text (State Stack) [Int]
popN = (`replicateM` pop)

pop :: ExceptT Text (State Stack) Int
pop = get >>= \case
    [] -> throwE "Stack Underflow"
    _  -> state $ \(x:xs) -> (x, xs)

rotate :: Int -> [a] -> [a]
rotate = Prelude.drop <> take

appIfNum :: (Int -> b) -> BNode -> Either Text b
appIfNum f (Int  n) = Right $ f n
appIfNum _ (Word w) = Left  $ w <> " ?"

apply :: ExceptT Text (State Stack) a -> Stack -> (ExceptT Text IO a, Stack)
apply f s = first hoistEither $ runExceptT f `runState` s
