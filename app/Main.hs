module Main where

import Data.Text.Read (decimal)
import qualified Data.Map as Map
import System.IO (putChar)
import Control.Monad.Trans.Except (throwE)

main :: IO ()
main = repl initWords []

type Stack = [Int]
type Def   = [ForthVls]

data ForthVls
    = Fun   (ExceptT String (State Stack) ())
    | IOFun (ExceptT String (State Stack) (IO ()))
    | Num   Int

type Env = Map Text Def

initWords :: Env
initWords = fromList
    [("+",   [Fun   add]),       ("-",   [Fun sub]),
    ("emit", [IOFun emit]),
    ("swap", [Fun   Main.swap]), ("dup", [Fun dup]),
    ("drop", [Fun   Main.drop]), ("rot", [Fun rot])]

repl :: Env -> Stack -> IO ()
repl env s = do
    putText "> "
    hFlush stdout
    input <- getLine

    runExceptT (interpret (words input) env s) >>= \case
        Left  err      -> putStrLn err >> repl env s
        Right (ns, ne) -> print ns     >> repl ne ns

interpret :: [Text] -> Env -> Stack -> ExceptT String IO (Stack, Env)
interpret []          env s = pure (s, env)
interpret (":":";":_) _   _ = throwE "Empty Definition"

interpret (":":w:ws)  env s = do
    def <- hoistEither $ toDef ds env
    interpret rs (Map.insert w def env) s
        where (ds, _:rs) = span (/= ";") ws

interpret (w:ws)      env s = run w env s >>= interpret ws env

toDef :: [Text] -> Env -> Either String Def
toDef []     _   = Right []
toDef (w:ws) env = case Map.lookup w env of
        Just def -> (def ++) <$> toDef ws env
        Nothing  -> ((:) . Num) `appIfNum` w <*> toDef ws env

run :: Text -> Env -> Stack -> ExceptT String IO Stack
run word env s = case Map.lookup word env of
    Just def -> run' def s
    Nothing  -> hoistEither $ (:s) `appIfNum` word

run' :: Def -> Stack -> ExceptT String IO Stack
run' []           s = pure s
run' (Fun   f:ds) s = do
    let (err, ns) = runExceptT f `runState` s
    hoistEither err
    run' ds ns

run' (IOFun f:ds) s = do
    let (io, ns) = runExceptT f `runState` s
    liftIO =<< hoistEither io
    run' ds ns

run' (Num   n:ds) s = run' ds (n:s)

add :: ExceptT String (State Stack) ()
add = do
    (n0, n1) <- pop2
    stackAdd [n1 + n0]

sub :: ExceptT String (State Stack) ()
sub = do
    (n0, n1) <- pop2
    stackAdd [n1 - n0]

emit :: ExceptT String (State Stack) (IO ())
emit = putChar . chr <$> pop

swap :: ExceptT String (State Stack) ()
swap = do
    (n0, n1) <- pop2
    stackAdd [n1, n0]

dup :: ExceptT String (State Stack) ()
dup = do
    n <- pop
    stackAdd [n, n]

drop :: ExceptT String (State Stack) ()
drop = do
    _ <- pop
    pure ()

rot :: ExceptT String (State Stack) ()
rot = do
    n0       <- pop
    (n1, n2) <- pop2

    stackAdd [n1, n2, n0]

stackAdd :: Stack -> ExceptT String (State Stack) ()
stackAdd n = modify $ \ns -> n ++ ns

pop2 :: ExceptT String (State Stack) (Int, Int)
pop2 = liftA2 (,) pop pop

pop :: ExceptT String (State Stack) Int
pop = get >>= \case
    []    -> throwE "Stack Underflow"
    (_:_) -> state $ \(x:xs) -> (x, xs)

appIfNum :: (Int -> b) -> Text -> Either String b
appIfNum f w = either (const (Left "Undefined Word")) (Right . f . fst) $ decimal w
