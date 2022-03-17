module Main where

import Data.Text.Read (decimal)
import qualified Data.Map as Map
import System.IO (putChar)
import Control.Monad.Trans.Except (throwE)

main :: IO ()
main = getArgs >>= (maybeAt 0 >>> \case
        Just filename -> do
            file <- readFileText filename
            runExceptT (interpret (words file) initWords [])
                >>= either putTextLn (const $ pure ())
        Nothing       -> repl initWords [])

type Stack = [Int]
type Def   = [ForthVls]

data ForthVls
    = Fun   (ExceptT Text (State Stack) ())
    | IOFun (ExceptT Text (State Stack) (IO ()))
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
        Left  err      -> putTextLn err >> repl env s
        Right (ns, ne) -> print ns     >> repl ne ns

interpret :: [Text] -> Env -> Stack -> ExceptT Text IO (Stack, Env)
interpret []          env s = pure (s, env)
interpret (":":";":_) _   _ = throwE "Empty Definition"

interpret (":":w:ws)  env s = do
    def <- hoistEither $ toDef ds env
    interpret rs (Map.insert w def env) s
        where (ds, _:rs) = span (/= ";") ws

interpret (w:ws)      env s = run w env s >>= interpret ws env

toDef :: [Text] -> Env -> Either Text Def
toDef []     _   = Right []
toDef (w:ws) env = case Map.lookup w env of
        Just def -> (def ++) <$> toDef ws env
        Nothing  -> ((:) . Num) `appIfNum` w <*> toDef ws env

run :: Text -> Env -> Stack -> ExceptT Text IO Stack
run word env s = case Map.lookup word env of
    Just def -> run' def s
    Nothing  -> hoistEither $ (:s) `appIfNum` word

run' :: Def -> Stack -> ExceptT Text IO Stack
run' []           s = pure s
run' (Fun   f:ds) s = err >> run' ds ns
    where (err, ns) = apply f s

run' (IOFun f:ds) s = (liftIO =<< io) >> run' ds ns
    where (io, ns) = apply f s

run' (Num   n:ds) s = run' ds (n:s)

add :: ExceptT Text (State Stack) ()
add = do
    (n0, n1) <- pop2
    stackAdd [n1 + n0]

sub :: ExceptT Text (State Stack) ()
sub = do
    (n0, n1) <- pop2
    stackAdd [n1 - n0]

emit :: ExceptT Text (State Stack) (IO ())
emit = putChar . chr <$> pop

swap :: ExceptT Text (State Stack) ()
swap = do
    (n0, n1) <- pop2
    stackAdd [n1, n0]

dup :: ExceptT Text (State Stack) ()
dup = do
    n <- pop
    stackAdd [n, n]

drop :: ExceptT Text (State Stack) ()
drop = do
    _ <- pop
    pure ()

rot :: ExceptT Text (State Stack) ()
rot = do
    n0       <- pop
    (n1, n2) <- pop2

    stackAdd [n1, n2, n0]

apply :: ExceptT Text (State Stack) a -> Stack -> (ExceptT Text IO a, Stack)
apply f s = first hoistEither (runExceptT f `runState` s)

stackAdd :: Stack -> ExceptT Text (State Stack) ()
stackAdd = modify . (++)

pop2 :: ExceptT Text (State Stack) (Int, Int)
pop2 = liftA2 (,) pop pop

pop :: ExceptT Text (State Stack) Int
pop = get >>= \case
    []    -> throwE "Stack Underflow"
    (_:_) -> state $ \(x:xs) -> (x, xs)

appIfNum :: (Int -> b) -> Text -> Either Text b
appIfNum f w = either (const $ Left "?") (Right . f . fst) $ decimal w
