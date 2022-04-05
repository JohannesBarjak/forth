module Main where

import Text.Megaparsec (parse, errorBundlePretty)

import Ast
import Interpreter
import Parser

main :: IO ()
main = getArgs >>= \case
        Just filename -> do
            file <- readFileText filename
            parseFile filename file >>= \case
                Just ast -> runInterpreter ast initWords []
                                >>= either putTextLn (const $ pure ())
                Nothing  -> pure ()
        Nothing       -> repl initWords []
        . maybeAt 0

repl :: Env -> Stack -> IO ()
repl env s = do
    putText "> "
    hFlush stdout
    input <- getLine

    parseFile "repl" input >>= \case
        Just ast -> runInterpreter ast env s >>= \case
            Left  err      -> putTextLn err >> repl env s
            Right (ns, ne) -> print (reverse ns) >> repl ne ns
        Nothing  -> repl env s

parseFile :: String -> Text -> IO (Maybe Ast)
parseFile filename file = case parse pAst filename file of
    Left  err -> do
        putStrLn $ errorBundlePretty err
        pure Nothing
    Right ast -> pure $ Just ast
