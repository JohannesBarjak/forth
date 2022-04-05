{-# LANGUAGE ApplicativeDo #-}
module Parser (pAst) where

import Ast

import Data.Char (isSpace)
import Data.Text (pack)

import Text.Megaparsec hiding (many, some)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void Text

pAst :: Parser Ast
pAst = sc *> many pNode <* sc <* eof

pNode :: Parser Node
pNode = (BNode <$> pInt) <|> pString <|> pDef <|> (BNode <$> pWord)
 
pWord :: Parser BNode
pWord = Word <$> lexeme (takeWhile1P Nothing $ not . isSpace) <?> "word"

pDef :: Parser Node
pDef = do
    pCTWord ":"
    defName <- pInt <|> pWord
    Def defName <$> manyTill pNode (pCTWord ";")

pInt :: Parser BNode
pInt = pCTWord $
    optional pSigned >>= \case
        Just int -> pure $ Int int
        Nothing  -> Int <$> L.decimal
        where pSigned = L.signed (pure ()) L.decimal

pString :: Parser Node
pString = do
    ".\"" *> void spaceChar
    Str . pack <$> lexeme (manyTill L.charLiteral "\"")

pCTWord :: Parser a -> Parser a
pCTWord p = try . lexeme $ p <* (void spaceChar <|> eof)

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "\\") (L.skipBlockCommentNested "(" ")")
