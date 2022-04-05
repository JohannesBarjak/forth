module Ast(Ast, Node(..), BNode(..)) where

type Ast = [Node]

data Node
    = Str   Text
    | BNode BNode
    | Def   BNode [Node] deriving (Show, Eq, Ord)

data BNode
    = Word Text
    | Int  Int deriving (Show, Eq, Ord)
