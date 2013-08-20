module Keen.Syntax where

data Top
    = TopValue Let
    | TopType String [String] Type
    | TopData String [String] [(String, [Type])]
    | TopRecord String [String] [(String, Scheme)]
    | TopOperator String Associativity [Identifier] [Identifier] [Identifier]
    | TopImport Alias (Maybe [(Alias, Maybe [Alias])])
    deriving Show

data Alias = Alias (Maybe String) String
    deriving Show

data Identifier = Identifier String
    deriving Show

data Associativity
    = LeftAssociative
    | RightAssociative
    | NonAssociative
    deriving Show

data Position = Position String Int Int
    deriving Show

data Let = Let String (Maybe Type) Term
    deriving Show

data Term
    = Lambda Identifier (Maybe Type) Term
    | Variable Identifier
    | Apply Term Term
    | LetIn [Let] Term 
    | Int Int
    | String String
    | Double Double
    | Unresolved [Term]
    | Term `At` Position
    deriving Show

data Type 
    = Type Identifier
    | TypeApply Type Type
    | TypeVariable String
    | Type `TypeAt` Position
    deriving Show

data Scheme = Scheme [String] Type
    deriving Show

data Pattern
    = PatternWildcard
    | PatternVariable String
    | PatternConstructor Identifier [Pattern]
    | PatternFunction Identifier Pattern
    | PatternList [Pattern]
    | PatternString String
    | PatternInt Int
    | PatternDouble Double
    | Pattern `PatternAt` Position
    deriving Show

-- End of file
-- nice due to a bug in this IDE
-- that hides the last lines

