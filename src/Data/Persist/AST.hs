module Data.Persist.AST where

data Multiplicity = OneToOne | OneToMany | ManyToOne | ManyToMany
 deriving (Show, Eq)

data Relationship = Relationship String Multiplicity String String
 deriving (Show)
