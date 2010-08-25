module Data.Persist.AST where

data Multiplicity = OneToOne | OneToMany | ManyToOne | ManyToMany
 deriving (Show, Eq)

data Relationship = Relationship 
  { relName         :: String
  , relMultiplicity :: Multiplicity
  , relFromName     :: String
  , relToName       :: String
  , reversed        :: Bool
  } deriving (Show)

isToOne :: Relationship -> Bool
isToOne r = m == OneToOne || m == ManyToOne
  where m = relMultiplicity r

reverseRelationship :: Relationship -> Relationship
reverseRelationship (Relationship nm m f t r) = Relationship nm (reverseMultiplicity m) t f (not r)

reverseMultiplicity :: Multiplicity -> Multiplicity
reverseMultiplicity OneToMany = ManyToOne 
reverseMultiplicity ManyToOne = OneToMany
reverseMultiplicity x         = x
