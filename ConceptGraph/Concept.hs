--
-- Represents a concept in the graph
--

module Concept (Concept(Concept),concept) where

-- | Name associated with a concept
type Name = String

-- | Concept description
type Description = String


-- | Any concept data type
data Concept a =
  -- conecpt has a name and zero or more descriptions
  Concept a
  deriving (Eq)


instance (Show a) => Show (Concept a) where
  show (Concept x) = show x ++ "\n"


-- | Type constructor for a concept
concept :: a -> Concept a
concept x = Concept x
