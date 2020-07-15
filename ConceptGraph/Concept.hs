--
-- Represents a concept in the graph
--

module Concept (Concept(Concept),concept) where

-- name associated with a concept
type Name = String

-- concept description
type Description = String


data Concept a =
  -- conecpt has a name and zero or more descriptions
  Concept a


instance (Show a) => Show (Concept a) where
  show (Concept x) = show x ++ "\n"


concept :: a -> Concept a
concept x = Concept x
