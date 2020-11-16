{-# LANGUAGE DeriveDataTypeable #-}
--
-- Represents a concept in the graph
--

module ConceptGraph.Concept (Concept(Concept),concept) where

import Data.Data

-- | Any concept data type
data Concept a =
  -- conecpt has a name and zero or more descriptions
  Concept a
  deriving (Eq,Data,Typeable)


instance (Show a) => Show (Concept a) where
  show (Concept x) = show x ++ "\n"


-- | Type constructor for a concept
concept :: a -> Concept a
concept x = Concept x
