--
-- Represents a relationship between concepts
-- where one concept is dependent on another
--

module ConceptDependency (ConceptDependency(ConceptDependency),conceptdep) where

-- | Concept dependency linking types of a -> a, by 'b'
data ConceptDependency b a =
  ConceptDependency b a a
  deriving (Eq,Ord)


instance (Show b, Show a) => Show (ConceptDependency b a) where
  show (ConceptDependency x y z) = show x ++ ", " ++ show y ++ ", " ++ show z ++ "\n"

-- | Type constructor for a concept dependency between two concepts
conceptdep :: (b,a,a) -> ConceptDependency b a
conceptdep (dep,n1,n2) = ConceptDependency dep n1 n2
