--
-- Represents a relationship between concepts
-- where one concept is dependent on another
--

module ConceptDependency (ConceptDependency) where

-- | Concept dependency linking types of a -> a, by 'b'
type ConceptDependency b a = (b,a,a)


--instance (Show b, Show a) => Show (ConceptDependency b a) where
--  show (ConceptDependency x y z) = show x ++ ", " ++ show y ++ ", " ++ show z ++ "\n"

-- | Type constructor for a dependency between two concepts
--conceptdep :: (b,a,a) -> ConceptDependency b a
--conceptdep (dep,n1,n2) = (dep,n1,n2)
