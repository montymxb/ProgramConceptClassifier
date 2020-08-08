--
-- KnowledgeBase.hs
--
-- TODO remove once the rest is cleaned up...this is more of a conceptual layout file
--

module KnowledgeBase where

import Grammar
import Node
import Edge

-- a node is a concept
type Concept = Node

-- edges relate dependent concepts
type ConceptDependency = Edge

-- overall concept map
type ConceptGraph  = ([Concept],[ConceptDependency])

-- list of known concepts on the graph
type KnownConcepts = [Node]

-- list of final concepts desired for students
type GoalConcepts  = [Node]

-- List of nodes that represent a path to one or more goal nodes
type Path = [Node]


conceptGraph_FromGrammar :: Grammar -> ConceptGraph
conceptGraph_FromGrammar g = let nlist = nodes g in (nlist,updateEdgeNodeData nlist (edges g))

-- TODO needs a search algo
-- Likely to be based on setting up the best possible
--generatePathToGoalsWithGrammar :: Grammar -> GoalConcepts -> Path
--generatePathToGoalsWithGrammar g glist = ---can setup various searches in here to demonstrate later
