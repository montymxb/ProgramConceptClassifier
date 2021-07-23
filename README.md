# Program Concept Classifier

The Program Concept Classifier is a practical application of a technique for producing classifications from a program and it's associated syntax in AST form, and relates classifications via a partial-order in a complete lattice. The resulting classification structure provides a taxonomy of programs by the concepts they present through their syntax. This taxonomy structure can be used to guide navigation with regards to programs and the concepts they entail. Much of the underlying theory for how to derive and relate classifations comes from Formal Concept Analysis.

The associated thesis: [Structuring Teachable Knowledge through Program-Concept Classifications](https://ir.library.oregonstate.edu/concern/graduate_thesis_or_dissertations/tq57nz829).

The project is built using [stack](https://docs.haskellstack.org/en/stable/README/).

## What's in this?

This project contains a generalizable implementation of the program-concept classification technique. It utilizes Formal Concept Analysis (FCA) to classify objects and their attributes (programs and their concepts) with respect to each other.

This particular implementation allows taking a list of objects that derive the **Data** typeclass. This allows a generic mapping to be defined recursively over objects, combined with a user-provided mapping function, to extract and associate each 'program' with the concepts entailed by its AST. This approach can work beyond programs in AST form considering the **Data** limitation is still quite broad, but this work did not explore those possibilities.

Upon running `stack ghci`, the functions `main` and `exampleToy` provide examples that can be run immediately to produce the files **AnalysisLattice.png** and **Toy.png** for inspection.

## How does it work?

If you take a peek at the quick implementation in [app/Main.hs](https://github.com/montymxb/ProgramConceptClassifier/blob/master/app/Main.hs) you can see an example of how to this works at a high-level:

1. Load up a database (or folder) of programs that you wish to classify
2. Have a means for parsing those programs (or another means to analyze them)
3. Determine zero or more known & goal programs to serve as your upper and lower bounds for displaying classifications
4. Apply the `analyze` function to a `MappablePrograms` argument, composed of:
    - a concept mapping function
    - a list of known programs
    - a list of goal programs
    - a list of database programs to compare against

The `analyze` function uses all provided programs and extracts the concepts to relate with those programs using the concept mapping function provided. With programs and their concepts together, we utilize FCA to build classifications from their pairings. These classifications are built in such a way that they can be partially-ordered via a sub-set relation. With a few other adjustments, we can produce a complete lattice to describe all classifications in relation to a supremum and infimum.

![Example of a complete lattice of program-concept classifications](https://github.com/montymxb/ProgramConceptClassifier/blob/master/images/AnalysisLattice.png "Example of a complete lattice of program-concept classifications")

This image is not *terribly* insightful, but the general idea is there. If you wish to learn more, I would strongly encourage you read through Chapters 2 and 4 of the [thesis itself](https://ir.library.oregonstate.edu/concern/graduate_thesis_or_dissertations/tq57nz829). If you're more interested in the technical details related to FCA you can go through Chapter 3, but having some preliminary familiarity with FCA is *highly* recommended. You can check out [Uta Priss's descriptions](https://upriss.github.io/fca/fca.html), a [Python implementation of FCA for reference](https://pypi.org/project/concepts/), and the [FCA wiki](https://en.wikipedia.org/wiki/Formal_concept_analysis) can give a general overview with plenty of links to follow through on.

The analysis result produced by this tool is a 2-tuple of:
- A DOT specification suitable for output using GraphViz
- A list of programs that are part of the 'outer fringe'

In the mock implementation present in **Main**, the DOT spec and the lattice are written out as **AnalysisDOT.gv** and **AnalysisLattice.png** respectively.

The outer fringe is a special part of this tool. The term 'outer fringe' itself is borrowed from Knowledge Space Theory, which itself is related to FCA. It describes a list of the next programs that can be used to introduce new concepts. Each program in the list introduces a unique set of concepts, such that no other program in the same list contains the same set of concepts. These programs represent the logical 'next steps' for learning objectives based on the known programs that were provided. Essentially, this implementation provides a deterministic way to explore through a list of programs in a bottom-up fashion.

## What can I use this for?
- Helping new instructors learning to teach CS by structuring examples
- Classifying groups of programs to determine their overall conceptual 'spread', such as in a classroom
- Gauging approximate conceptual similarity between groups of programs

There are also limitations to this approach, such as the general inability to quantify recursion. This is due to the approach outlined here only taking into account the set of concepts derived from the syntax of programs. It does not capture the semantics of programs or the particular structure of their syntax. Future work may benefit from exploring the structural or semantic aspects of programs to find additional points of analysis.

## Questions?

If you're interested in learning more about my work and how it can be applied feel free to reach out to me. I'm more than happy to answer questions and chat.
