# Research Progress
- produced a visual DAG for BoGL specifically, but for a simpler version of BoGL
- produce a general description of a CFG to represent language dependencies (derived from the grammar)
- updated with notes from last meeting
- should be developing this DAG approach for one or more toy languages
    - abstract graph away from JUST BoGL
    - Arithmetic toy language
- Usage dependencies
    - Program can be considered a syntactic rep of a given language, w/ bound names
    - For every name bound in the program, the type of the binding is a language dependency (NT)
        - This falls in line with the Grammar idea, Terminals are dependent on Nonterminals (Ts are bindings, NTs are types, types are eventually bound into a terminal value)
    - For every unique name used, but not bound in the program, this qualifies as a usage dependency
- goal: produce a method for grading program examples with respect to dependencies in language (syntax) and usage (bound names)
    - grading is subjective, but, given a collection of examples (a curriculum) we could practically determine whether or not they are progressing too fast, too slow, or just right (in regards to introduction of novel syntax and names)
(5/3/2020)
- each kind of dependency should be obtained from a general set of principles
    - usage deps simply based on use of defined names (types,vals, funcs)
        - existing analysis
    - language deps come from syntax (language definitions)
        - lang deps are relationships between nonterminals derived in grammar productions
        - lexical primitives (integers, terminals), not defined in the grammar itself
- enrich symbols (provide explanations...context)
    - something of that...

## May 10th, 2020
- for any rule, LHS must be a symbol as well
- quickly add some code that determines NTs and Ts in our language purely by whether they are present in any LHS or not
  - lower case Ts, upper case NTs on print
- extend Symbol with Def (bind) & Use (call) parts, in addition to what we already have
  - both are akin to Terminal 'T', but allow easier usage dep identification
  - plug this into the arithmetic grammar
  - should allow checking of usage deps now
- Use SymbolType, check email
  - converts them all to NonTerm or Term when doing basic analysis
- plug this into the arithmetic lang
- write a simple version of the Arithmetic Lang using this

## June 21st, 2020
- hypothesis: Can we use a grammar representation of a language to determine the quality of teaching material
  - write analyzer in Dependencies.hs
  - test for the arithmetic language
  - test for BoGL
> sketch out and formalize the rules for gauging material (NOT generic)
  - one for the context of the arithmetic lang
  - one for the BoGL


## Aug 3rd, 2020
- cleanup existing implementation, reassess what needs to be done next (combo of items above & items below)
- some thoughts
  - may want to clean up some new data types to just be type synonyms...have to think about it
  - add a way to print out a concept graph (and a list of paths) as a graphviz specification (that can be done by hand pretty easily)
    - so I can demonstrate the results quickly to Erwig and PM
    - (An example graphviz spec that I can use by hand): https://github.com/osu-cs480-sp20/assignment-3-montymxb/blob/e97bd6fa0b317391d4c653e768f86cc182bf5ecd/example_output/p1.gv

## Aug 4th, 2020
- Transcribe the BoGL grammar from notes into Haskell
- Add gvSpecProduce for concept graphs...infinite loop seems to be a bit of a problem?
  - test it with some stuff (also revise the implementation to simplify things...as needed)
  - show what it takes to use
    - while
    - if-then-else


## Aug 5th, 2020
- Check 'tree.png' in desktop
- Fix stranded 'Uppercase Word'
- Wrap up 'do while', 'if-then-else', and 'let'...they should be their own sections
- Fix plurals to be self-referncing instead...it will auto-cull that way
  - i.e, expr can expand to 'expr expr'
- Unify 'Type' and 'type', a typo there (okay, 'Type' is data type, 'type' is the keyword)
- BoardDef/BoardDecl mixup from program
- remove lowercase & uppercase word
- Change so that Maybe Path is just a ConceptLattice
- Print out the concept lattice (analogy for ConceptGraph)
- Get queries to work, and graph the paths, I'm really curious what it will look like!
- what's interesting, is trying to learn 'if-then-else' from int does not produce a query, so instead, a shared query has to be added
  - unify the results from querying from both int & if-then-else, to observe the commonalities

## Aug 6th, 2020
- Clean up the code-base a bit, specifically in 'query'
- start looking into how we could auto-gen samples based on this information
  - lookup papers about program examples
- I saw some visual breakdowns based on tracing, debugging....
- In addition, I also came across this "https://dl-acm-org.ezproxy.proxy.library.oregonstate.edu/doi/abs/10.1145/3279720.3279726"
  - talks about interactive program construction examples
- there is an additional paper as well, but I could not find it, so I requested it, should show up in a day or 2
- Revisit the idea of 'known', 'unknown', and 'goal' nodes...revisit the goals of potential matchings (i.e) check the mission statement again
  - see how things line up so far
- Research how to auto-gen a code example off of this
  - likely requires formatting the nodes so they can subbed in by the types below?
  - existing research may prove helpful, document and note any findings here

- Okay, so given a concept lattice, and an ordering in that regard...how do we generate our example...
  - here's an idea...one large program, that minimally contains the smallest aspects of the items they want to teach
  - tagged if you will
  - phase in/out items that are minimally relevant from the main example to the sub examples (if any)
  - introduce a TaggedProgram type...uses all the syntactic items, and tags every standalone line of code
    - ex. produce library of examples, selectively serve up a combination of the ones that work best
    - if you understand an expression, the smallest value of 123 should work for a program, since Int should already be understood
    - type TaggedSyntax = (String,[Symbol])
      - describes the syntax in question, the symbol(s) it requires
      - no...data type
- interesting paper on generating example programs
  - talks about 'fading out' examples as a student's knowledge increases over time
    - Title: Program Construction Examples in Computer Science Education: From Static Text to Adaptive and Engaging Learning Technology
    - Author: Hosseini, Roya
    - TN: 1186650


## Aug 8th, 2020
- Remove 'TaggedSyntax', more work for no benefit, and lots of mess...
- Something else I was thinking, about generating examples...from ConceptLattices...hmm
- Above is too complex...instead use the explanation annotations for every syntactic element to also have code explanations in them
- these would make a better approach instead
- add explanations as the annotations for every syntactic element
- the comments on the syntactic elements are the examples used
- the description of each rule set is used as the comment for leading the items, before they are added together
- update graph so edge descriptor contains the comment for the rule in the final ConceptLattice
- rules of explanations (as stated below) should be followed, no more complex than the thing itself!
  - if a violation of the rules presents itself (like expr being needed for if ... then ... else ...) then a known version of that item (expr) must be used
    - if there is no such case, the item can't be taught by this ruleset, without first making an assumption that is reasonable
  - then, given a set of 'Known' and 'Goal' concepts, produce one set of examples describing all the simplest concepts, then the next simplest, and so on, and so forth
    - simplest concepts are those farthest from the goal node by path
    - for a series of unmerged paths, if there are multiple paths from the same Known -> Goal concept, then the 'simplest' concept to start with is the one that takes the fewest steps
    - the most 'complete' concept is the one that has all it's child nodes covered by previous examples
    - need to play around with this idea some more...I like it however
- Print out an example program purely from the concepts displayed...see how that works
  - Path (Concept Symbol) -> Program ...I think
- Comes out a bit messy, the problem is that Nodes (Concepts) only encode something without context for what we know before it
  - An edge encodes information in the 'context' of 2 concepts, so a board of INT, or a board of BOOL, or something else...


## Aug 11th, 2020
- For the moment, stay away from producing examples directly, but break up an existing example (coin toss, tictactoe)
- Compress 'ifthenelse' & 'while' into a single set of nodes
- Integrate query chaining...i.e A -> B -> C .... -> Z, with at least 2 or more queries
  - makes me think that I need to produce a 'query' data type
- Lookup gfold and gmap, Martin had said this would be helpful (instead of writing them all out, handle specific cases only)
  - these are generalized versions of fold map, using Data & Typeable to allow general approaches to map & fold
- Consolidate the existing codebase, particularly in BoGL_Lang.hs AND Query.hs
- Retry 'queryFromKnownToGoal'


## Aug 12th, 2020
- long term goal: can programs be evaluated in terms of pedagogical quality by using concept graphs composed of the grammatical structure of a language, and the concepts used by a given program
- short term goals:
  - 1) Incorporate the actual BoGL AST into this as an item
  - 2) Incorporate an actual BoGL program, written normally, parsed, and returned as a ConceptGraph that we can work on
  - why? Because if we know the actual AST, and 1 or more programs, we can start making relative comparisons in terms of cost, usage, etc.
  - can try to break up programs into smaller parts...sketch it out, in terms of what we're looking for



## Aug 13th, 2020 :: Formal Rules for Evaluating Programs (with respect to Concepts)

#### Boilerplate
- Consider S is the set of all syntactic elements of a language
- Let L be the structure of all these elements (grammar)
- For any list of syntactic elements in L where the 2 or more distinct rules are possible, the rule on the left will be considered the simplest
  - so this implies that the ordering of the rules in our grammar should reflect the complexity of them in the language
  - or perceived teaching order
- Let P represent a runnable program, which is a tree as well.
- Any element of P is also an element of S, and any structure in P is also in L
- P may have the same elements more than once, as well as repeating structure, but nothing introduced is novel

#### Rules
- Let PA be the set of all programs that can be written using L and S
- Consider P and P' to be elements of PA (programs) if not clarified explicitly in any of the following
- Let TOrd(L) be the totally ordered set of elements in L, where the order is established by a depth-first post-order search of L
- Let C1 and C2 (concepts) be elements of L
- C1 is **conceptually more advanced** than C2 if C2 preceeds C1 in TOrd(L)
- C1 is **conceptually unrelated** to C2 if no path can be formed between C1 and C2 in either direction
- P is the **minimal program** if it contains the least number of unique elements, such that any other program in PA has as many or more unique elements, and still runs
  - conceptually simplest
- P is the **maximal program** if it contains the largest number of unique elements, such that any other program in PA has as many or fewer unique elements
  - conceptually complex
- P is **conceptually complete** if for any concept C in P, all reachable nodes from C in L are also present in P
  - **CONCEPTUALLY COMPLETE IS A BAD USE OF TERMINOLOGY**
- Let **Ord(P)** be the number of concepts introduced in program P, where in-order is determined by conceptually less advanced concepts coming before more advanced ones
  - Ord(P) is calculated by running a sort on the concepts to determine order, and counting the number of swaps
  - in order is 0 swaps
  - max is n^2 swaps, where n is the number of concepts in P
- P is **conceptually ordered** if it contains elements in the least possible ordering, such that for any program in PA, P', Ord(P) <= Ord(P')
  - i.e, there is no other ordering of any example with the same distinct concepts that produces a lesser ordering than in P
- Let **C(P)** be the set of unique concepts in P, where P is an element of PA
- P is **equal** to P' if both are elements of PA ^ C(P) = C(P') ^ |P| = |P'| ^ Ord(P) = Ord(P')
- P is **conceptually equivalent** to P' if both are elms of PA and C(P) = C(P')
- P **uses more concepts** than P' if |C(P)| > |C(P')|
- P is **conceptually more advanced** than P' if P' is a proper subset of P and for a concept *c* such that *c* is in P but not in P', there is a path from *c* to any concept *x* present in P'
  - or...just use TOrd(L)....
  - i.e P uses the same concepts as P', and additional concepts that are directly related
- P is **a larger program** than P' if |P| > |P'|
- Any concept C in P represents a **language level (LL)** of P, where LL is a sub-graph starting at C
- Let K be the set of all known concepts by a student in S
- P is said to be **known** if C(P) is a subset of K
- P is said to be **partially known** if C(P) is not a subset of K, and the intersection of C(P) and K has a size >= 1
  - if the intersection produces an empty set, P is said to be **unknown**
- (???) A program is an **introductory example** if a program can be divided into three sub-programs, P1, P2, P3, where P1 is known (and P1 is not the empty program) and P2 is unknown, and P3 is a valid program (including the empty program), and they order P1 -> P2 -> P3 in P


## Aug 14th, 2020
- this really reminds me of something....it was important...but not sure now....I wanted to make a note right after I clocked out....
- We need 'Literal' as a kind of Expr in BoGL
- I was thinking of how 'Extract into Method/Function' behavior is performed...this would be a way to factor out our examples
  - common feature in most IDEs
  - oh...this sounds like an example guided IDE...or a theory for one anyways
  - **A theory for a way of evaluating & refactoring examples to produce conceptually-guided lessons in a targeted language**
- Need to distinguish optional and required nodes for any node (deps of 2 different types) in our graph
  - an expression can be one or many types (don't need to require all if we want to show it) dashed lines maybe
  - a funcEq MUST have a name, parameters, =, expr. Distinguish these with solid lines
  - These mean adding a different kind of edge...hmmm
  - this is important so we can identify what is necessary, and what is not necessary in examples, otherwise we can't do this
- Calculate the 'cost' of a concept (should be easy using the items above ^^)
  - with a total ordering, it's the index
- in an example (Martin talked about breaking up a single large example into sub-parts, recommendations by our tool)
  - break up programs by identifying the set of runnable sub-programs (in our case, body & expr)
    - recommend to extract sub-programs to break it into steps
    - create P1, P2, P3...where P1 is the previous program with the known bit, P2 is the unknown bit alone, and P3 is the rest
  - determine what is extraneous to the example...hmmm
    - for the unknown concepts, mark to remove all concepts that are not directly related to what is required to show it (i.e, can drop board eq to show other thing)
      - this is interesting...we used this approach in terms of how we dropped Board & Input declarations...we can formalize this further maybe
  - what is necessary
    - All required concepts beneath an unknown concept, and all required concepts to reach it from above
  - what a good order for examples would be
    - let's go with TOrd(L) for now
- Medium-Term Ex: apply this to coin flip, nim, rock-paper-scissors
- try directly encoding the AST for these programs into a graph (skipping the grammar)
  - this is where 'scrap your boilerplate' might be helpful
  - from this, illuminate the concepts and their relationships
  - use this to try and gauge the example itself
- BoGL subset...just expressions or something to simplify what we are trying to look at


## Aug 15th, 2020
- add in required/optional dependencies, this will adjust how we handle deps
  - deps 'should' be externally definable...
- Change ConceptDependency to a type of (b,a,a)
  - so if we 'want' to add optional edges..then we can do so via a 'print' to describe the edge type...yeah...that would be best
- integrate stack to build a package out of this


## Aug 20th, 2020
- scrap using special show...for now
- Build in Spiel-Lang into your project, so you have access to it
- use BoGL's stack package to pull in it's AST directly from here...w/out having to interact with it directly, yep!


## Aug 21st, 2020
- Attempted work with SYB, spent a very very very very long time trying to make this work in the context that I was looking for
  - I was under the impression that you can create a traversable data type such that you can continue to traverse down the data type without having to add anything
  - It seems that is the case, **but** you have to add in the appropriate Typeclasses, such that the logic for chaining down into different values is feasible
  - also noted that my own setup did not work when I applied this to it, it just stopped at the first entry...hmmm

## Aug 22nd, 2020
- Okay, what I want is to be able to
  - take a starting value
  - the data type for a syntactic element is the starting Class
  - the value constructors for that data are the edges that
- FIRST for a test A B C types...make sure it traverses properly
  - yeah, that looks okay...
- SECONDLY for the actual BoGL AST, given a program in BoGL that is valid
  - produce a BoGL program that we can use for testing
  - attempt to cgraph it until it works...
- Seeing that I want to have something more general...
  - maybe something that
    - 1 grabs the constructor if there is one
    - 2 reports the type as well as the constructor
    - ignores this for all the other types
    - this is a crawled along the structure, and reported at the end...right...
- Stopped at setting this up for the BoGL ast...seems kind of not that generic though...tbh...kind of a pain in the butt actually


## Aug 24th, 2020
- https://neilmitchell.blogspot.com/2013/10/haskell-type-graphs-with-uniplate-and.html (helpful for Uniplate instead of SYB)
- take advantage of the 'universe' function in Uniplate (https://wiki.haskell.org/Uniplate)
  - can traverse a given type, but gets stuck when trying to jump into the next type...
  - okay, so we can only flatten and browse a tree structure all of the same data type, jumping into another one breaks it...
  - using this, skip the values we don't care about, and continue digging into those that we do care about
- run these steps
  - 1. Hook in the typeclass instances for conceptualizing all aspects of the BoGL syntax
    - little issue is enumerating over fields...this will work for record syntax, but what about non-record syntax? Try things out in T2
    - if record syntax present (i.e not the empty list), try to access those elements?
    - had a thought while walking over, I cannot create a 'list' of fields, because the types are different, however a tuple of fields would be possible
  - 2. each type is the node name
  - 3. each constructor name is the edge name between the high type and the low type
    - *working on finishing the edge encodings, so we can see how things look*, graph is kind of a mess as well...

## Aug 25th, 2020
4. Verify this works in a general sense w/ the program below
  - working on it...a little overly complex, don't need a set yet, but need to cleanup the output, because some things are incomplete
  - go back over types that you'll be working on again, they need to be complete!!!
5. Then start to hook this into the type checker
  - take advantage of the setup from the AST
- Change nodes from simple node into
- (Env, Expr, Type)
- Every node is associated with some expression that is a usable example
- Each expression has a given type
- Each expression is evaluated within the context of an environment, such that we can then determine what can be bound to produce an evaluable result
- Test it with skipping into a ConceptGraph directly, without mapping from the Grammar...doable right?
- might be weird with the typeclasses perhaps...eh w/e
- should allow extracting nodes into a list
- can use that list to make queries from nodes into the full graph
- Downsides of doing it this way? The actual AST implementation may show some internal bits that don't make sense to what is seen in a program


# Additional Notes

## Rules for the Arithmetic Language
1. Produce a representation that captures the grammatical structure of the language
  - representation uses symbols that represent distinct syntactic elements in the language (this could use clarification towards the process)
  - NTs are determined as symbols that show up on the LHS of any rule
  - Ts are any symbols that do not show up as the LHS of any rule
2. For a given program, produce an equivalent representation in the grammar produced in step 1
3. For N programs, do the same
4. Compare the...

## What makes good teaching material in English?
- Limited use of grammar (nothing fancy)
- Limited length of material (nothing exhausting)
- Restricted scope to something relevant (nothing boring)

## What makes bad teaching material in English?
- Using concepts to base learning on that a student doesn't know of (students are unprepared)
- Teaching concepts in an arbitrary fashion (lack of structure)
- Missing concepts that will be needed later on (missing important details)
- Misleading students in terms of what they are expecting (failed expectations)

## Breaking down Good/Bad Examples
- Good examples serve a goal
- Good examples are concise
- Good examples use what a student has learned or already knows
- Good examples teach or reiterate unfamiliar concepts
and
- Bad examples do not serve a goal
- Bad examples are not concise
- Bad examples do not use what a student already knows
- Bad examples teach or reiterate familiar concepts

## As a formalized set of rules
- For a given set of language dependencies (grammar) L, a subset of these that the student understands U, a subset of these that the student does not understand N, and a goal subset the student wishes to learn G.
  - L, U, N, G
- For an example written in this language, deduce the language dependencies present in the example by matching it to the equivalent grammatical representation
- This example is a good example if it
  - uses previous dependencies the student understands (if any)
  - introduces new dependencies the student does not understand that
    - are goals
    - are dependencies of goals
- This example is the 'simplest' example if there is no other way to write a program that introduces the same new dependencies using fewer known dependencies
  - This is highly contingent on whether the student actually understands what they say they do

So, good examples are segments along a structured path, from a given knowledge base (or recently acquired bit(s) of knowledge), such that they allow a student to stretch a little bit further towards a goal.
