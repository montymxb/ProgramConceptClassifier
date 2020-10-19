# Research Progress on Concept Graph (Lattice)
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
- Verify this works in a general sense w/ the program below (working on it now)
  - Fix so that names of graphs use 'safe ids'
    - for vertex ids: should remove quotes, and spaces, replace w/ underscores
    - labels look 'ok' for vertices
    - nodes for edges should also be 'safe', remove quotes & spaces
    - edge labels broken, need fixing
    - fix up analyzing of lists...think I can fix that up actually
    - some exprs don't finish, double check all types, make sure everything is defined properly
    - Need to fix how Lists of elements are evaluated conceptually
      - this should be done by either:
        - skipping the list, relating the underlying concept directly
        - including the list as the type, and then running in the type, like "List Int"
          - this would be better...

## Aug 26th, 2020
- cleanup some warnings...
- worked on the haddock integration to set things up with doc generation for things


## Aug 27th, 2020
- finish up the haddock PR
  - cleanup warnings
  - integrate automatic doc generation and publication on the gh-pages website


## Aug 28th, 2020
- sit down and investigate the typechecker thoroughly, refresh your conceptual understanding of this particular implementation
- use this understanding to go write out a brief explanation of how I intend to use it (did so under the research notes)
- formalized some of the important details on this same date (but in the meeting notes record instead)
- decided to cool my heels in regards to actual implementation, the BoGL typechecker is about to undergo a change, best to wait


## Aug 31st, 2020 (Mon)
- look up information on typechecker theory
- look up research on typecheckers
- [Research] Concept of a Knowledge Graph might be helpful (?) for student items
- [Research?] Clocked typed theory has some odd elements that may be of interest...possibly
- [CSforAll] Talks about using an online-functional language, O-Caml, w/ automatic grading, may be of interest to us (https://dl-acm-org.ezproxy.proxy.library.oregonstate.edu/doi/10.1145/3341719)
- [CSforAll] Decomposition for teaching, https://dl-acm-org.ezproxy.proxy.library.oregonstate.edu/doi/epdf/10.1145/3279720.3279736

## Sept 1st, 2020 (Tue)
- Helpful paper on concept lattices: https://www-sciencedirect-com.ezproxy.proxy.library.oregonstate.edu/science/article/pii/S0304397516306806?via%3Dihub#br0290
    - especially Definition 8 (Section 2.2)
- Sat down and worked through some ideas on paper, in particular with regards to how I should show 'types' vs. 'representations'
  - turns out this is the wrong idea, as I'm worrying about the actual Haskell type & representations, which is not what we are actually working with
  - this should be akin to a grammar rep, and so it should preserve the same structure we would do by hand (and so the constructor names need their own nodes as well, since they are essentially productions on their own)
- so need to ADD in the constructors as their own nodes, NOT as separate edges...
- In addition, I'm thinking of using the TC expanded environment as a way to use a 'program as a proof of a concept' (loosely worded at the moment)
  - Needs to be revised conceptually, and in terms of how it is stated, and to make it work a bit better
  - But, now I just need to dial in the specifics of how I want to do this
  - Can still use Known -> Unknown, and in addition I can potentially discard what I was doing before...or not? we'll see as we go (research...)

## Sept 2nd, 2020 (Wed) (LAST)
- Fixing up things, cleaning up the repos...
- Formalize some of these specs in pseudo paper...should document your research, and you can chop it up later as you go


## Sept 16th, 2020 (Wed)
- Go back over and review items

## Sept 16th-24th, 2020 (Thu)
- From before, the Constructor names shouldn't be the edges, they should be their own nodes as well...
- start to hook this (the AST graphing tool) into the type checker
  - take advantage of the setup from the AST
  - shouldn't have to change anything to make it work with BoGL (ehhh?)
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


## Sept 25th, 2020 (Fri)
- (observation) Record my observations/results in this format from here on out, so I can have something for my thesis and such
  - (obs) or like this


## Oct. 2nd, 2020 (Fri)
- Fix up so that this thing works (against updated BoGL)
  - (obs) Fixed it up
- Verify, I already skipped the grammar directly and reference the AST of the language itself, right? (pretty sure that's what I last did)
  - (obs) Yes, this works
- Re-familizarize myself with how to generate my list from BoGL
- THEN, Constructor names should be their own nodes, fix this
  - put down constructors as generating their own nodes, rather than named edges...
  - need additional edge connecting a type to it's own constructor...this can be programmed in automatically..I think?
- The constructor for lists should be it's own node
- Primitive should be a node
- Expr SourcePos should go to a node named 'I', NOT just an edge named 'I'
- BoardDef should NOT refer to itself by an edge (has to do with the type and constructor having the same name...)
- InputDef should not refer to itself, type & constr share a name
- Binop should NOT refer back to 'Expr SourcePos' (can't fix at the moment...)
- Add +,-,*,/ operations as well, to get an idea of how things relate (extend existing equation)
- consolidate the existing implementation, prune, will help me situate
- verify a node can be an abstract 'thing', such as (Env,Expr,Type)
  - could be int, and such, this will allow us to take our system a bit further...proposing a general classification for a formalizable tree of concepts
  - (obs) this is correct
- make a separate directory, and fill it in with the bones of a thesis...will help to see what I need to fill things in
  - a trial, if I were to write now, what would I be missing, what am I showing...I will need 100+ pages of material...it's going to take a long-long-long-long time if completely full, right?
  - (obs) As noted in the last meeting, I should start to describe what 'kind' of system this will be, and how it will work
- Start theorizing how to tap into the typechecked tree produced by the typechecker in BoGL
  - (obs) Probably won't be able to...instead I'm going to have to make a separate copy...as much as tapping in directly is nice, the actual AST has artifacts that are not representative of the final language
- Next step, actually tap into the typechecked tree, and record my results (not yet...)


## Sun, Oct. 11th
- Write in an Abstract Syntax standin for BoGL based on Alex's new changes (with consideration to the existing designs)
- Write in a new test module that taps into this Abstract Syntax
- Write in 3 programs directly in the abstract syntax as an AST (checked!)
- Generate a ConceptGraph on the results of the tool
- Verify this can be used to analyze programs 1-4
- (obs) The layout of this thing is a bit of a mess as compared to the other items...
- Dip back into the TOODs in the file itself, make corrections
- Compare the old graph vs. the new graph of a program (where it's actually straight up TicTacToe)
- Use my tool to determine whether or not the programs are in/out of order
  - isInOrder ...
  - start with a fixed 'Known' list
  - start with an unknown list of all unique Concepts
  - using the concept graph, for any node that's added, ensure that all nodes it depends on are ALSO known
  - if they are, consider this acceptable, and add it to the known list
  - if NOT, report out of order error, and indicate the item that is out of order...
  - 30 more mins on this and then cut it out...
- Undo the '*' thing, that doesn't help
- Remove duplicate nodes in the CG

## Mon, Oct. 12th
- Fixing Cycles
  - 2 functions
  - 1st maps all concepts over the 2nd
  - 2nd accumulates the visited nodes by making a traversal, and then removes the visited edge, and sets the new tgt as the target of that edge
    - this fans out, and so it does this for ALL matches
    - if any match adds a redundant node, that edge forms a cycle, return it, skip it, and continue crawling
    - at the end no cycles == [], but anything returned should be removed, and the next concept is mapped over it again
- Verify that all concepts can be traced back to 'Concept', if not report an error
- Fix stranded concepts
  - Tup, While, Cond, BTuple, BBoard
  - fixes broke a lot..
- Improve printing of concept data (change from a => String), for showing isKnown information
- Was interesting to see that BBoard is a BType, which a board includes, and so a board can be defined in terms of a Board, but that means that BBoard is lesser than board...

## Oct. 18th, 2020
- Research points
  - "knowledge discovery", or "knowledge processing" keeps coming up, this is likely to be of interest
  - not sure if I really understood how to setup via Formal Concept Analysis properly, verify my implementation is correct to this theory
  - refactoring based on the patterns for knowledge discovery is not something I have literally seen, to conceptually introduce aspects of programs
- Look at rules below, I'm pretty sure if a concept is known it can be assumed that all concepts underneath it are known as well
  - can be qualified as Known vs. Understood?
  - Known concepts are just themselves
  - Understood concepts implicitly include all sub-concepts
  - Combine these two lists...that would be nice to do, and then run the knownCheck on this
  - leaving it alone for now...
- Clean up prior work a bit, document parts that are undocumented
- Verify that if A is unknown, but B is known, then A can be found out as well (no gap...)
  - I 'think' this is being preserved...but check it with the simple program first...
- *(Obs) Along the way of my work I have noticed that the set of concepts produced is in the same order as they are first discovered. This has some interesting implications.*
   - 1st) Programs produce their own known sets that could be used as knowns to evaluate subsequent programs, and to compare directly whether they are advancing too quickly or not (an additional pedagogical tool). As in 'after this program you will know A,B,C'.
     - In turn this can be used to evaluate pacing to some degree? Possibly
   - 2nd) These sets can directly be evaluated for a relative ordering to determine whether they are in order or not, and what concepts are out of order
   - 3rd) Far fetched, but was thinking of a type system kinda thing that could be formulated as an expanding concept lattice (or knowledge graph). Each program is responsible for proving it's own elements that are not previously known. Test of organization perhaps? Hmmm...or getting back some warnings or something...this particular idea needs further thought
   - 4th) The idea of conceptually significant statements comes up, which are ones which introduce novel concepts, and this is how we would be able to tag things in order to suggest addressing them. This is where we have to consider refactoring...
- Verify that the order that the ConceptGraph is built is the proper order to follow (most likely this will need some rectifying, but so far it seems good!)
- run the 'isKnown' checker on the Simple program, assert that Actual meets Expected
  - no, it does not, 'Simple' is on known but does NOT work?
- (Obs), perhaps, rather than trying to 'fix' things continually, this is indicative of the overhead required to be able to write even a simple program, and suggests a minimum level of understanding to be able to write a program. This would be the smallest set of examples required to be able to know how to write any program in the given language. Such prior understanding is critical to getting a foothold, but not covered by this then? You can't analyze anything other than a program here.
- Simplify the example program to make all kinds of statements...
- *(Obs) A depth first search of the concept graph produced leads to an ordering of the concepts in the order that they were introduced*. But the setup won't allow for certain recursive defs (like Statement1 back to Statement0, even though this is valid...)*
- (Obs), we can understand how the graph is expanded over time by comparing the results of understanding concepts from one program to another, So the knowns as the result of working with one program can be used as the knowns of another program. It can be literally stated that knowning the conepts involved in program 1 will be pre-requisites for program 2, but another program X could also fill this gap to the same extent, more or less...
- Setup the list approach for the BoGL examples
- (Obs) Don't have refactorings yet, but I do have ways to accumulate knowledge of concepts over time through a list of programs...along with independently assessing the programs too for how they are laid out

##> Current
- Would be nice to have a 'show' instance or something for each of these program types that produces the concrete syntax from the abstract syntax?
- Use my tool to determine how to improve the order of things (by re-ordering)
  - In particular, experiment with insertions to help teach what's being given
    - objectives to maximize are those we associate with good examples
      - teach good habits
      - minimize unnecessary addons
      - teach the goals in question, and do not go past them
  - Be able to tag elements of an AST with their respective orderings....
    - new data type (a,[String]), where 'a' is an AST element, and [String] is the associated concept tags
    - then we can recrawl the AST, and see how things are organized
    - when an AST element is encountered that is not 'known' yet, do one of 2 things
      - search the following elements to see if any sub-element(s) includes the necessary deps, and recommend they be moved upwards
        - but also do not introduce further conceptual mis-orderings
      - if nothing found, recommend the addition of AST elements containing those concepts above the given concept
- Use my tool to at least be able to analyze and potentially pick apart sub-parts
- begin designing how the final tool will work, and by the characteristics that might help define what the final system will look like
- Look up related work for refactoring research, and add this to my related works section!



- (from TODOs, more relevant here) Work on thesis/research
    - had an idea of maxing/mining some quality of the ordering...
    - had an idea of the 'program as a series of lets' in haskell
        - i.e. you have to order it so that the thing is used AFTER it is declared
        - concepts can only be used after they are defined basically, or introduced in an acceptable context
        - can't co-introduce
        - can't reverse introduce
        - must introduce in such as a way that no other following program could introduce that concept as novel...



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
