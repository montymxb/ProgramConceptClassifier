# Sources for Bibliography

Started organizing everything Mar. 28th, 2021 ~ Finished on April 2nd, 2021 (6 days lost to organize things)

## Sources

### Lattice Theory / Concept Lattices / FCA
- [Lattice Theory](https://search.library.oregonstate.edu/permalink/f/ueodtl/CP71109807550001451)
  - 1940, G. Birkhoff, the OG Lattice Theory book
- [Lattices, closures systems and implication bases: A survey of structural aspects and algorithms](https://www-sciencedirect-com.ezproxy.proxy.library.oregonstate.edu/science/article/pii/S0304397516306806?via%3Dihub#br0290)
  - survey of lattice theory in connection with concept lattices, 'implication bases', and the algorithms behind them
  - [SUPPORTS] provides a basis for concept lattices and lattice theory, in particular providing a definition of the closure system used in Section 2.2. Definition number 8
- [Learning Thresholds in Formal Concept Analysis](https://search.library.oregonstate.edu/permalink/f/1g9lfhc/TN_cdi_springer_books_10_1007_978_3_319_59271_8_13)
  - talks about pushing for general understanding of FCA, that 'formal concept' is a learning threshold
  - [SUPPORTS] on second glance this paper doesn't seem useful, but my notes indicate that this shows the relevance of FCA in the context of represnting the learning of new things on the boundary of existing understanding, with a 'good quote' (see todo for it, just says how learning thresholds are transformative, can't go back once you pass them, can contradict and change your thoughts on things)
- [Delving Source code with Formal Concept Analysis](https://doi.org/10.1016/j.cl.2004.11.004)
  - FAC applied to pull out concepts from source code (concrete syntax parsing however)
  - [SUPPORTS] That this has been before, (PRIOR WORK)
- [Applying Formal Concept Analysis to Cascading Style Sheets](https://www.cs.utexas.edu/users/wcook/Drafts/2010/FCAtoCSS.pdf)
  - Paper that talks about using FCA on CSS ASTs, this is relevant, finish reading tomorrow
  - they discuss elements as being the tags themselves (ids, classes, names), and the properties as being distinct key-value pairs
- [Concept Location using Formal Concept Analysis and Information Retrieval](https://search.library.oregonstate.edu/permalink/f/1g9lfhc/TN_cdi_crossref_primary_10_1145_2377656_2377660)
  - very much related work, concept location using FCA and information retrieval in source code
  - [SUPPORTS] related work that's close to what we're doing, but is instead looking at concepts as the functional purpose of a section of code, rather than the syntactic elements themselves (i.e. not targeted towards learners of the language, but rather engineers navigating a system)
- [From Reality to Programs and (Not Quite) Back Again](https://search.library.oregonstate.edu/permalink/f/1g9lfhc/TN_cdi_ieee_primary_4268244)
  - very close to what I am doing, but ties in 'real world concepts', into the code, still this is really close, I need to cite this
  - [SUPPORTS] that this has been done before in some way to support the understanding of programs for learners
- [The Role of Concepts in Program Comprehension](https://search.library.oregonstate.edu/permalink/f/1g9lfhc/TN_cdi_ieee_primary_1021348)
  - summary, talks about how program concepts factor into program comprehension
  - [SUPPORTS] Concepts are in code and abstract concepts (notions expressed semantically)
- [Assessing Modular Structure of Legacy Code Based on Mathematical Concept Theory](https://search.library.oregonstate.edu/permalink/f/1g9lfhc/TN_cdi_ieee_primary_610285)
  - Uses FCA to find concepts in old legacy code by looking at relationship between procedures & global variables to extract the concepts
  - [SUPPORTS] FCA has been used to analyze programs for some time (1997)
- [An Invitation to Knowledge Space Theory](https://search.library.oregonstate.edu/permalink/f/1g9lfhc/TN_cdi_springer_books_10_1007_978_3_319_59271_8_1)
  - connects Knowledge Space Theory and Formal Concept Analysis, specifically the educational component and learning
- [Restructuring lattice theory: an approach based on hierarchies of concepts](https://search.library.oregonstate.edu/permalink/f/1g9lfhc/TN_cdi_springer_books_10_1007_978_3_642_01815_2_23)
  - an additional reference to FCA, excellent quote: "The concept lattice can be understood as a basic answer to two fundamental questions concerning a context, namely the question of an appropriate classification of the objects and the question about the dependencies between the attributes." (very very helpful)
- [Contributions to Pattern Discovery and Formal Concept Analysis](https://hal.archives-ouvertes.fr/tel-02495263/document)
  - Mehdi Kaytoue's thesis
  - [S. O. Kuznetsov and J. Poelmans. Knowledge representation and processing with formal concept analysis.Wiley Interdisc. Rew.: Data Mining and Knowledge Discovery, 3(3):200–215, 2013.](https://www-sciencedirect-com.ezproxy.proxy.library.oregonstate.edu/science/article/pii/S0957417413002935)
    - This one is another literature survey, might be helpful there
    - "A concept is considered to be a unit of thought constituted of two parts: its extent and its intent.", yet again, heard this one before
    - Indicates ordering is (O1,A1) <= (O2,A2) iff (O1 subset O2 iff A2 subset A1)
      - suggests my partial ordering is incorrect? Well at least the parts are abundantly clear
    - and then (O1,A1) would be a subconcept of (O2,A2), and (O2,A2) is superconcept
    - Mention of obeying Armstrong rules when Implication comes in
      - A,B subset of M... A -> B if A' subset B'...hmm
      - what are Armstrong rules?
        - a set of 3 axioms, reflexivity, transitivity, and augmentation
        - these all relate to DB management stuff, and A -> B means "A functionally determines B"
- [Assisted Software Exploration using Formal Concept Analysis](https://www.scitepress.org/papers/2012/41754/41754.pdf)
 - use prolog DB to represent full AST of a Java program under analysis. But their approach looks at just classes and the relationships between classes (has-a, is-a, etc.)
 - however still very relevant
- [Aleks Knowledge Space Theory](https://www.aleks.com/about_aleks/knowledge_space_theory)
   - ALEKS KST page, this is worth looking into
   - closely related, a learning system
- [A Framework for Conceptually Modeling the Domain Knowledge of an Instructional System](https://link.springer.com/chapter/10.1007/11758525_27)
  - "used LCA (Logical Concept Analysis) to model training domain knowledge in a computer-assisted instructional environment"
  - 8 pg. text describes an approach using FCA and Logical Concept Analysis (LCA). The approach seems pretty good, but they define hand relations based on what the teacher thinks, and this may help/hurt the approach.
- [Methods of Conceptual Knowledge Processing](https://search.library.oregonstate.edu/permalink/f/1g9lfhc/TN_cdi_pascalfrancis_primary_19206583)
  - subfield, but most importantly brings up attribute exploration (which I have not had time to explore, and would be worth future work)
- [B. Ganter, R. Wille: Formal Concept Analysis: mathematical foundations. Springer, Heidelberg 1999](https://search.library.oregonstate.edu/permalink/f/ueodtl/CP71157531710001451)
  - a more recent foundational book on the topic (should have read it...)
- [Formal Concept Analysis: Foundations and Applications](https://search.library.oregonstate.edu/permalink/f/ueodtl/CP71133049210001451)
  - One of the core books with Willes
  - Extremely telling quote: "The aim and meaning of Formal Concept Analysis as mathematical theory of concepts and concept hierarchies is to support the rational communication of humans by mathematically developing appropriate conceptual structures which can be logically activated."
  - first part, helpful
    - also talks about attribute implication
    - A -> B, if whenever A(X) -> B(X)
    - same for A,B -> C if whenever A(X) & B(X) then C(X)
    - it's these attribute implications that will allow us to make judgement mistakes (leading into expanding our knowledge universe, that something new is ready to be learned?)
  - pg. 114 (Treating Incomplete Knowledge in Formal Concept Analysis)
    - for the case of (g,m) not in I, and if we know the history fully, then we state case (ii). That it is unknown whether or not object g has attribute m, but this has not been made explicit yet
    - apparently multi-valued contexts (triadic for ex.) simplify issues with this, as there are 3 cases that can pop up if you don't know your history
    - can construct an incomplete context with x,?,- hmmm...
    - okay, what the hell, Kripke semantics & Kleene semantics can be used, but Kripke is best (for compound attributes), and even modal logic can be used in this context, are you serious right now? That was not intentional to come across this, coincidence perhaps or not?...
    - okay, there are some aspects of this that may be helpful, but we're not trying to build up knowledge that we don't know about, we have a known goal, and a target...so it's very akin to this, but well structured, there are no wrong queries, and we only allow valid progressions to the goal knowledge
    - I should read this again later, it was really dense, especially the compound attribute parts, and whether I have to construct a totally different formal context.
    - but it does seem this in at least some form for attribute exploration will work...that or object exploration...and a rule system can be devised (can write up a better presentation on this as well)
- Coulet, Adrien, Florent Domenach, Mehdi Kaytoue, and Amedeo Napoli (2013). [“Using Pattern Structures for Analyzing Ontology-Based Annotations of Biomedical Data"](https://search.library.oregonstate.edu/permalink/f/1g9lfhc/TN_cdi_springer_books_10_1007_978_3_642_38317_5_5)
    - I already had this one open, going over it again
    - [THESIS] This has been used in such a way that it may be helpful for very large languages with Pattern Structures (closely related)

### ASTs and such
- [Formal Concept Analysis applied to fault localization](https://search.library.oregonstate.edu/permalink/f/1g9lfhc/TN_cdi_acm_primary_1370220)
  - connection of FCA and ASTs, only one I could find apparently
- [Case study on which relations to use for clustering-based software architecture recovery](https://search.library.oregonstate.edu/permalink/f/1g9lfhc/TN_cdi_gale_infotracacademiconefile_A499186161)
  - Case study on which relations to use for clustering-based software architecture recovery
  - Limitation with using the entire AST is it is not tractable for large languages
  - Either the language internals are not available, or the AST is tremendously huge
  - "Experience has shown that for very large systems obtaining a fully linked abstract syntax tree, or loading it in-memory for analysis is not always possible"

### Knowledge Space Theory
- [A possible future for next generation adaptive learning systems](https://search.library.oregonstate.edu/permalink/f/1g9lfhc/TN_cdi_proquest_journals_1987907659)
  - notion of adaptive tutors
  - [SUPPORTS] idea that better systems work with a user's pre-existing knowledge, rather than working on a forced track
- [Problem solving learning environments and assessment: A knowledge space theory approach](https://search.library.oregonstate.edu/permalink/f/1g9lfhc/TN_cdi_gale_infotracacademiconefile_A322439212)
  - Knowledge Space Theory in terms of problem solving and how to test for this in assessment
  - [SUPPORTS] talks about how assessment is tough (and has important quotes itself that can be used too), and makes it important for building teacher competency to this end (and this backs what we are doing, helping teachers build up understanding so that they can teach and assess properly)

### Program Construction
- [PCEX: Interactive Program Construction Examples for Learning Programming](https://dl-acm-org.ezproxy.proxy.library.oregonstate.edu/doi/abs/10.1145/3279720.3279726)
  - talks about interactive examples that build up things
  - "The innovative idea behind PCEX is to create “rich examples” that support free exploration and challenge the student", pg. 2
  - [SUPPORTS] indicating that free exploration of material is a critical part of learning programming, and that our approach supports structuring this exploration
- [Program Construction Examples in Computer Science Education: From Static Text to Adaptive and Engaging Learning Technology](https://search.library.oregonstate.edu/permalink/f/1g9lfhc/TN_cdi_eric_primary_ED596007)
  - talks about the importance of worked examples, building up knowledge
  - [SUPPORTS] indicating other work related to the significance of changing the way examples are presented based on a student's understanding, which varies over a class

### Language Learning
- [Teaching the Art of functional programming using automated grading (experience report)](https://dl-acm-org.ezproxy.proxy.library.oregonstate.edu/doi/10.1145/3341719)
  - talks about teaching OCaml in an online setting
  - [SUPPORTS] showing that an online format is more accessible (supporting my prototype approach), and has been found so in other work
- [Teaching Abstraction in Computer Science to 7th Grade Students](https://dl.acm.org/doi/10.1145/3372143)
  - talks about significance of abstraction, and observations of abstraction techniques in a classroom that focused on 'Scratch'.
  - [SUPPORTS] Notion that abstraction is a difficult concept, but a critical concept to teach in problem solving in CS
- [Studying the difference between natural and programming language corpora](https://search.library.oregonstate.edu/permalink/f/1g9lfhc/TN_cdi_gale_infotracacademiconefile_A594563350)
  - notion that natural language and programming languages share similarities, in particular the notion of native speakers vs. English language Learners, which presents an idea that non-native speakers (and coders) tend to write more repetitive stuff (use what they know...okay)
  - [SUPPORTS] the notion that limited understanding in a domain may lead to poor solutions (using multiple low-level constructs that could be replaced by fewer high-level ones, using if-then-else instead of loops for example), and that this is related to the level of understanding of the programmer in that language (syntax & what it does is important to know)
- [Does syntax highlighting help programming novices?](https://search.library.oregonstate.edu/permalink/f/1g9lfhc/TN_cdi_gale_infotracacademiconefile_A547632235)
  - indicates that this doesnot help, and is 'wasted feedback channel', could be used to help carry type-related information (or something else)
  - [SUPPORTS] We can use coloration to convey more important aspects of our langauges, namely the programs & concepts...
  - Not so sure about this one...
- [Gradually Learning Programming Supported by a Growable Programming Language](https://search.library.oregonstate.edu/permalink/f/1g9lfhc/TN_cdi_crossref_primary_10_1109_TETC_2015_2446192)
  - self-explanatory
  - [SUPPORTS] Notion that syntax is a limiting factor, which ties into how we view unique syntactic categories as concepts, and this supports our reasoning in this system that limited introduction of syntax is important, such that knowledge is built
  - "The gradual introduction of programming concepts helps the students to exclusively focus on the new concepts. This has the benefit that at any time any example the teacher shows to the students will only use those concepts that the students are acquainted with" (excellent quote), where concepts are abstract notions (control flows, expressions, functions, etc.)
  - HOWEVER, they also note that the 'need for a precise syntactic pattern' is correlated with 'let me just pass' attitudes
- [A Systematic Literature Review on Teaching and Learning Introductory Programming in Higher Education](https://search.library.oregonstate.edu/permalink/f/1g9lfhc/TN_cdi_crossref_primary_10_1109_TE_2018_2864133)
  - Talks about study of body of work that has been produced on learning CS through introductory problems
  - [SUPPORTS] that problem solving is a huge part of CS, and that learning syntax is also a difficult challenge behind motivation and problem solving
- [Expanding computer science education in schools: understanding teacher experiences and challenges](https://search.library.oregonstate.edu/permalink/f/1g9lfhc/TN_cdi_crossref_primary_10_1080_08993408_2016_1257418)
  - this suggests known issues with K-12 instructors that are teaching CS, in particular isolation, lack of adequate CS background, limited professional dev. resources
  - [SUPPORTS] lack of background & prof. dev resources are a bad combination, this seeks to provide a prof. dev. resource to build background
- [Computer Science Pedagogical Content Knowledge: Characterizing Teacher Performance](https://search.library.oregonstate.edu/permalink/f/1g9lfhc/TN_cdi_crossref_primary_10_1145_3303770)
  - talks about measuring content knowledge to help teachers out in CS, talks about identifying this knowledge in various ways, but different from what I see here
  - [SUPPORTS] they find that prior teaching experience does not influence performance, and that

### Ontologies
- [AI could constantly scan the internet for data privacy violations, a quicker, easier way to enforce compliance](https://theconversation.com/ai-could-constantly-scan-the-internet-for-data-privacy-violations-a-quicker-easier-way-to-enforce-compliance-128973)
  - not really a good source, but generally helps describing ontology
- [Logic and Ontology](https://plato.stanford.edu/entries/logic-ontology/)
  - stanford philosophy doc, particularly interested in "ontology" as "...what the most general features and relations of these things are"
- [Web Ontology Language (OWL)](https://www.w3.org/OWL/)
  - "semantic web language designed to represent rich and complex knowledge about things, groups of things, and relations between things."

### Orthogonality
- [Michael L. Scott, Programming Language Pragmatics, p. 228](https://search.library.oregonstate.edu/permalink/f/ueodtl/CP71189427280001451)
  - Ah, excellent definition of "Orthogonality" in terms of CS
  - "Orthogonality means that features can be used in any combination, the combinations all make sense, and the meaning of a given feature is consistent, regardless of the other features with which it is combined."

### Prototype Components
- [D3 Graphviz, Github](https://github.com/magjac/d3-graphviz)
  - supplies an implementation of GraphViz in the browser for converting files written in DOT into an SVG rendering of a graph

## Unused sources
- [Earthworm: Automated Decomposition Suggestion](https://dl-acm-org.ezproxy.proxy.library.oregonstate.edu/doi/epdf/10.1145/3279720.3279736)
  - not useful, but talks about decomposition tools to teach that concept
- [Some paper on teaching recursion](https://search.library.oregonstate.edu/permalink/f/1g9lfhc/TN_cdi_doaj_primary_oai_doaj_org_article_1f52d79b705445b2930b35c178722510)
  - Talks about the various ways recursion is taught
- [Jennifer & Shannon's Story Programming Paper](https://search.library.oregonstate.edu/permalink/f/1g9lfhc/TN_cdi_acm_primary_3287397)
  - no difference...so maybe I can use that?...the notion that if that doesn't make a difference, possibly the way in which the language is presented does not impact the learning?
