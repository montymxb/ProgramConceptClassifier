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
