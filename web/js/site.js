"use strict";

//
// Helpers
//
// Crude 'maybe'...does not handle bind/fmap/joining correctly yet...
// for any undefined & null data type, could easily write
// this to take a function 'f', and pass it a Just & Nothig construct...gives
// the function the ability to define it's own callbacks
const maybe = v => {
  return {
    // returns a val if there is one, else an exception
    // used in the linear context of the program's execution
    // assumes everything goes right, error handling could be implemented, but not yet
    get: (nothing) => {
      if(v !== undefined && v !== null) {
        // Just v
        return v

      } else if(nothing) {
        // Nothing handler
        nothing()
        return undefined

      } else {
        // no handler for nothing, cannot account for this in the standard program flow
        // and must throw an error
        throw ReferenceError("'get' called on Maybe to extract value of undefined or null")
      }
    },

    join: () => {
      return v.map(x => maybe(x).get(() => {})).filter(x => x !== undefined)
    },

    fmap: (f, nothing) =>  {
      return maybe(f(maybe(v).get(nothing)))
    },

    bind: (f, nothing) => {
      return f(maybe(v).get(nothing))
    }
  }
}

const maybeElmById = id => maybe(document.getElementById(id));
const maybeElmsByClassName = cn => maybe(document.getElementsByClassName(cn));

//
// Primary stuff
//

const queryConstraints = {
  'Match to Concepts in Program Only' : 1,
  'Match all Concepts To Understand This Program': 2
  /*
  'FindAllExact' : 2,
  'FindOneContaining' : 3,
  'FindAllContaining': 4,
  'FindOneAny': 5,
  'FindAllAny': 6,
  'FindInOrder': 7
  */
}

const buildConcept = concept => patt => ex => sub => {
  return {
    "concept": concept,
    "patt"   : patt,
    "ex"     : ex,
    "sub"    : sub
  }
}

const cGame = buildConcept ("Game") (/(game\s+[A-Z][A-Za-z_0-9]*)/m) ("game Example")

const cType = buildConcept ("Type") (undefined) ("")
const cInt = buildConcept ("Int") (/Int/m)
const cBool = buildConcept ("Bool") (/Bool/m)
const cSymbol = buildConcept ("Symbol") (/\{|\}/)
const cTuple = buildConcept ("Tuple") (/\((.+,)+.+\)/m)

const cLit = buildConcept ("Literal") (undefined) ("")



const cBinOps = buildConcept ("Bin Ops") (undefined) ("")
const cAdd = buildConcept ("Addition") (/[^+]+\+[^+]+/m)
const cSub = buildConcept ("Subtraction") (/[^-]+-[^->]+/m)
const cMul = buildConcept ("Multiplication") (/[^\*]+\*[^\*]+/m)
const cDiv = buildConcept ("Division") (/[a-zA-Z0-9)]\s?[/][^/]+/m)
const cMod = buildConcept ("Modulo") (/[^%]+%[^%]+/m)

//  broken regex, needs fixing
const cVal = buildConcept ("Value") (/:/m) ("ex : Int\nex = 1")
const cFunc = buildConcept ("Function") (/->/m) ("identity : Int -> Int\nidentity(x) = x")
const cParam = buildConcept ("Param") (/[a-z0-9_]+\s*\([^)]+\)/m) ("")

const cExprs = buildConcept ("Expressions") (undefined) ("")
const cIfThenElse = buildConcept ("If Then Else") (/if\s+.+/m)
const cLetExpr = buildConcept ("Let") (/let\s+[a-z0-9_]+\s*=/m)
const cWhile = buildConcept ("While") (/while\s+/m)

const cInput = buildConcept ("Input") (/Input/m) ("type Input = Int")
const cBoard = buildConcept ("Board/Array") (/Board/m) ("type Board = Array(1,1) of Int")

// TODO, take a moment to think about how the constraints will factor in
//
// Q1: If I give a program that shows function application, I want it to to show everything before that if I've selected a constraint that enforces completeness
// Q2: Display the relative group 'ordering' above, items from the same category should get recognition


// Q3: Yeah, basically improve the constraint approach, and actually use it
// Could simplify constraints to 'Similar Examples' and 'Complete Concepts'
// Allow 'manually' toggling of concepts on/off ???
const pattConceptProgMapping = [
  cGame([

    cBoard([]),
    cInput([]),

    cType([
        cInt ("") ([]),
        cBool ("") ([]),
        cSymbol ("type ABC = {A,B,C}") ([]),
        cTuple ("type Tup = (Int,Int)") ([])
      ]),
    cVal([
      cLit([
        cInt ("ival : Int\nival = 24") ([]),
        cBool ("bval : Bool\nbval = True") ([]),
        cSymbol ("sval : ABC\nsval = B") ([]),
        cTuple ("tval : Tup\ntval = (2,4)") ([]),
      ]),
      cBinOps([
        cAdd ("twoPlus2 : Int\ntwoPlus2 = 2 + 2") ([]),
        cSub ("tenMinus3 : Int\ntenMinus3 = 10 - 3") ([]),
        cMul ("vmul : Int\nvmul = 3 * 3") ([]),
        cDiv ("vdiv : Int\nvdiv = 9 / 3") ([]),
        cMod ("vmod : Int\nvmod = 5 % 2") ([])
      ]),
      cExprs([
        cIfThenElse ("isZero : Int\nisZero = if 10 > 0 then True else False") ([]),
        cLetExpr ("mult2 : Int\nmult2 = let x = 5 in let y = 2 in x * y") ([]),
        cWhile ("countup : Int\n let x = 0 in while x < 10 do x + 1") ([])
      ])
    ]),

    cFunc([
      cParam([]),
      cLit([
        cInt ("ifun : Int -> Int\nifun(x) = x + 1") ([]),
        cBool ("bfun : Bool -> Bool\nbfun(x) = x") ([]),
        cSymbol ("sfun : ABC -> ABC\nsfun(x) = x") ([]),
        cTuple ("tfun : Int -> Tup\ntfun(x) = (x,x+1)") ([]),
      ]),
      cBinOps([
        cAdd ("increment : Int -> Int\nincrement(x) = x + 1") ([]),
        cSub ("decrement : Int -> Int\ndecrement(x) = x - 1") ([]),
        cMul ("double : Int -> Int\ndouble(x) = x * 2") ([]),
        cDiv ("halve : Int -> Int\nhalve(x) = x / 2") ([]),
        cMod ("evenOdd : Int -> Int\nevenOdd(x) = x % 2") ([])
      ]),
      cExprs([
        cIfThenElse ("positive : Int -> Bool\npositive(x) = if x > 0 then True else False") ([]),
        cLetExpr ("letEx : Int -> Int\nletEx(x) = let y = 2 in x * y") ([]),
        cWhile ("toZero : Int -> Int\ntoZoro(x) = while x > 0 do x - 1") ([])
      ])
    ]),
  ])
]


const matchFor = a => r => c => (a.match(r) ? c : undefined)

let discoveryOrder = 0
const traverseAndFindConcepts = (p, mapping) => {
  const constraint = maybeElmById("constraint-select").get()
  let concepts = []
  mapping.forEach(x => {
    let didCheck = false
    if(x["patt"]) {
      // exists, check it
      if(matchFor (p) (x["patt"]) (x["concept"])) {
        // match, push & check sub-concepts
        x["discoveryOrder"] = discoveryOrder++
        concepts.push(x)
        let subConcepts =  traverseAndFindConcepts(p, x["sub"])
        concepts = concepts.concat(subConcepts)

      } else if(constraint.value == 1) {
        let subConcepts =  traverseAndFindConcepts(p, x["sub"])
        if(subConcepts.length > 0) {
          // concepts on the way, safe to add
          x["discoveryOrder"] = discoveryOrder++
          concepts.push(x)
        }
        concepts = concepts.concat(subConcepts)

      }

    } else {
      // does not exist, okay to check sub concepts
      let subConcepts =  traverseAndFindConcepts(p, x["sub"])
      if(subConcepts.length > 0 && !x["patt"]) {
        // this is a wrapper concept, add it first
        x["discoveryOrder"] = discoveryOrder++
        concepts.push(x)
      }
      concepts = concepts.concat(subConcepts)

    }
  })
  return concepts
}


// extracting of BoGL concepts from regex parsing of concrete syntax
const extractConcepts = (p) => {
  // remove comments
  p = p.replaceAll(/--.+(\n|$)/g, "\n")
  p = p.replaceAll(/&gt;/g, ">")
  p = p.replaceAll(/&gl;/g, "<")

  discoveryOrder = 0
  let concepts = traverseAndFindConcepts(p, pattConceptProgMapping)

  // get non-null concepts we're interested in
  concepts = maybe(concepts).join()

  // filter dups
  concepts = [...new Set(concepts)]

  //concepts.sort((a,b) => a["discoveryOrder"] > b["discoveryOrder"])

  return concepts
}


const genConceptButton = concept => {
  return "<div class='concept'>"+concept+"</div>";
}


const addConceptsInTreeOrder = (concepts) => (tree) => {
  let content = ""
  tree.forEach(e => {

    if(concepts.some(x => e["concept"] == x)) {
      // show this concept
      content += "<div class='tree-branch'>" + genConceptButton(e["concept"]) + "</div>";

    }

    // check sub nodes
    let subContent = addConceptsInTreeOrder(concepts) (e["sub"])

    if(subContent.length > 0) {
      // add, but nested
      content += "<div class='tree-branch'>" + subContent + "</div>";

    }

  })

  return content
}


const addConceptsToView = (concepts,conceptsElm) => {
  let content = "";
  concepts = [...new Set(concepts)]

  // build these up in a nested fashion as we find them...
  /*
  concepts.forEach(x => {
    content += genConceptButton(x)
  })
  */
  content = addConceptsInTreeOrder(concepts) (pattConceptProgMapping)

  conceptsElm.innerHTML = content
}


const buildDecomposition = (conceptExs, decompElm) => {
  conceptExs = conceptExs.filter(x => x !== "")
  decompElm.innerHTML = conceptExs.join("<br/><br/>").replaceAll(/\n/g,'<br/>')
}


const ProgDecomp = () => {
  // get access to the 4 sections
  const conceptsElm     = maybeElmById("concepts").get()
  const constraintsElm  = maybeElmById("constraints").get()
  const programElm      = maybeElmById("program").get()
  const decompElm       = maybeElmById("decomp").get()

  // get the concepts from the program text
  const concepts = extractConcepts(programElm.innerHTML)

  // place these concepts in the 'concepts' window above
  addConceptsToView(concepts.map(x => x["concept"]), conceptsElm)

  // 6. based on the concepts available, and their associated ordering in the JSON, extract the examples, and build the program
  // based on the current value of the selector
  buildDecomposition(concepts.map(x => x["ex"]), decompElm)

  // establish highlighting patterns for these aspects (and highlight the matches on the left with the same color as well, for debugging purposes)

};


const addConstraints = () => {
  const constraintsElm  = maybeElmById("constraints").get()

  const keys = Object.keys(queryConstraints)

  let select = "<select id='constraint-select' name='constraint-name' onChange='ProgDecomp();'>";

  keys.map((v,i) => {
    select += "<option value='" + i + "'>" + v + "</option>"
  })

  select+="</select>";
  constraintsElm.innerHTML = select;

}


window.onload = (e) => {

  // load in constraints
  addConstraints();

  ProgDecomp();
  const programElm = maybeElmById("program").get();
  programElm.addEventListener('keydown', (e) => {
    //console.info("CHANGED")
    //e.preventDefault()
    ProgDecomp()
  })
}
