let progs = document.getElementsByClassName("bogl-code");

for(let x = 0; x < progs.length; x++) {
  progs[x].innerHTML = progs[x].innerHTML.replaceAll(/\n/gi, "<br/>");
}

let buttons = document.getElementsByClassName("button");
let selected = [];


const baseQuery = "test ("; //FindOneContaining [
let queryType = "FindOneContaining";

const queryField = document.getElementById("query-field");

const quote = a => {
  let quoted = "";
  for(let y = 0; y < a.length; y++) {
    quoted += "\"" + a[y] + "\"";
    if(y < a.length-1) {
      quoted+= ',';
    }
  }
  return quoted
}

const updateQueryField = () => {
  let quoted = "";
  for(let y = 0; y < selected.length; y++) {
    quoted += "\"" + selected[y] + "\"";
    if(y < selected.length-1) {
      quoted+= ',';
    }
  }

  // apply the selected diff
  queryField.value = baseQuery + queryType + " [" + quoted + "]" + ")";
}

const unselectAll = n => {
  let items = document.getElementsByClassName(n);
  for(let x = 0; x < items.length; x++) {
    items[x].className = n;
  }
}


// concept selection
for(let x = 0; x < buttons.length; x++) {
  buttons[x].addEventListener("click", (e) => {
    let val = buttons[x].innerHTML;
    if(buttons[x].className == "button") {
      // add
      buttons[x].className = "button selected";
      selected.push(val);

    } else {
      // remove
      buttons[x].className = "button";
      selected = selected.filter(word => word != val)

    }

    updateQueryField();
  });
}

// query mode selection
let queryButtons = document.getElementsByClassName("qbutton");
for(let x = 0; x < queryButtons.length; x++) {
  queryButtons[x].addEventListener("click", (e) => {
    let val = queryButtons[x].innerHTML;

    unselectAll("qbutton");

    if(queryButtons[x].className == "qbutton") {
      // add
      queryButtons[x].className = "qbutton selected";
      queryType = val;

    } else {
      // remove
      queryButtons[x].className = "qbutton";
      queryType = val;

    }

    updateQueryField();
  });
}

const matchFor = a => r => c => {if(a.match(r)){extractedConcepts.push(c)}}

// decomposition handler
// applies of series of RegExp to extract concepts
let extractedConcepts = []
const decompResultElm = document.getElementById("decomposition");
const decompose = p => {
  extractedConcepts = []

  // remove comments
  p = p.replaceAll(/--.+(\n|$)/g, "\n")

  //console.info(p)

  let m = matchFor(p)

  m (/^game\s+[A-Za-z_0-9]+$/m) ("Game")
  m (/^type\s+[A-Za-z_0-9]+\s+=/m) ("Type")
  m (/({([A-Za-z0-9_]+,?)+})|=\s+[A-Z]/m) ("Symbol")
  m (/{([A-Za-z0-9_]+,)+[A-Za-z0-9_]+}/m) ("Set")
  m (/Int/m) ("Int")
  m (/Bool/m) ("Bool")
  m (/\((.+,)+.+\)/m) ("Tuple")
  m (/Board/m) ("Board")
  m (/Array/m) ("Array")
  m (/[A-Za-z0-9_]+\s*&\s*{/m) ("Extended Type")
  m (/^[a-z0-9_]+\s*:\s*[A-Za-z0-9_]+\s*$/m) ("Value")
  m (/^[a-z0-9_]+\s*:\s*[^-]+->.+\s*$/m) ("Function")
  m (/^[a-z0-9_]+\s*\([^)]+\)/m) ("Parameter")
  m (/^type\s+Input/m) ("Input")
  //m (/^TESTING/m) ("Declaration")
  //m (/^TESTING/m) ("Definition")
  m (/[^+]+\+[^+]+/m) ("Addition")
  m (/[^-]+-[^->]+/m) ("Subtraction")
  m (/[^\*]+\*[^\*]+/m) ("Multiplication")
  m (/[^/]+\/[^/]+/m) ("Division")
  m (/[^%]+%[^%]+/m) ("Modulo")
  m (/let\s+[a-z0-9_]+\s*=/m) ("Let Expression")
  // bugged...needs better regex
  m (/([+\-*/%][^>].+\(.+[+\-*/%][^>])|(\(.+[+\-*/%][^>].+[+\-*/%][^>])/m) ("Parentheses (Order of Operations)")
  m (/==/m) ("Equality")
  m (/[^-]>/m) ("Greater Than")
  m (/</m) ("Less Than")
  m (/>=/m) ("Greater Than Equal To")
  m (/<=/m) ("Less Than Equal To")
  m (/\/=/m) ("Non Equality")
  m (/if\s+.+/m) ("If Then Else")
  // bugged
  m (/=\s*[a-z]+([^(]|$)/m) ("Reference")
  m (/=\s*[a-z0-9_]+([(]|$)/m) ("Func Application")
  m (/^[a-z0-9A-Z_]+!/m) ("Get")
  m (/\(\s*[a-z0-9A-Z]+\s*,\s*[a-z0-9A-Z]+\s*\)/m) ("X and Y Position")
  m (/(\s*[0-9]+\s*,)|(,\s*[0-9]+\s*)/m) ("Position Literal")
  m (/(\s*[a-zA-Z_]+\s*,)|(,\s*[a-zA-Z_]+\s*)/m) ("Position Variable")
  // ...ignore this for now
  //m (/^TESTING/m) ("Assigning a value to a Position")
  // this one needs multi-line regex instead
  m (/^TESTING/m) ("Many Board Equations")


  // for every extracted concept, show it
  let data = "Found Concepts: ";
  for(let x = 0; x < extractedConcepts.length; x++) {
    data += extractedConcepts[x];
    if(x < extractedConcepts.length - 1) {
      data += ", ";
    }
  }
  data += "<div id='use-all'>Use All</div>";
  decompResultElm.innerHTML = data;

  let useAll = document.getElementById("use-all");
  useAll.addEventListener("click", (e) => {
    // on click, auto select EACH of these values and add them
    let quoted = quote(extractedConcepts)
    queryField.value = baseQuery + queryType + " [" + quoted + "]" + ")";

    // clear 1st
    for(let q = 0; q < buttons.length; q++) {
      buttons[q].className = "button";
    }

    // then set
    for(let x = 0; x < extractedConcepts.length; x++) {
      for(let q = 0; q < buttons.length; q++) {
        if(buttons[q].innerHTML == extractedConcepts[x]) {
          buttons[q].className = "button selected";
        }
      }
    }

  })

};
const decompField = document.getElementById("program-decomp");
decompField.addEventListener("click", (e) => {
  decompose(decompField.value);
})
