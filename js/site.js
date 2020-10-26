let progs = document.getElementsByClassName("bogl-code");

for(let x = 0; x < progs.length; x++) {
  progs[x].innerHTML = progs[x].innerHTML.replaceAll(/\n/gi, "<br/>");
}

let buttons = document.getElementsByClassName("button");
let selected = [];


const baseQuery = "test ("; //FindOneContaining [
let queryType = "FindOneContaining";

const queryField = document.getElementById("query-field");

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
