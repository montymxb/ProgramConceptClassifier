let progs = document.getElementsByClassName("bogl-code");

for(let x = 0; x < progs.length; x++) {
  progs[x].innerHTML = progs[x].innerHTML.replaceAll(/\n/gi, "<br/>");
}
