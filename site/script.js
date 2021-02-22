
window.onload = function () {

  // update code divs
  let codeDivs = document.getElementsByClassName("code");
  for(let x = 0; x < codeDivs.length; x++) {
    let cdn = codeDivs[x].innerHTML.replaceAll(/\n/gi,"<br/>");
    codeDivs[x].innerHTML = cdn;
  }
}
