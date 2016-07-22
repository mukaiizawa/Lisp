

function calculate(){
  'use strict';
  var volume;
  var radius = document.getElementById('radius').value;
  volume = (parseInt(radius) && radius>0)?
    ((4/3)*Math.PI*Math.pow(parseInt(radius), 3)).toFixed(4):
    'invalid radius';
  document.getElementById('volume').value = volume;
  return false;
}

function init(){
  'use strict';
  var theform = document.getElementById('theform');
  theForm.onsubmit = calculate;
}

window.onload = init;
