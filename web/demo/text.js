

function limitText(){
  'use strict';
  var limitedText;
  var originalText = document.getElementById('comments').value;
  var lastSpace = originalText.lastIndexOf(' ', 100);
  limitedText = originalText.slice(0, lastSpace);
  document.getElementById('count').value = originalText.length;
  document.getElementById('result').value = limitedText;
  return false;
}

function init(){
  'use strict';
  var theform = document.getElementById('theform');
  theForm.onsubmit = limitText;
}

window.onload = init;
