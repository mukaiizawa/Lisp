

function updateDuration(){
  'use strict';
  var now = new Date();
  var msg = 'since ' + now.getTime().toFixed(4) + ' second from epoch.(update when mouse over)';
  U.setText('output', msg);
}


window.onload = function init(){
  'use strict';
  U.addEvent(U.$('output'), 'mouseover', updateDuration);
  updateDuration();
};
