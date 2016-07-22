
function calculate(){
  'use strict';
  var total;
  var quantity = document.getElementById('quantity').value;
  var price = document.getElementById('price').value;
  var tax = document.getElementById('tax').value;
  var discount = document.getElementById('discount').value;
  total = (quantity * price) * ((tax / 100) + 1) - discount;
  document.getElementById('total').value = total.toFixed();
  return false;
}

function init(){
  'use strict';
  var theform = document.getElementById('theform');
  theForm.onsubmit = calculate;
}

window.onload = init;
