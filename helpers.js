var helpers = (function () {
  
  var badboy = {};
  
  return {
    newState : function () {
        badboy = { state: 'SAFE' };
    },
      
    readState : function () {
      return badboy.state;
    },
    
    readHealth : function () {
      return badboy.health;
    },
    
    readEnemy : function () {
      return badboy.enemy;
    },
    
    writeStateStr : function (stateStr) {
      badboy.state = stateStr;
    },
    
    writeHealth : function (health) {
      badboy.health = health;
    },
    
    writeEnemy : function (enemy) {
      badboy.enemy = enemy;
    }
  }
  
}());

function log(msg) {
  var node = document.createElement('ons-list-item');
  var text = document.createTextNode(msg);
  node.appendChild(text)
  document.querySelector('#log').appendChild(node);
}
    
function onClick(selector, callback) {
    document.querySelector(selector).addEventListener('click', callback);
}

function onInit(callback) {
    document.addEventListener('init', callback);
}

function enemyResponse() {
  return Math.floor((Math.random() * 3) + 1);
}

function showStatus(status) {
  return document.querySelector('#status').innerHTML = status;
}
