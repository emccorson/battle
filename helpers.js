/* Holds all the functions that can manipulate game state */
var gameState = (function () {
  
  /* The object that holds the game state */
  var state = {};
  
  return {
    newState : function () {
        state = { state: 'SAFE' };
    },
      
    readState : function () {
      return state.state;
    },
    
    readHealth : function () {
      return state.health;
    },
    
    readEnemy : function () {
      return state.enemy;
    },
    
    writeStateStr : function (stateStr) {
      state.state = stateStr;
    },
    
    writeHealth : function (health) {
      state.health = health;
    },
    
    writeEnemy : function (enemy) {
      state.enemy = enemy;
    }
  }
  
}());

/* Adds a new item to the end of the log list */
function log(msg) {
  var node = document.createElement('ons-list-item');
  var text = document.createTextNode(msg);
  node.appendChild(text)
  document.querySelector('#log').appendChild(node);
}

/* Adds a click event listener */
function onClick(selector, callback) {
    document.querySelector(selector).addEventListener('click', callback);
}

/* Does something on init event */
function onInit(callback) {
    document.addEventListener('init', callback);
}

/* Returns a random integer representing the move the enemy used.
 * The integer is interpreted by the Idris wrapper function. */
function enemyResponse() {
  return Math.floor((Math.random() * 3) + 1);
}

/* Updates the status bar at the top of the UI. */
function showStatus(status) {
  return document.querySelector('#status').innerHTML = status;
}
