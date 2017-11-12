(function() {
    
    var GAME_WORLD_WIDTH = 200;
    var GAME_WORLD_HEIGHT = 200;

	  var canvas = document.getElementById("canvas");
    canvas.width = document.body.clientWidth; //document.width is obsolete
  	canvas.height = document.body.clientHeight; //document.height is obsolete
  	canvasW = canvas.width;
  	canvasH = canvas.height
    var ctx = canvas.getContext("2d");

    ctx.fillRect(0, 0, canvasW, canvasH);

    var socket = new WebSocket("ws://" + location.hostname+":"+location.port+"/ws");

    var player = {
      x: 0,
      y: 0
    }

    var key_codes = {
      37: 'a',
      38: 'w',
      39: 'd',
      40: 's',
      32: 'space',

      65: 'a',
      87: 'w',
      83: 's',
      68: 'd'
    };

    socket.onmessage = function(event) {
    	var data = JSON.parse(event.data);
      console.log(data);
      player.x = data.position_update.x;
      player.y = data.position_update.y;
    };

    window.onkeydown = function(event) {
      var keyCode;
      if(key_codes[event.keyCode] != undefined) {
        keyCode = key_codes[event.keyCode]
      } else {
        keyCode = "-";
      }
      var command = JSON.stringify({
        "type": "key_command",
        "key_command": {
          "key": keyCode,
          "command": "key_down"
        },
      });
      console.log(command);
      socket.send(command);
        // inputHandler.pressed_keys[inputHandler.key_codes[event.keyCode]] = true;
    };

    window.onkeyup = function(event) {
      var keyCode;
      if(key_codes[event.keyCode] != undefined) {
        keyCode = key_codes[event.keyCode]
      } else {
        keyCode = "-";
      }
      var command = JSON.stringify({
        "type": "key_command",
        "key_command": {
          "key": keyCode,
          "command": "key_up"
        },
      });
      console.log(command);
      socket.send(command);
        // inputHandler.pressed_keys[inputHandler.key_codes[event.keyCode]] = false;
    };


    window.setInterval(function() {
      ctx.fillStyle = "black";
      ctx.fillRect(0, 0, canvasW, canvasH);
      ctx.fillStyle = "white";
      ctx.fillRect(player.x, player.y, 10, 10);
    }, 1000/60)

})();
