(function() {
	var canvas = document.getElementById("canvas");
    canvas.width = document.body.clientWidth; //document.width is obsolete
  	canvas.height = document.body.clientHeight; //document.height is obsolete
  	canvasW = canvas.width;
  	canvasH = canvas.height
    var ctx = canvas.getContext("2d");

    ctx.fillRect(0, 0, canvasW, canvasH);

    var socket = new WebSocket("ws://" + location.hostname+":"+location.port+"/ws");

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
    	console.log(event.data);
    	var data = JSON.parse(event.data);
    };

    window.onkeydown = function(event) {
      var command = JSON.stringify({
        "type": "key_command",
        "key_command": {
          "key": "w",//key_codes[event.key_code],
          "command": "key_down"
        },
      });
      socket.send(command);
        // inputHandler.pressed_keys[inputHandler.key_codes[event.keyCode]] = true;
    };

    window.onkeyup = function(event) {
      var command = JSON.stringify({
        "type": "key_command",
        "key_command": {
          "key": "w",//key_codes[event.key_code],
          "command": "key_up"
        },
      });
      socket.send(command);
        // inputHandler.pressed_keys[inputHandler.key_codes[event.keyCode]] = false;
    };

})();
