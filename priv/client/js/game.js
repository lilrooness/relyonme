(function() {
	var canvas = document.getElementById("canvas");
    canvas.width = document.body.clientWidth; //document.width is obsolete
  	canvas.height = document.body.clientHeight; //document.height is obsolete
  	canvasW = canvas.width;
  	canvasH = canvas.height
    var ctx = canvas.getContext("2d");

    ctx.fillRect(0, 0, canvasW, canvasH);

    var socket = new WebSocket("ws://" + location.hostname+":"+location.port+"/ws");

    socket.onmessage = function(event) {
    	console.log(event.data);
    	var data = JSON.parse(event.data);
    };

})();
