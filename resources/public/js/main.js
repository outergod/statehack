;(function() {
  var socket = new WebSocket("ws://localhost:8080/socket");
  var term = new Terminal({
    cols: 80,
    rows: 24,
    useStyle: true,
    screenKeys: true,
    cursorBlink: false
  });
  term.open(document.body);
  term.on('title', function (title) {
    document.title = title;
  });

  socket.onopen = function () {
    socket.send(JSON.stringify({command: 'write', payload: 'Hello World!'}));
  };

  socket.onmessage = function (e) {
    var data = JSON.parse(e.data);
    if (data.command === 'write') {
      term.write(data.payload);
    }
  };

  socket.onclose = function () {
    term.destroy();
  };

  this.onbeforeunload = function () {
    socket.close();
  };
}).call(this);
