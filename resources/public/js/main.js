;(function() {
  var socket = new WebSocket('ws://' + this.location.host + '/socket');
  var term = new Terminal({
    cols: 80,
    rows: 24,
    useStyle: true,
    cursorBlink: false
  });
  term.open(document.body);
  term.on('title', function (title) {
    document.title = title;
  });
  
  socket.addEventListener('open', function () {
    term.on('data', function (queue) {
      socket.send(queue);
    });
  });

  socket.addEventListener('message', function (e) {
    term.write(e.data);
  });

  socket.addEventListener('close', function () {
    term.destroy();
  });
  
  this.addEventListener('beforeunload', function () {
    socket.close();
  });
}).call(this);
