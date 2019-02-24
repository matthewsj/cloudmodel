const express = require('express');
const app = express();
const http = require('http').Server(app);
const io = require('socket.io')(http);

app.use(express.static('dist'));

const eventStream = [];

io.on('connection', function(socket) {
  socket.emit('catchup', {
    eventStream
  });

  socket.on('propose', function(proposal, responseFn) {
    const latestEventId = eventStream.length;
    const { latestKnownEventId, sharedModelMsg, clientEventId } = proposal;
    if (latestKnownEventId === latestEventId) {
      const newEventId = latestEventId + 1;
      const newEvent = {
        id: newEventId,
        msg: sharedModelMsg
      };
      eventStream.push(newEvent);
      socket.broadcast.emit('event', newEvent);
      responseFn({
        accept: {
          clientEventId,
          eventId: newEventId,
        }
      });
    } else {
      responseFn({
        reject: {
          clientEventId,
          missingEvents: eventStream.slice(-(latestEventId - latestKnownEventId))
        }
      });
    }
  });
});

http.listen(3000, function() {
  console.log('listening on *:3000');
});
