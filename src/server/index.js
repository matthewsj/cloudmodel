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

  socket.on('propose', function(proposal) {
    const latestEventId = eventStream.length;
    const { latestEventIdBelief, proposedEvent, clientEventId } = proposal;
    if (latestEventIdBelief === latestEventId) {
      const newEventId = latestEventId + 1;
      const newEvent = {
        eventId: newEventId,
        event: proposedEvent
      };
      eventStream.push(newEvent);
      socket.broadcast.emit('events', newEvent);
      socket.emit('accept', {
        clientEventId,
        eventId: newEventId,
        event: proposedEvent
      });
    } else {
      socket.emit('reject', {
        clientEventId,
        latestEventId: latestEventId,
        missingEvents: eventStream.slice(-(latestEventId - latestEventIdBelief))
      });
    }
  });
});

http.listen(3000, function() {
  console.log('listening on *:3000');
});
