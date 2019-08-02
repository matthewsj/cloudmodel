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
    console.log("received proposal", proposal);
    const latestEventId = eventStream.length;
    const { latestKnownEventId, sharedMsg, clientEventId } = proposal;
    if (latestKnownEventId === latestEventId) {
      const newEventId = latestEventId + 1;
      const newEvent = {
        id: newEventId,
        msg: sharedMsg
      };
      eventStream.push(newEvent);
      console.log("Accepted event", newEvent);
      socket.broadcast.emit('event', newEvent);
      responseFn({
        accept: {
          clientEventId,
          eventId: newEventId,
        }
      });
    } else {
      console.log("Rejected event", proposal);
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
