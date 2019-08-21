#!/usr/bin/env node

const express = require('express');
const app = express();
const http = require('http').Server(app);
const io = require('socket.io')(http);
const fs = require('fs');
const path = require('path');
const yargs = require('yargs');

const parsedArgs = yargs
  .option('port', {
    alias: 'p',
    default: 3000,
    demandOption: true,
    description: 'The port on which to serve',
    type: 'number',
  })
  .option('static_dir', {
    alias: 'd',
    description: 'The directory of the static frontend asset to serve. If none is provided, no asset will be served.',
    type: 'string',
  })
  .check(function (argv, options) {
    if (!argv.static_dir) {
      return true;
    }
    const staticDir = computeAbsServingDirectory(argv.static_dir);
    if (!fs.existsSync(staticDir)) {
      throw new Error(`Cannot find assets at ${staticDir}`);
    }
    return true;
  })
  .argv;

if (parsedArgs.static_dir) {
  const staticDir = computeAbsServingDirectory(parsedArgs.static_dir);
  app.use(express.static(staticDir));
}

const eventStream = [];

io.on('connection', function (socket) {
  socket.emit('catchup', {
    eventStream
  });

  socket.on('propose', function (proposal, responseFn) {
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

http.listen(parsedArgs.port, function () {
  if (parsedArgs.static_dir) {
    console.log(`Serving from ${parsedArgs.static_dir} listening on port *:${parsedArgs.port}`);
  } else {
    console.log(`Socket server is listening on port *:${parsedArgs.port}`)
  }
});

function computeAbsServingDirectory(staticDir) {
  return path.join(process.cwd(), staticDir);
}
