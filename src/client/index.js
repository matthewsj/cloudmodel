import './index.html';

import { Elm } from './elm/Main.elm';

const app = Elm.Main.init({
  node: document.getElementById('elm')
});
const socket = io();

socket.on('catchup', (catchupMessage) => {
  const { eventStream } = catchupMessage;
  app.ports.receiveEvents.send(eventStream);
});

socket.on('event', (newEvent) => {
  app.ports.receiveEvents.send([newEvent]);
})

app.ports.proposal.subscribe(function(event) {
  console.log("Sending proposal", event);
  socket.emit('propose', event, (response) => {
    console.log("Received response", response);
    app.ports.proposalResponse.send(response);
  });
});
