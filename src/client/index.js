import './index.html';

import { Elm } from './elm/Main.elm';

const app = Elm.Main.init({
  node: document.getElementById('elm')
});
const socket = io({
  autoConnect: false
});

socket.on('connect', function(){
  console.log('socket connected');
});

socket.on('disconnect', function(){
  console.log('socket disconnected');
});

socket.on('catchup', (catchupMessage) => {
  const { eventStream } = catchupMessage;
  app.ports.receiveEvents.send(eventStream);
});

socket.on('event', (newEvent) => {
  console.log("Received remote event", newEvent);
  app.ports.receiveEvents.send([newEvent]);
})

// let proposedEvents = [];
app.ports.proposal.subscribe(function(event) {
  console.log("Sending proposal", event);
  // proposedEvents = proposedEvents.concat([event]);
  // console.log('proposedEvents before', proposedEvents);
  socket.emit('propose', event, (response) => {
    console.log("Received response", response);
    app.ports.proposalResponse.send(response);
    // proposedEvents = proposedEvents.filter(proposedEvent => proposedEvent.clientEventId !== event.clientEventId);
    // console.log('proposedEvents after', proposedEvents);
  });
});

socket.connect();

window.RECONNECT_SOCKET = () => {
  console.log('Reconnecting...');
  // NOTE: Socket.io already does some queing automatically. I noticed that if I disconnected using the
  // hacking mechanism commented out in the server index.js, that all I had to do was call socket.connect() again
  // and it would complete sending the `propose`. However, I'm not sure if that works with all kinds of disconnects.
  socket.connect();
  // if (proposedEvents.length < 1) {
  //   return;
  // }
  // const event = proposedEvents[0];
  // console.log('Re-proposing event', event);
  // socket.emit('propose', event, (response) => {
  //   console.log("Received response", response);
  //   app.ports.proposalResponse.send(response);
  //   proposedEvents = proposedEvents.filter(proposedEvent => proposedEvent.clientEventId !== event.clientEventId);
  //   console.log('proposedEvents after', proposedEvents);
  // });
}
