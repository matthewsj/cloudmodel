import io from 'socket.io-client';

/*
 * TODOs:
 * 1. Give clearer type definitions to `initializePorts`
 * 2. Create a type definition for an Event
 * 3. Type definition for acceptance event
 * 4. Type definition for rejection event
 * 5. Typescript?
 * 6. Share strings between client and server?
 */

/**
 * Takes an object mapping keys to the required ports of an elm app, and wires
 * up those ports to talk to the server using socket.io sockets.
 *
 * The outgoing port is:
 * - `proposal`: This is called from Elm when the application wants to propose
 *   a new event stream event to the server. The only argument is an event
 * 
 * The incoming ports are:
 * - `receiveEvents`: This is called whenever the server
 *   sends events to the client that originated from a different client. It is
 *   sent a single parameter which is an array of events
 * - `proposalResponse`: This port is called when the server responds to
 *   a proposal from the application. It can either be an acceptance response,
 *   or a rejection response.
 */
export function initializePorts(elmAppPorts) {  
  validatePortsOrThrow(elmAppPorts);
  const socket = io();
  console.debug("Initialized connection between Socket.io server and Elm app ports.")

  socket.on('catchup', (catchupMessage) => {
    const { eventStream } = catchupMessage;
    elmAppPorts.receiveEvents.send(eventStream);
  });

  socket.on('event', (newEvent) => {
    console.log("Received remote event", newEvent);
    elmAppPorts.receiveEvents.send([newEvent]);
  })

  elmAppPorts.proposal.subscribe(function (event) {
    console.log("Sending proposal", event);
    socket.emit('propose', event, (response) => {
      console.log("Received response", response);
      elmAppPorts.proposalResponse.send(response);
    });
  });
}

function validatePortsOrThrow(elmAppPorts) {
  if (!elmAppPorts.hasOwnProperty('receiveEvents')) {
    throw new Error("The set of elm ports must include a port for `receiveEvents`.");
  }
  if (!elmAppPorts.hasOwnProperty('proposal')) {
    throw new Error("The set of elm ports must include a port for `proposal`.");
  }
  if (!elmAppPorts.hasOwnProperty('proposalResponse')) {
    throw new Error("The set of elm ports must include a port for `proposalResponse`.");
  }
}

