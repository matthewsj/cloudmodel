import io from 'socket.io-client';

interface RemoteEvent<T> {
  id: number;
  msg: T;
}

interface LocalEvent<T> {
  clientEventId: number;
  latestKnownEventId: number;
  sharedMsg: T;
}

interface CatchupMessage<T> {
  eventStream: Array<RemoteEvent<T>>;
}

interface ProposalResponse<T> {
  accept?: {
    clientEventId: number;
    eventId: number;
  };
  reject?: {
    clientEventId: number;
    missingEvents: Array<RemoteEvent<T>>;
  };
}

interface AppPorts<Msg> {
  onProposal(cb: (localEvent: LocalEvent<Msg>) => void): void;
  proposalResponse(response: ProposalResponse<Msg>): void;
  receiveEvents(events: Array<RemoteEvent<Msg>>): void;
}

/**
 * This method initializes the relationship between the frontend application and
 * the websocket connection that manages the "event stream" protocol.
 * The protocol is described in more detail in the README.
 * 
 * It requires two incoming ports:
 * - `receiveEvents`: This is called whenever the server
 *   sends events to the client that originated from a different client.
 * - `proposalResponse`: This port is called when the server responds to
 *   a proposal from the application.
 * 
 * And one outgoing port:
 * - `onProposal`: This is called when the frontend application wants to propose
 *   a new event to the stream event.
 */
export function initializePorts<Msg>(appPorts: Partial<AppPorts<Msg>>) {
  const { onProposal, receiveEvents, proposalResponse } = appPorts;
  if (!receiveEvents) {
    throw new Error("The set of ports must include a port for `receiveEvents`.");
  }
  if (!onProposal) {
    throw new Error("The set of ports must include a port for `proposal`.");
  }
  if (!proposalResponse) {
    throw new Error("The set of ports must include a port for `proposalResponse`.");
  }

  const socket = io();
  console.log("Initialized connection between Socket.io server and ports.")

  socket.on('catchup', (catchupMessage: CatchupMessage<Msg>) => {
    console.log('Got catchup message', catchupMessage)
    const { eventStream } = catchupMessage;
    receiveEvents(eventStream);
  });

  socket.on('event', (newEvent: RemoteEvent<Msg>) => {
    console.log("Received remote event", newEvent);
    receiveEvents([newEvent]);
  })

  onProposal(function (event: LocalEvent<Msg>) {
    console.log("Sending proposal", event);
    socket.emit('propose', event, (response: ProposalResponse<Msg>) => {
      console.log("Received response", response);
      proposalResponse(response);
    });
  });
}
