"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
const socket_io_client_1 = __importDefault(require("socket.io-client"));
function initializePorts(appPorts) {
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
    const socket = socket_io_client_1.default();
    console.log("Initialized connection between Socket.io server and ports.");
    socket.on('catchup', (catchupMessage) => {
        console.log('Got catchup message', catchupMessage);
        const { eventStream } = catchupMessage;
        receiveEvents(eventStream);
    });
    socket.on('event', (newEvent) => {
        console.log("Received remote event", newEvent);
        receiveEvents([newEvent]);
    });
    onProposal(function (event) {
        console.log("Sending proposal", event);
        socket.emit('propose', event, (response) => {
            console.log("Received response", response);
            proposalResponse(response);
        });
    });
}
exports.initializePorts = initializePorts;
//# sourceMappingURL=index.js.map