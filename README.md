# elm-cloudmodel
This repository provides [Elm 0.19](https://elm-lang.org/) code, as well as an example Javascript client and server, that implements the "Cloudmodel Protocol". The Cloudmodel Protocol is a pattern that allows multi-client applications to share a model, even if they go offline. See this blog post [TODO] for more information about how it works and why you might use it.

# Examples
There are same examples in the [examples](https://github.com/matthewsj/cloudmodel/tree/master/examples) folder of this repository. Each has its own README for how to get started. They can also be viewed on Heroku (links are in the respective READMEs). The examples that currently exist are the following:
1. A [Chat-app](https://github.com/matthewsj/cloudmodel/tree/master/examples/chatapp). [Heroku link](https://sheltered-savannah-70764.herokuapp.com/)
2. A [Shared to-do list](https://github.com/matthewsj/cloudmodel/tree/master/examples/shared-todo). [Heroku link](https://limitless-dusk-45367.herokuapp.com/).

# Installation
**At this time, this package has not been published to either NPM or the Elm Packages.**

To install the repository, run
```sh
npm install --save matthewsj/cloudmodel
```

This will install the following files:
- [client/elm/CloudModel.elm](https://github.com/matthewsj/cloudmodel/blob/master/src/client/elm/CloudModel.elm):
  This file implements the details of the "Cloudmodel Protocol".
- [server/index.js](https://github.com/matthewsj/cloudmodel/blob/master/src/server/index.js): This implements a very
  simple server for the CloudModel protocol. It keeps the events of the "event stream" in memory as a list,
  so is not suited for production use. The websocket implementation is using socket.io. The server can be started directly by running `npx cloudmodel_serve`.
- [client/js/index.ts](https://github.com/matthewsj/cloudmodel/blob/master/src/client/js/index.ts):
  This provides the client-side socket.io hooks for talking to the server. Event names (strings) are shared
  between the client and server code. The compiled, Javascript version of this file is committed directly into the repository.

Technically, the CloudModel protocol has nothing to do with Elm, so if you so choose to, you can use only the provided
Javascript code with your application. Conversely, the provided Javascript client and server are just examples, so you can use the Elm code and write your own server/client for the CloudModel. 

## Using the provided Elm code
You'll need to update your `elm.json` file so that the compiler can find CloudModel.elm. This can be done by adding `"node_modules/elm-cloudmodel/src/client/elm"` to the `"source-directories"` key:

```json
{
    ...
    "source-directories": [
        "YOUR_SRC_DIRECTORIES",
        "node_modules/elm-cloudmodel/src/client/elm"
    ],
    ...
}
```

This should allow you to `import CloudModel` in your `Main.elm` file (or whatever your application is called).

This module exposes a method `CloudModel.element`, which is the equivalent of `Browser.element` for a `CloudModel`-based program. That method takes a `CloudModelConfig`. Where normal Elm applications are built around
a `Model` type and a `Msg` type, cloud model based applications have both a `SharedModel` and a
`LocalModel`, and corresponding `SharedMsg` and `LocalMsg`s that interact with them. See the Chat-app example's [Main.elm](https://github.com/matthewsj/cloudmodel/blob/master/examples/chatapp/src/client/elm/Main.elm) for a reference implementation.

### Advanced Usage
For users with special needs, for instance for composing multiple large framework libraries,
`wrapAll` takes a `CloudModel` config and produces standard Elm architecture functions.

## Using the provided Javascript code
If you want to use the provided client code for interacting with the provided stub server, you need to inform
the provided client code of the relevant ports into your application. In the Javascript file where you initialize
your application, you'll need to do the following:

```js
import { initializePorts } from 'elm-cloudmodel';

// Assuming you have an Elm application and installed it with the provided instructions.
const app = Elm.Main.init({
  node: document.getElementById('elm')
});

initializePorts({
  onProposal: app.ports.proposal.subscribe,
  proposalResponse: app.ports.proposalResponse.send,
  receiveEvents: app.ports.receiveEvents.send,
});
```

`onProposal` should be called by your application whenever it wants to propose an event to the server.
`proposalResponse` will inform your application of whether the server accepted or rejected the proposed event.
`receiveEvents`: will inform your application of any events sent by other clients. It is called whenever another
client has an event accepted by the server and on first connect with the entire event stream up to the present.

See [index.ts](https://github.com/matthewsj/cloudmodel/blob/master/src/client/js/index.ts) for the types of each method.

The server does not need to be integrated into anything. It can be started directly with `npx cloudmodel_serve`.

## Credits

* The client and server code are based off of the socket.io [Getting Started guide](https://socket.io/get-started/chat/).
* Build system is based on [crsandeep/simple-react-full-stack](https://github.com/crsandeep/simple-react-full-stack).
