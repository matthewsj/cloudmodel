# Cloudmodel Chat-app
This example is a fully-working implementation of the
chat system from socket.io's [Getting Started guide](https://socket.io/get-started/chat/),
but instead of rendering with jQuery, it uses Elm plus a small JavaScript
wrapper that passes websocket events to and from Elm using ports. It also
includes an example build system based on [npm](https://www.npmjs.com/) and
[webpack](https://webpack.js.org/) using
[elm-webpack-loader](https://github.com/elm-community/elm-webpack-loader) and
[babel](https://babeljs.io/).

A working version is visible on Heroku [here](https://sheltered-savannah-70764.herokuapp.com/).

# Running locally
You need to have already installed Node.js on your system.
From a terminal run the following:

```
$ git clone https://github.com/matthewsj/cloudmodel.git
$ cd elm-socketio/examples/chatapp
$ npm install
$ npm run dev
```

This should start an express server that handles websockets and open a browser
window on localhost port 4000 with the chat app. Open the same URL multiple times
and you'll see that messages sent from one client are broadcast to all of them.

To run a production build locally,

```
$ npm run build
$ npm run start
```

This will start a server on localhost:3000 that serves the client assets at `/`
and handles websocket connections for them.

# Deploying to Heroku
Run the following from the root of the repository.
```sh
git push HEROKU_GIT_REMOTE_NAME `git subtree split --prefix examples/chatapp master`:master --force
```

If you want to push a branch, rather than master, you can change the `master` inside of the subtree command to the branch name you want to push.

Heroku runs `npm run build` on any push, so the `Procfile` instructs the server
to run only `npm run heroku_serve` to start up.
