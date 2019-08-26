# Cloudmodel Shared to-do list
This is a fully working example of a "shared to-do list." It is a heavily modified version of
Evan Czaplicki's [Elm TODO-MNC](https://github.com/evancz/elm-todomvc), meant to demonstrate
the Cloudmodel protocol and why it might be useful. In addition to the original functionality
of the TODO-MVC, it allows TODOs to be claimed, and the list to be filtered by owner. Functionality
is still a work-in progress.

Example is visible [here](https://limitless-dusk-45367.herokuapp.com/).
Unfortunately, there is not currently a way to clear the TODOs other than to re-deploy / reset the server.

# Running locally
You need to have already installed Node.js on your system.
From a terminal run the following:

```
$ git clone https://github.com/matthewsj/cloudmodel.git
$ cd elm-socketio/examples/shared-todo
$ npm install
$ npm run dev
```

This should start an express server that handles websockets and open a browser
window on localhost port 4000 with the shared to-do list. Open the same URL multiple times
and you'll see that any changes to the list made on one client are
sent from one client are broadcast to all of them.

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
git push HEROKU_GIT_REMOTE_NAME `git subtree split --prefix examples/shared-todo master`:master --force
```

If you want to push a branch, rather than master, you can change the `master` inside of the subtree command to the branch name you want to push.

Heroku runs `npm run build` on any push, so the `Procfile` instructs the server
to run only `npm run heroku_serve` to start up.


# Remaining stuff to be done
- [ ] Build proper "User / Owner" management
- [ ] Improve styling of owner's filter
- [ ] Clean up CSS and maybe migrate to SCSS?
- [ ] Allowing "claiming" todos on mobile
- [ ] Make it so that only the current owner can complete the task
- [ ] With filtering by owner, make the current user's filter say "Mine", and be first in the list.
