import './index.html';

import { Elm } from './elm/Main.elm';
import { initializePorts } from 'elm-cloudmodel';


const app = Elm.Main.init({
  node: document.getElementById('elm')
});

initializePorts({
  proposal: app.ports.proposal,
  proposalResponse: app.ports.proposalResponse,
  receiveEvents: app.ports.receiveEvents,
});
