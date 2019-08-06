import './index.html';

import { Elm } from './elm/Main.elm';
import { initializePorts } from 'elm-cloudmodel';


const app = Elm.Main.init({
  node: document.getElementById('elm')
});

initializePorts({
  onProposal: app.ports.proposal.subscribe,
  proposalResponse: app.ports.proposalResponse.send,
  receiveEvents: app.ports.receiveEvents.send,
});
