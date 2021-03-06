import './main.css';
import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';
import * as hpccWasm from '@hpcc-js/wasm';

const app = Elm.Main.init({
  node: document.getElementById('root'),
});

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();

hpccWasm.wasmFolder('https://unpkg.com/@hpcc-js/wasm/dist/');

app.ports.dot.subscribe((data) => {
  hpccWasm.graphviz.layout(data, 'svg', 'dot').then((svg) => {
    app.ports.updateSVG.send(svg);
  });
});
