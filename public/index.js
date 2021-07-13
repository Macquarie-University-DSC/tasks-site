import { Elm } from '../src/Main.elm';
import { time_converter } from '../time_converter/src/lib.rs';

const app = Elm.Main.init({ node: document.getElementById('root') });

app.ports.sendDateTime.subscribe((dt) => {
    app.ports.receivePosixTime.send(time_converter(dt.year, dt.month, dt.day, dt.hour, dt.minute));
})