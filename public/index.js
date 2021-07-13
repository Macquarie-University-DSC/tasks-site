import { Elm } from '../src/Main.elm';


const app = Elm.Main.init({ node: document.getElementById('root') });

app.ports.sendDateTime.subscribe(async (dt) => {
    const { time_converter } = await import('../time_converter/src/lib.rs');

    app.ports.receivePosixTime.send(time_converter(dt.year, dt.month, dt.day, dt.hour, dt.minute));
});