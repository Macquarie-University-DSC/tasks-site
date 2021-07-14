import { Elm } from '../src/Main.elm';


const app = Elm.Main.init({ node: document.getElementById('root') });

app.ports.sendDateTime.subscribe(async (dt) => {
    const { time_converter } = await import('../time_converter/src/lib.rs');

    const posix = time_converter(dt.year, dt.month, dt.day, dt.hour, dt.minute);
    console.log(posix);
    app.ports.receivePosixTime.send(posix);
});