import { Elm } from '../src/Main.elm';

const app = Elm.Main.init({ node: document.getElementById('root') });

app.ports.sendDateTime.subscribe((dt) => {
    console.log("Running port");

    console.log(dt);
    console.log(typeof dt.hour);

    const posix = new Date(dt.year, dt.month - 1, dt.day, dt.hour, dt.minute);

    app.ports.receivePosixTime.send(posix);
});
