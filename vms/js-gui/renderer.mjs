/* eslint-env browser */

// TODO fade the grid slowly depending on how far in the past did the read/write event happen

let image;

const onLoadImage = (loadedImage) => {
    //console.error(loadedImage);
    image = loadedImage;
};

window.vm.onLoadImage(onLoadImage);

let lastUpdate;

const getUpdate = async () => {
    const upd = await window.vm.getUpdate();
    //console.log(JSON.stringify(upd));

    if (lastUpdate && Object.keys(upd.events).length === 0) {
        return lastUpdate;
    }

    return upd;
};

function setup() {
    createCanvas(1400, 800);
    background(255);
}

const COLS = 200;
const SIZE = 7;

const getFill = (upd, r, c) => {
    const addr = r * COLS + c;

    switch (upd.events[addr]) {
        case undefined: return 'white';
        case 'r': return 'lightgreen';
        case 'w': return 'orange';
    }
};

const getStroke = (upd, r, c) => {
    const addr = r * COLS + c;

    // TODO fix stack handling
    // if (addr >= upd.rb && addr <= image?.stack) {
    //     return 'pink';
    // } else 
    if (addr < image?.size) {
        return 'lightblue';
    } else {
        return 'lightgray';
    }
};

const addrToRowCol = (addr) => [Math.floor(addr / COLS), addr % COLS];

const squareForRowCol = (r, c) => square(SIZE + c * SIZE, SIZE + r * SIZE, SIZE);

async function draw() {
    // TODO draw only the delta for speed

    const upd = await getUpdate();

    clear();

    const rows = upd.size ? Math.ceil(upd.size / COLS) : 100;

    for (let r = 0; r < rows; r++) {
        for (let c = 0; c < COLS; c++) {
            stroke(getStroke(upd, r, c));
            fill(getFill(upd, r, c));
            squareForRowCol(r, c);
        }
    }

    const [ipr, ipc] = addrToRowCol(upd.ip);
    noFill();
    stroke('red');
    squareForRowCol(ipr, ipc);

    if (upd.ip !== upd.rb) {
        const [rbr, rbc] = addrToRowCol(upd.rb);
        noFill();
        stroke('blue');
        squareForRowCol(rbr, rbc);
    }
}
