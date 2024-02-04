/* eslint-env browser */

let image;

const onLoadImage = (loadedImage) => {
    image = loadedImage;
    redraw();
};

window.vm.onLoadImage(onLoadImage);

function setup() {
    createCanvas(1000, 800);
    background(255);
}

const COLS = 200;
const SIZE = 5;

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
    // TODO do not clear, draw only the delta for speed
    clear();

    const upd = await window.vm.getUpdate();
    console.log(JSON.stringify(upd));

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
}
