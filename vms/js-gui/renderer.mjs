/* eslint-env browser */

// TODO fade the grid slowly depending on how far in the past did the read/write event happen

let image;

const onLoadImage = (loadedImage) => {
    //console.error(loadedImage);
    image = loadedImage;
};

window.vm.onLoadImage(onLoadImage);

let state;
const MAX_AGE = {
    reads: 5000,
    writes: 50000
};

const processStateUpdate = async () => {
    const oldState = state;
    state = await window.vm.getUpdate();

    for (const type of ['reads', 'writes']) {
        for (const addr in oldState?.[type] ?? []) {
            const newCycle = state[type][addr];
            const oldCycle = oldState[type][addr];

            if (newCycle === undefined && state.cycle - oldCycle <= MAX_AGE[type]) {
                state[type][addr] = oldCycle;
            }
        }
    }
};

function setup() {
    createCanvas(1400, 800);
    background(255);
}

const COLS = 200;
const SIZE = 7;

const getFill = (r, c) => {
    const addr = r * COLS + c;

    if (state.writes[addr] !== undefined) {
        const color = color('orange');
        color.setAlpha(255 - 255 * (state.cycle - state.writes[addr]) / MAX_AGE.writes);
        return color;
    } else if (state.reads[addr] !== undefined) {
        const color = color('lightgreen');
        color.setAlpha(255 - 255 * (state.cycle - state.reads[addr]) / MAX_AGE.reads);
        return color;
    } else {
        return 'white';
    }
};

const getStroke = (r, c) => {
    const addr = r * COLS + c;

    // TODO fix stack handling
    // if (addr >= state.rb && addr <= image?.stack) {
    //     return 'pink';
    // } else 
    if (addr < image?.image.length) {
        return 'lightblue';
    } else {
        return 'lightgray';
    }
};

const addrToRowCol = (addr) => [Math.floor(addr / COLS), addr % COLS];

const squareForRowCol = (r, c) => square(SIZE + c * SIZE, SIZE + r * SIZE, SIZE);

async function draw() {
    // TODO draw only the delta for speed

    await processStateUpdate();

    clear();

    const rows = state.size ? Math.ceil(state.size / COLS) : 100;

    for (let r = 0; r < rows; r++) {
        for (let c = 0; c < COLS; c++) {
            stroke(getStroke(state, r, c));
            fill(getFill(state, r, c));
            squareForRowCol(r, c);
        }
    }

    const [ipr, ipc] = addrToRowCol(state.ip);
    noFill();
    stroke('red');
    squareForRowCol(ipr, ipc);

    if (state.ip !== state.rb) {
        const [rbr, rbc] = addrToRowCol(state.rb);
        noFill();
        stroke('blue');
        squareForRowCol(rbr, rbc);
    }
}
