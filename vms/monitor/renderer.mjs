/* eslint-env browser */

const state = {
    memSize: 0,
    imageSize: 0,
    ip: 0,
    rb: 0,
    mem: {}
};

const onUpdateState = (update) => {
    state.mem = {};

    for (const event of update) {
        if (event.e === 's') {
            // Reset
            state.memSize = event.a;
            state.imageSize = event.a;
        } else if (event.e === 'r' || event.e === 'w') {
            // Memory read/write
            state.memSize = Math.max(state.memSize, event.a);
            state.mem[event.a] = event;
        } else if (event.e === 'v') {
            // Register values
            state.memSize = Math.max(state.memSize, event.ip, event.rb);
            state.ip = event.ip;
            state.rb = event.rb;
        }
    };

    redraw();
};

window.ipc.onUpdateState(onUpdateState);

function setup() {
    createCanvas(1000, 800);
    background(255);
    noLoop();
}

const COLS = 200;
const SIZE = 5;

const getFill = (r, c) => {
    const addr = r * COLS + c;
    switch (state?.mem[addr]?.e) {
        case undefined: return 'white';
        case 'r': return 'lightgreen';
        case 'w': return 'orange';
    }
};

const getStroke = (r, c) => {
    const addr = r * COLS + c;
    if (addr < state?.imageSize) {
        return 'lightblue';
    } else {
        return 'lightgray';
    }
};

const addrToRowCol = (addr) => [Math.floor(addr / COLS), addr % COLS];

const squareForRowCol = (r, c) => square(SIZE + c * SIZE, SIZE + r * SIZE, SIZE);

function draw() {
    clear();

    const rows = Math.max(Math.ceil(state.memSize / COLS), 100);

    for (let r = 0; r < rows; r++) {
        for (let c = 0; c < COLS; c++) {
            stroke(getStroke(r, c));
            fill(getFill(r, c));
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
        stroke('black');
        squareForRowCol(rbr, rbc);
    }
}
