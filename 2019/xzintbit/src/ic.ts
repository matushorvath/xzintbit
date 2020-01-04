import { Vm } from './vm';
import { promises as fs } from 'fs';

async function* getIns() {
    for await (const chunk of process.stdin) {
        for (const char of chunk as Buffer) {
            yield char;
        }
    }
}

const main = async () => {
    const input = await fs.readFile(process.argv[2], 'utf8');
    const mem = input.split(',').map(i => Number(i));
    const vm = new Vm(0, mem);

    let line: number[] = [];
    for await (const char of vm.run(getIns())) {
        if (char === 10) {
            console.log(line.map(n => String.fromCharCode(n)).join('')/*, [...line, char]*/);
            line = [];
        } else {
            line.push(char);
        }
    }
};

main()
    .then(() => console.log('done'))
    .catch(error => { console.log('error:', error); process.exit(1); });
