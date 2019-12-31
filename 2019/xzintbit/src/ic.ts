import { Vm } from './vm-n';
import { promises as fs } from 'fs';
import * as readline from 'readline';

const rl = readline.createInterface({
    input: process.stdin,
    output: process.stdout
});

async function* getIns() {
    while (true) {
        const input = await new Promise<string>(resolve => rl.question('', resolve));
        for (const char of input) {
            yield char.charCodeAt(0);
        }
        yield 10;
    }
}

const main = async () => {
    const input = await fs.readFile(process.argv[2], 'utf8');
    const mem = input.split(',').map(i => Number(i));
    const vm = new Vm(0, mem);

    let line: number[] = [];
    for await (const char of vm.run(getIns())) {
        if (char === 10) {
            console.log(line.map(n => String.fromCharCode(n)).join(''), [...line, char]);
            line = [];
        } else {
            line.push(char);
        }
    }

    rl.close();
};

main()
    .then(() => console.log('done'))
    .catch(error => console.log('error:', error));
