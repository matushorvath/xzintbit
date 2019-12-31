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

    let line: string[] = [];
    for await (const char of vm.run(getIns())) {
        if (char === 10) {
            console.log(line.join(''));
            line = [];
        } else {
            //console.log('c', char, line);
            line.push(String.fromCharCode(char));
        }
    }

    rl.close();
};

main()
    .then(() => console.log('done'))
    .catch(error => console.log('error:', error));
