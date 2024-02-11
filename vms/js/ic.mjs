import fs from 'fs/promises';
import { Vm } from './vm.mjs';

async function* getIns() {
    for await (const chunk of process.stdin) {
        for (const char of chunk) {
            yield char;
        }
    }
}

const main = async () => {
    try {
        const input = await fs.readFile(process.argv[2], 'utf8');
        const mem = input.split(',').map(i => Number(i));

        const vm = new Vm();

        for await (const char of vm.run(mem, getIns())) {
            process.stdout.write(String.fromCharCode(char));
        }
    } catch (error) {
        process.stderr.write(error.toString());
    }
};

await main();
