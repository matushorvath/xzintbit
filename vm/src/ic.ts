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
    const vm = new Vm(input.split(',').map(i => Number(i)));

    for await (const char of vm.run(getIns())) {
        await new Promise(resolve => process.stdout.write(String.fromCharCode(char), resolve));
    }
};

main().catch(error => { console.error(error); process.exit(1); });
