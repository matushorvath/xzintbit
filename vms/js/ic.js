import { NoMoreInputsError, Vm } from './vm.js';
import { writeStreamAndWait } from './util.js';
import yaml from 'yaml';
import fs from 'node:fs/promises';
import util from 'node:util';
import '@ungap/with-resolvers';

const parseCommandLine = () => {
    try {
        const { values, positionals } = util.parseArgs({
            options: {
                debug: { type: 'boolean', short: 'd' },
                trace: { type: 'boolean', short: 't' },
                'trace-address': { type: 'string', short: 'a' }
            },
            allowPositionals: true
        });

        if (values['trace-address']) {
            values['trace-address'] = Number.parseInt(values['trace-address']);
        }

        if (positionals.length > 1) {
            throw new Error('invalid command line; too many parameters');
        }

        return {
            debug: values.debug,
            trace: values.trace,
            traceAddress: values['trace-address'],
            image: positionals[0]
        };
    } catch (error) {
        console.error(error.message);
        console.log('Usage: ic [(--debug|-d)] [(--trace|-t)] [(--trace-address|-a) <address>] path/to/image.input');
        process.exit(1);
    }
};

const loadMap = async (imagePath) => {
    const mapPath = `${imagePath}.map.yaml`;

    let modules;
    try {
        modules = yaml.parse(await fs.readFile(mapPath, 'utf8'));
    } catch {
        // No symbols found
        return;
    }

    return Object.fromEntries(Object.values(modules).flatMap(symbols =>
        Object.entries(symbols).map(([identifier, { address }]) => [address, identifier])));
};

async function* getIns() {
    for await (const chunk of process.stdin) {
        for (const char of chunk) {
            yield char;
        }
    }
}

const main = async () => {
    try {
        const args = parseCommandLine();

        const input = await fs.readFile(args.image, 'utf8');
        const mem = input.split(',').map(i => Number(i));

        const vm = new Vm();

        vm.debug = args.debug;
        vm.trace = args.trace;
        vm.traceAddress = args.traceAddress;

        if (vm.trace || vm.traceAddress) {
            vm.map = await loadMap(args.image);
        }

        for await (const char of vm.run(mem, getIns())) {
            await writeStreamAndWait(process.stdout, String.fromCharCode(char));
        }
    } catch (error) {
        if (error instanceof NoMoreInputsError) {
            process.stderr.write('no more inputs\n');
            process.exit(1);
        }
        process.stderr.write(error.toString());
    }
};

await main();
