import { Vm } from './vm.mjs';
import { writeStreamAndWait } from './util.mjs';
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
        console.log('Usage: node ic.mjs [(--debug|-d)] [(--trace|-t)] [(--trace-address|-a) <address>] path/to/image.input');
        process.exit(1);
    }
};

const loadMap = async (imagePath) => {
    const mapPath = `${imagePath}.map.yaml`;

    let mapData;
    try {
        mapData = yaml.parse(await fs.readFile(mapPath, 'utf8'));
    } catch {
        // No symbols found
        return;
    }

    return Object.fromEntries(Object.entries(mapData.symbols).map(([symbol, sdata]) =>
        [sdata.export.module + sdata.export.offset, symbol]));
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
        process.stderr.write(error.toString());
    }
};

await main();
