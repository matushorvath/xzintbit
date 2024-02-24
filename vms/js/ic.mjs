import { Vm } from './vm.mjs';
import yaml from 'yaml';
import fs from 'node:fs/promises';
import path from 'node:path';
import util from 'node:util';

const parseCommandLine = () => {
    try {
        const { values, positionals } = util.parseArgs({
            options: {
                debug: { type: 'boolean', short: 'd' },
                trace: { type: 'boolean', short: 't' }
            },
            allowPositionals: true
        });

        if (positionals.length > 1) {
            throw new Error('invalid command line; too many parameters');
        }

        return { debug: values.debug, trace: values.trace, image: positionals[0] };
    } catch (error) {
        console.error(error.message);
        console.log('Usage: node ic.mjs [(--debug|-d)] [(--trace|-t)] path/to/image.input');
        process.exit(1);
    }
};

const loadMap = async (imagePath) => {
    const parsedPath = path.parse(imagePath);
    const mapPath = path.format({ ...parsedPath, base: undefined, ext: 'map.yaml' });

    let mapData;
    try {
        mapData = yaml.parse(await fs.readFile(mapPath, 'utf8'));
    } catch {
        // No symbols found
        return;
    }

    return {
        locations: Object.fromEntries(Object.entries(mapData.symbols).map(([symbol, sdata]) =>
            [sdata.export.module + sdata.export.offset, symbol])),
        references: Object.fromEntries(Object.entries(mapData.symbols).map(([symbol, sdata]) =>
            sdata.imports.map(({ module, offsets }) => offsets.map(offset =>
                [module + offset, symbol])))),
        symbols: Object.fromEntries(Object.entries(mapData.symbols).map(([symbol, sdata]) =>
            [symbol, sdata.export.module + sdata.export.offset]))
    };
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

        if (args.trace) {
            vm.map = await loadMap(args.image);
        }

        for await (const char of vm.run(mem, getIns())) {
            process.stdout.write(String.fromCharCode(char));
        }
    } catch (error) {
        process.stderr.write(error.toString());
    }
};

await main();
