import fs from 'node:fs/promises';
import yaml from 'yaml';

const main = async () => {
    if (process.argv.length < 5) {
        process.stderr.write('Usage: profile <profile.yaml> <map.yaml> <result.yaml>\n');
        process.exit(1);
    }

    const profile = yaml.parse(await fs.readFile(process.argv[2], 'utf8'));
    const modules = yaml.parse(await fs.readFile(process.argv[3], 'utf8'));

    const symbols = Object.fromEntries(Object.values(modules).flatMap(moduleSymbols =>
        Object.entries(moduleSymbols).map(([identifier, { address }]) => [address, identifier])));

    const hotlist = Object.entries(profile)
        .filter(([address]) => Object.hasOwn(symbols, address))
        .toSorted((a, b) => b[1] - a[1])
        .map(([address, hits]) => ({ address: symbols[address], hits }));

    await fs.writeFile(process.argv[4], yaml.stringify(hotlist), 'utf8');
};

await main();
