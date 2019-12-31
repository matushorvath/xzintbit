import { Vm } from './vm';
import { promises as fs } from 'fs';

const main = async () => {
    const vm = new Vm(0, []);
    vm.asm(await fs.readFile(process.argv[2], 'utf8'));
    await fs.writeFile(process.argv[3], vm.getAsInput(), 'utf8');
};

main()
    .then(() => console.log('done'))
    .catch(error => console.log('error:', error));
