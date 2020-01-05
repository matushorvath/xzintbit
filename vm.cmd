REM Edit this script to run your ASCII-enabled IntCode VM
REM It should read input from stdin and write output to stdout (with no extra characters)

PUSHD vm

REM sanity check, node.js must be installed
node --version > NUL
IF ERRORLEVEL 1 (
    ECHO "You will need Node.js version 10.x or newer to run the Intcode VM" >&2
    ECHO "Or you can use your own Intcode VM, see https://tinyurl.com/vq59mtk" >&2
    EXIT 1
)

REM install modules if they don't exist
IF NOT EXIST node_modules (
    ECHO "First run, installing TypeScript modules" >&2
    npm i >&2
)

REM execute the vm
npx ts-node src\ic.ts "..\%1"

POPD
