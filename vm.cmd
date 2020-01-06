@ECHO OFF
SETLOCAL

REM Edit this script to run your ASCII-enabled IntCode VM
REM It should read input from stdin and write output to stdout (with no extra characters)

REM build the VM
IF NOT EXIST vm\ic.exe (
    ECHO First run, building the Intcode VM >&2
    make -C vm >&2
    IF ERRORLEVEL 1 EXIT /B 1
)

REM execute the vm
vm\ic.exe "%1"
IF ERRORLEVEL 1 EXIT /B 1
