@ECHO OFF
SETLOCAL EnableExtensions EnableDelayedExpansion

SET failed_count=0

:outdir_loop
SET "outdir=%TMP%\xzintbit~%RANDOM%.tmp"
IF EXIST "%outdir%" GOTO :outdir_loop
MKDIR "%outdir%"

ECHO Output dir: %outdir%

ECHO.

FOR %%i IN (test\*.s) DO (
    FOR /F "usebackq delims=" %%j IN ('%%i') DO SET id=%%~nj
    SET input="%%i"
    SET output="%outdir%\!id!.input"
    SET expect="test\!id!.input"

    ECHO | SET /P="Test !id!: "

    CMD /C vm.cmd src\as.input < "!input!" > "!output!" 2> NUL

    ECHO n | COMP /A "!output!" "!expect!" > NUL 2> NUL
    IF NOT ERRORLEVEL 1 (
        ECHO OK
    ) else (
        ECHO FAILED
        ECHO n | COMP /A "!output!" "!expect!" 2> NUL
        ECHO Output:
        TYPE "!output!"
        ECHO Expect:
        TYPE "!expect!"
        SET /A "failed_count=(!failed_count! + 1)"
    )
)

ECHO.

IF %failed_count% EQU 0 (
    ECHO All tests PASSED
) ELSE IF %failed_count% EQU 1 (
    ECHO 1 test FAILED
    EXIT /B 1
) ELSE (
    ECHO %failed_count% tests FAILED
    EXIT /B 1
)
