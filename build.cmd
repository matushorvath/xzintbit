SETLOCAL

CMD /C vm.cmd src\as.input < src\as.s > as.stg1.input
IF ERRORLEVEL 1 (
    TYPE as.stg1.input
    EXIT /B 1
)

CMD /C vm.cmd as.stg1.input < src\as.s > as.stg2.input
IF ERRORLEVEL 1 (
    TYPE as.stg2.input
    EXIT /B 1
)

ECHO n | COMP /A as.stg1.input as.stg2.input
IF ERRORLEVEL 1 EXIT /B 1

COPY /Y as.stg2.input src\as.input
IF ERRORLEVEL 1 EXIT /B 1

DEL /Q as.stg1.input
DEL /Q as.stg2.input
