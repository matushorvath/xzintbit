      * make clean-cobol ; make build-cobol && ./cobol/ic ../bin/as.input

       IDENTIFICATION DIVISION.
       PROGRAM-ID. Intcode-Virtual-Machine.

       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT program-file
               ASSIGN TO DISK program-name
               ORGANIZATION IS BINARY SEQUENTIAL.
           SELECT output-file
               ASSIGN TO DISK "/dev/stdout"
               ORGANIZATION IS BINARY SEQUENTIAL.
           SELECT input-file
               ASSIGN TO DISK "/dev/stdin"
               ORGANIZATION IS BINARY SEQUENTIAL.

       DATA DIVISION.

       FILE SECTION.
       FD program-file.
       01  program-char
           USAGE IS BINARY-CHAR.
       FD output-file.
       01  output-char
           USAGE IS BINARY-CHAR.
       FD input-file.
       01  input-char
           USAGE IS BINARY-CHAR.

       WORKING-STORAGE SECTION.
       01  program-name PIC X(256).

       PROCEDURE DIVISION.
           DISPLAY 1 UPON ARGUMENT-NUMBER.
           ACCEPT program-name FROM ARGUMENT-VALUE
               ON EXCEPTION
                   DISPLAY "Usage: ic program.input" UPON STDERR
                   GOBACK GIVING 1.

           OPEN INPUT program-file.

           READ program-file.
           DISPLAY program-char.
           READ program-file.
           DISPLAY program-char.
           READ program-file.
           DISPLAY program-char.

           CLOSE program-file.

           OPEN INPUT input-file.
           OPEN OUTPUT output-file.

           DISPLAY "Hello, world!".

           WRITE output-char FROM 65.
           READ input-file.
           WRITE output-char FROM 66.
           WRITE output-char FROM 120.
           DISPLAY input-char.

           READ input-file.
           DISPLAY input-char.

      * c2 a1 194 161 = inverted exclamation mark
           WRITE output-char FROM 194.
           WRITE output-char FROM 161.

           CLOSE output-file.
           CLOSE input-file.

           STOP RUN.
