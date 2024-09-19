000001 IDENTIFICATION DIVISION.
000002 PROGRAM-ID. Intcode-Virtual-Machine.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT output-file
               ASSIGN TO DISPLAY
               ORGANIZATION IS BINARY SEQUENTIAL.
      *    SELECT input-file
      *        ASSIGN TO KEYBOARD
      *        ORGANIZATION IS BINARY SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD output-file.
      *    RECORD CONTAINS 1 CHARACTERS.
       01  output-char
           USAGE IS BINARY-CHAR.
      *FD input-file
      *01  input-char.

       PROCEDURE DIVISION.
           OPEN OUTPUT output-file.
           WRITE output-char FROM 65.
           WRITE output-char FROM 66.
           WRITE output-char FROM 120.
      * c2 a1 194 161 = inverted exclamation mark
      *    WRITE output-char FROM 194.
      *    WRITE output-char FROM 161.
           CLOSE output-file.

      *    DISPLAY "Hello, world!".
           STOP RUN.
