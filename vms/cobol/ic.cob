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
      *    RECORD IS 1 CHARACTERS.
       01  output-char
           PICTURE IS X(1).
      *    USAGE IS BINARY.
      *FD input-file
      *01  input-char.

       PROCEDURE DIVISION.
           OPEN OUTPUT output-file.
           WRITE output-char FROM "X".
           WRITE output-char FROM "Y".
           WRITE output-char FROM "Z".
           CLOSE output-file.

           DISPLAY "Hello, world!".
           STOP RUN.
