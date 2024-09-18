000001 IDENTIFICATION DIVISION.
000002 PROGRAM-ID. Intcode-Virtual-Machine.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT input-file
               ASSIGN TO KEYBOARD
               ORGANIZATION IS BINARY SEQUENTIAL.
      *    SELECT output-file
      *        ASSIGN TO DISPLAY
      *        ORGANIZATION IS BINARY SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD input-file.
      *    RECORD IS 1 CHARACTERS.
       01  val PICTURE X(1).
      *FD output-file
      *01  val.

       PROCEDURE DIVISION.
           DISPLAY "Hello, world!".
           STOP RUN.
