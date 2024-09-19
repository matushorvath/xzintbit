000001 IDENTIFICATION DIVISION.
000002 PROGRAM-ID. Intcode-Virtual-Machine.

       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *     SELECT output-file
      *         ASSIGN TO DISPLAY
      *         ORGANIZATION IS BINARY SEQUENTIAL.
           SELECT input-file
               ASSIGN TO KEYBOARD
               ORGANIZATION IS BINARY SEQUENTIAL.

       DATA DIVISION.

       FILE SECTION.
      * FD output-file.
      *     RECORD CONTAINS 1 CHARACTERS.
      * 01  output-char
      *     USAGE IS BINARY-CHAR.
       FD input-file.
       01  input-char
           USAGE IS BINARY-CHAR.

       WORKING-STORAGE SECTION.
       01  input-1 PIC X.

       PROCEDURE DIVISION.
           OPEN INPUT input-file.

           DISPLAY "A" WITH NO ADVANCING.
      *     ACCEPT input-1 FROM STDIN.
           READ input-file RECORD.
           DISPLAY "B" WITH NO ADVANCING.
           DISPLAY "x" WITH NO ADVANCING.
           DISPLAY input-char.

           READ input-file RECORD.
           DISPLAY input-char.

      *     OPEN OUTPUT output-file.
      *     WRITE output-char FROM 65.
      *     WRITE output-char FROM 66.
      *     WRITE output-char FROM 120.
      * c2 a1 194 161 = inverted exclamation mark
      *     WRITE output-char FROM 194.
      *     WRITE output-char FROM 161.

      *     CLOSE output-file.
           CLOSE input-file.

      *     DISPLAY "Hello, world!".
           STOP RUN.
