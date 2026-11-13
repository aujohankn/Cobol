       IDENTIFICATION DIVISION.
       PROGRAM-ID. OPGAVESEKS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "Kundeoplysninger.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE.
       01 INPUT-RECORD.
           05 KUNDE-ID         PIC X(10) VALUE ZEROS.
           05 KUNDE-FN         PIC X(20) VALUE SPACES.
           05 KUNDE-EN         PIC X(20) VALUE SPACES.
           05 KONTONR      PIC X(20) VALUE ZEROS.
           05 BALANCE      PIC 9(7)V99 VALUE ZEROS.
           05 VALUTA       PIC X(3) VALUE SPACES.
           05 VEJNAVN      PIC X(30).
           05 HUSNR        PIC X(5).
           05 ETAGE        PIC X(5).
           05 SIDE         PIC X(5).
           05 BYNAVN       PIC X(20).
           05 POSTNR       PIC X(4).
           05 LANDE-KODE   PIC X(2).
           05 TELEFON      PIC X(8).
           05 EMAIL        PIC X(50).


       WORKING-STORAGE SECTION.
       01 END-OF-FILE      PIC X VALUE "N".
       
       PROCEDURE DIVISION.
       
       OPEN INPUT INPUT-FILE

       PERFORM UNTIL END-OF-FILE = "Y"
           READ INPUT-FILE INTO INPUT-RECORD
               AT END
                   MOVE "Y" TO END-OF-FILE
               NOT AT END
                   DISPLAY INPUT-RECORD
           END-READ
       END-PERFORM
    
       CLOSE INPUT-FILE
    
       STOP RUN.
