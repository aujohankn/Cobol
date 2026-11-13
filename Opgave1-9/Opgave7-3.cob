       IDENTIFICATION DIVISION.
       PROGRAM-ID. OPGAVESYV.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "Kundeoplysninger.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTPUT-FILE ASSIGN TO "KundeoplysningerOut.txt"
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

       FD OUTPUT-FILE.
       01 OUTPUT-RECORD.
           05 NAVN-ADR     PIC X(100).
       
       WORKING-STORAGE SECTION.
       01 END-OF-FILE      PIC X VALUE "N".
       01 FULL-NAME        PIC X(40) VALUE SPACES.
       01 FULL-ADDRESS     PIC X(45) VALUE SPACES.
       01 FULL-CITY        PIC X(24).
       01 NEW-LINE         PIC X(2) VALUE X'0D0A'.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           OPEN INPUT INPUT-FILE
           OPEN OUTPUT OUTPUT-FILE

           PERFORM UNTIL END-OF-FILE = "Y"
               READ INPUT-FILE INTO INPUT-RECORD
                   AT END
                       MOVE "Y" TO END-OF-FILE
                   NOT AT END
                       MOVE KUNDE-ID TO NAVN-ADR
                       WRITE OUTPUT-RECORD
                       MOVE SPACES TO NAVN-ADR

                       PERFORM FORMAT-NAVN
                       
                       MOVE FULL-NAME TO NAVN-ADR
                       WRITE OUTPUT-RECORD
                       MOVE SPACES TO NAVN-ADR

                       PERFORM FORMAT-ADRESSE

                       MOVE FULL-ADDRESS TO NAVN-ADR
                       WRITE OUTPUT-RECORD
                       MOVE SPACES TO NAVN-ADR

                       STRING BYNAVN DELIMITED BY SIZE
                           " " DELIMITED BY SIZE 
                           POSTNR DELIMITED BY SIZE
                           INTO FULL-CITY
                       MOVE FULL-CITY TO NAVN-ADR
                       WRITE OUTPUT-RECORD
                       MOVE SPACES TO NAVN-ADR

                       MOVE TELEFON TO NAVN-ADR
                       WRITE OUTPUT-RECORD
                       MOVE SPACES TO NAVN-ADR

                       MOVE EMAIL TO NAVN-ADR
                       WRITE OUTPUT-RECORD
                       MOVE SPACES TO NAVN-ADR

                       MOVE SPACE TO NAVN-ADR
                       WRITE OUTPUT-RECORD
                       
               END-READ
           END-PERFORM
    
           CLOSE INPUT-FILE
           CLOSE OUTPUT-FILE
    
           STOP RUN.

           FORMAT-NAVN.
               STRING KUNDE-FN DELIMITED BY SPACE
                       " " DELIMITED BY SIZE 
                       KUNDE-EN DELIMITED BY SPACE
                       INTO FULL-NAME
               END-STRING
           EXIT.
           
           FORMAT-ADRESSE.
               STRING VEJNAVN DELIMITED BY SIZE
                       " " DELIMITED BY SIZE 
                       HUSNR DELIMITED BY SIZE
                       " " DELIMITED BY SIZE 
                       ETAGE DELIMITED BY SIZE
                       " " DELIMITED BY SIZE 
                       SIDE DELIMITED BY SIZE
                       INTO FULL-ADDRESS
               END-STRING
           EXIT.