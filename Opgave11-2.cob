       IDENTIFICATION DIVISION.
       PROGRAM-ID. OPGAVEELLEVETO.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TRAN-FILE ASSIGN TO "Transaktioner.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT TABLE-FILE ASSIGN TO "Transaktion_tabel.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           
       DATA DIVISION.
       FILE SECTION.

       FD TRAN-FILE.
       01 TRAN-RECORD.
           05 SNDR-CPR         PIC X(15).
           05 SNDR-NAME        PIC X(30).
           05 SNDR-ADDRESS     PIC X(50).
           05 SNDR-BIRTHDAY    PIC X(10).
           05 SNDR-ACC-ID      PIC X(15).
           05 SNDR-ACC-REG     PIC X(4).
           05 AMOUNT           PIC X(17).
           05 CRNCY            PIC X(3).
           05 TRAN-TYPE        PIC X(20).
           05 STORE            PIC X(20).
           05 FILLER           PIC X.
           05 TRAN-TIME        PIC X(26).
       FD TABLE-FILE.
       01 TABLE-RECORD         PIC X(150).
       
       WORKING-STORAGE SECTION.
       01 END-OF-TRAN-FILE     PIC X VALUE "N".

       01 MONTHS OCCURS 12 TIMES.
           05 USD              PIC S9(11)V99 VALUE 0000000000.00.
           05 EUR              PIC S9(11)V99 VALUE 0000000000.00.
           05 DKK              PIC S9(11)V99 VALUE 0000000000.00.
           
       01 IX                   PIC 9(4) VALUE 0001.
       01 IX2                  PIC 9(4) VALUE 0001.
       01 PREV-CPR             PIC X(15) VALUE SPACES.
       01 BANK-ID              PIC 9(4).
       01 TOTAL-IN             PIC S9(11)V99 VALUE ZEROS.
       01 TOTAL-OUT            PIC S9(11)V99 VALUE ZEROS.
       01 TOTAL-FINAL          PIC S9(11)V99 VALUE 0000050000.00.
       01 MONEY-NUMERIC        PIC S9(11)V99 VALUE ZEROS.
       01 MONEY-DKK            PIC S9(11)V99 VALUE ZEROS.
       
       01 CUR-MONTH            PIC 9(2).
       
       01 MONEY-FORMAT-USD     PIC Z,ZZZ,ZZ9.99 VALUE ZEROS.
       01 MONEY-FORMAT-EUR     PIC Z,ZZZ,ZZ9.99 VALUE ZEROS.
       01 MONEY-FORMAT-DKK     PIC Z,ZZZ,ZZ9.99 VALUE ZEROS.
  
       01 MONTH-NAME           PIC X(20).

       PROCEDURE DIVISION.

       OPEN INPUT TRAN-FILE
       
       PERFORM UNTIL END-OF-TRAN-FILE = "Y"
           READ TRAN-FILE
               AT END
                   MOVE "Y" TO END-OF-TRAN-FILE
               NOT AT END
                   MOVE TRAN-TIME(6:2) TO CUR-MONTH
                   MOVE AMOUNT TO MONEY-DKK
                   IF CRNCY = "USD"
                       MULTIPLY 6.8 BY MONEY-DKK
                       MOVE MONEY-DKK TO MONEY-NUMERIC
                       IF MONEY-NUMERIC >= 0
                           ADD MONEY-NUMERIC TO USD(CUR-MONTH)
                       ELSE
                           SUBTRACT MONEY-NUMERIC FROM USD(CUR-MONTH)
                       END-IF
                   ELSE IF CRNCY = "EUR"
                       MULTIPLY 7.5 BY MONEY-DKK
                       MOVE MONEY-DKK TO MONEY-NUMERIC
                       IF MONEY-NUMERIC >= 0
                           ADD MONEY-NUMERIC TO EUR(CUR-MONTH)
                       ELSE
                           SUBTRACT MONEY-NUMERIC FROM EUR(CUR-MONTH)
                       END-IF
                   ELSE
                       MOVE MONEY-DKK TO MONEY-NUMERIC
                       IF MONEY-NUMERIC >= 0
                           ADD MONEY-NUMERIC TO DKK(CUR-MONTH)
                       ELSE
                           SUBTRACT MONEY-NUMERIC FROM DKK(CUR-MONTH)
                       END-IF
                   END-IF
       END-PERFORM
       CLOSE TRAN-FILE

       OPEN OUTPUT TABLE-FILE
       MOVE 0001 TO IX
       MOVE SPACES TO TABLE-RECORD
       STRING "Maaned               | "
              "USD (DKK)    | EUR (DKK)    | DKK"
           INTO TABLE-RECORD
       END-STRING
       WRITE TABLE-RECORD
       MOVE SPACES TO TABLE-RECORD
       PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 12
           EVALUATE IX
               WHEN 0001
                   MOVE "JANUAR" TO MONTH-NAME
               WHEN 0002
                   MOVE "FEBRUAR" TO MONTH-NAME
               WHEN 0003
                   MOVE "MARTS" TO MONTH-NAME
               WHEN 0004
                   MOVE "APRIL" TO MONTH-NAME
               WHEN 0005
                   MOVE "MAJ" TO MONTH-NAME
               WHEN 0006
                   MOVE "JUNI" TO MONTH-NAME
               WHEN 0007
                   MOVE "JULI" TO MONTH-NAME
               WHEN 0008
                   MOVE "AUGUST" TO MONTH-NAME
               WHEN 0009
                   MOVE "SEPTEMBER" TO MONTH-NAME
               WHEN 0010
                   MOVE "OCTOBER" TO MONTH-NAME
               WHEN 0011
                   MOVE "NOVEMBER" TO MONTH-NAME
               WHEN 0012
                   MOVE "DECEMBER" TO MONTH-NAME
               WHEN OTHER
                   MOVE "ERROR" TO MONTH-NAME
           END-EVALUATE
           MOVE USD(IX) TO MONEY-FORMAT-USD
           MOVE EUR(IX) TO MONEY-FORMAT-EUR
           MOVE DKK(IX) TO MONEY-FORMAT-DKK

           STRING
               MONTH-NAME
               " | "
               MONEY-FORMAT-USD
               " | "
               MONEY-FORMAT-EUR
               " | "
               MONEY-FORMAT-DKK
               INTO TABLE-RECORD
           END-STRING 

           WRITE TABLE-RECORD

           MOVE SPACES TO TABLE-RECORD
               
       END-PERFORM
       
       CLOSE TABLE-FILE

       STOP RUN.
