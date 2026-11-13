       IDENTIFICATION DIVISION.
       PROGRAM-ID. OPGAVETI.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT BANK-FILE ASSIGN TO "Banker.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT TRAN-FILE ASSIGN TO "Transaktioner.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT CUSTOMER-FILE ASSIGN TO "Kontoudskrifter.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD BANK-FILE.
       01 BANK-RECORD.
           05 REG-NR           PIC X(4).
           05 BANK-NAME        PIC X(30).
           05 BANK-ADDRESS     PIC X(50).
           05 PHONE            PIC X(15).
           05 EMAIL            PIC X(32).

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

       FD CUSTOMER-FILE.
       01 CUSTOMER-RECORD        PIC X(150).

       WORKING-STORAGE SECTION.
       01 END-OF-BANK-FILE     PIC X VALUE "N".
       01 END-OF-TRAN-FILE     PIC X VALUE "N".
       01 CUSTOMER-ID          PIC 9(5) VALUE 00000.
       
       01 BANKS OCCURS 100 TIMES.
           COPY "Opgave10_BANK.cpy".
       01 IX                   PIC 9(4) VALUE 0001.
       01 PREV-CPR             PIC X(15) VALUE SPACES.
       01 FILE-NAME            PIC X(100).
       01 BANK-ID              PIC 9(4).
       01 TOTAL-IN             PIC S9(11)V99 VALUE ZEROS.
       01 TOTAL-OUT            PIC S9(11)V99 VALUE ZEROS.
       01 TOTAL-FINAL          PIC S9(11)V99 VALUE 0000050000.00.
       01 MONEY-NUMERIC        PIC S9(11)V99 VALUE ZEROS.
       01 MONEY-DKK            PIC S9(11)V99 VALUE ZEROS.
       01 MONEY-FORMAT         PIC +Z,ZZZ,ZZ9.99.
       01 MONEY-FORMAT-DKK     PIC +Z,ZZZ,ZZ9.99.

       PROCEDURE DIVISION.

       OPEN INPUT BANK-FILE
       PERFORM UNTIL END-OF-BANK-FILE = "Y"
           READ BANK-FILE
               AT END
                   MOVE "Y" TO END-OF-BANK-FILE
               NOT AT END
                   MOVE BANK-RECORD TO BANKS(IX)
                   ADD 1 TO IX
       END-PERFORM
       CLOSE BANK-FILE

       OPEN INPUT TRAN-FILE
       OPEN OUTPUT CUSTOMER-FILE
       PERFORM UNTIL END-OF-TRAN-FILE = "Y"
           READ TRAN-FILE
               AT END
                   MOVE "Y" TO END-OF-TRAN-FILE
               NOT AT END
                   IF SNDR-CPR = PREV-CPR
                       MOVE AMOUNT TO MONEY-DKK
                       IF CRNCY = "USD"
                           MULTIPLY 6.8 BY MONEY-DKK
                       ELSE
                           IF CRNCY = "EUR"
                               MULTIPLY 7.5 BY MONEY-DKK
                           END-IF
                       END-IF
                       MOVE MONEY-DKK TO MONEY-FORMAT-DKK

                       MOVE AMOUNT TO MONEY-FORMAT
                       MOVE MONEY-DKK TO MONEY-NUMERIC
                       IF MONEY-NUMERIC >= 0
                           ADD MONEY-NUMERIC TO TOTAL-IN
                       ELSE
                           ADD MONEY-NUMERIC TO TOTAL-OUT
                       END-IF
                       STRING
                           TRAN-TIME(1:10)
                           " | "
                           TRAN-TIME(12:8)
                           "  | "
                           TRAN-TYPE
                           " |"
                           MONEY-FORMAT-DKK
                           " DKK | "
                           MONEY-FORMAT
                           " "
                           CRNCY
                           " | "
                           STORE
                           INTO CUSTOMER-RECORD
                       WRITE CUSTOMER-RECORD
                       MOVE SPACES TO CUSTOMER-RECORD
                       
                   ELSE
                       IF CUSTOMER-ID > 0000
                           PERFORM WRITE-FINAL
                       END-IF

                       MOVE ZEROS TO TOTAL-IN
                       MOVE ZEROS TO TOTAL-OUT
                       MOVE 0000050000.00 TO TOTAL-FINAL

                       ADD 1 TO CUSTOMER-ID
                       
                       MOVE SNDR-ACC-REG TO BANK-ID

                       MOVE
                       "--------------------------------------------"
                       TO CUSTOMER-RECORD
                       WRITE CUSTOMER-RECORD
                       MOVE SPACES TO CUSTOMER-RECORD

                       PERFORM WRITE-CUSTOMER
                       
                       PERFORM WRITE-ADDRESS

                       PERFORM WRITE-REG

                       PERFORM WRITE-BANK
                       WRITE CUSTOMER-RECORD
                       STRING
                           "Kontoudskrift for kontonr.: "
                           SNDR-ACC-ID
                       INTO CUSTOMER-RECORD
                       WRITE CUSTOMER-RECORD
                       MOVE SPACES TO CUSTOMER-RECORD

                       STRING
                       "Dato       | Tidspunkt | "
                       "Transaktionstype     | "
                       "Beløb (DKK)     | Beløb (valuta)  "
                       " | Butik"
                       INTO CUSTOMER-RECORD
                       WRITE CUSTOMER-RECORD
                       MOVE SPACES TO CUSTOMER-RECORD

                   END-IF
                   
                   MOVE SNDR-CPR TO PREV-CPR
       END-PERFORM

       PERFORM WRITE-FINAL

       CLOSE CUSTOMER-FILE

       CLOSE TRAN-FILE

       STOP RUN.

       WRITE-CUSTOMER.
           STRING
               "Kunde: "
               SNDR-NAME
           INTO CUSTOMER-RECORD
           WRITE CUSTOMER-RECORD
           MOVE SPACES TO CUSTOMER-RECORD
       EXIT.
       WRITE-ADDRESS.
           STRING
               "Adresse: "
               SNDR-ADDRESS
           INTO CUSTOMER-RECORD
           WRITE CUSTOMER-RECORD
           MOVE SPACES TO CUSTOMER-RECORD
       EXIT.
       WRITE-REG.
           STRING
               "Registreringsnummer: "
               SNDR-ACC-REG
           INTO CUSTOMER-RECORD
           WRITE CUSTOMER-RECORD
           MOVE SPACES TO CUSTOMER-RECORD
       EXIT.
       WRITE-BANK.
           STRING
               "Bank: "
               B-NAME OF BANKS(BANK-ID)
               INTO CUSTOMER-RECORD
           WRITE CUSTOMER-RECORD
           MOVE SPACES TO CUSTOMER-RECORD.
           STRING
               "Bankadresse: "
               B-ADDRESS OF BANKS(BANK-ID)
               INTO CUSTOMER-RECORD
           WRITE CUSTOMER-RECORD
           MOVE SPACES TO CUSTOMER-RECORD.
           STRING
               "Telefon: "
               B-PHONE OF BANKS(BANK-ID)
               INTO CUSTOMER-RECORD
           WRITE CUSTOMER-RECORD
           MOVE SPACES TO CUSTOMER-RECORD.
           STRING
               "E-mail: "
               B-EMAIL OF BANKS(BANK-ID)
               INTO CUSTOMER-RECORD
           WRITE CUSTOMER-RECORD
           MOVE SPACES TO CUSTOMER-RECORD.
           EXIT.
       WRITE-FINAL.
           MOVE SPACES TO CUSTOMER-RECORD
           WRITE CUSTOMER-RECORD
           MOVE TOTAL-IN TO MONEY-FORMAT
           STRING
               "Totalt indbetalt (DKK): "
               MONEY-FORMAT
               " DKK"
               INTO CUSTOMER-RECORD
           WRITE CUSTOMER-RECORD
           MOVE SPACES TO CUSTOMER-RECORD
           
           MOVE TOTAL-OUT TO MONEY-FORMAT
           STRING
               "Totalt udbetalt (DKK):  "
               MONEY-FORMAT
               " DKK"
               INTO CUSTOMER-RECORD
           WRITE CUSTOMER-RECORD
           MOVE SPACES TO CUSTOMER-RECORD
           ADD TOTAL-IN TO TOTAL-FINAL
           ADD TOTAL-OUT TO TOTAL-FINAL
           MOVE TOTAL-FINAL TO MONEY-FORMAT
           STRING
               "Saldo (DKK):            "
               MONEY-FORMAT
               " DKK"
               INTO CUSTOMER-RECORD
           WRITE CUSTOMER-RECORD
           MOVE SPACES TO CUSTOMER-RECORD
           WRITE CUSTOMER-RECORD

           MOVE "Med venlig hilsen" TO CUSTOMER-RECORD
           WRITE CUSTOMER-RECORD
           MOVE SPACES TO CUSTOMER-RECORD
           WRITE CUSTOMER-RECORD

           MOVE B-NAME OF BANKS(BANK-ID)
           TO CUSTOMER-RECORD
           WRITE CUSTOMER-RECORD
           MOVE SPACES TO CUSTOMER-RECORD
           WRITE CUSTOMER-RECORD
       EXIT.
