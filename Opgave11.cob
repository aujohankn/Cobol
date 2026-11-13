       IDENTIFICATION DIVISION.
       PROGRAM-ID. OPGAVEELLEVE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT BANK-FILE ASSIGN TO "Banker.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT TRAN-FILE ASSIGN TO "Transaktioner.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT STORE-FILE ASSIGN TO "Butikker.txt"
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
       01 CUSTOMER-RECORD      PIC X(150).

       FD STORE-FILE.
       01 S-NAME               PIC X(20).

       WORKING-STORAGE SECTION.
       01 END-OF-BANK-FILE     PIC X VALUE "N".
       01 END-OF-TRAN-FILE     PIC X VALUE "N".
       01 CUSTOMER-ID          PIC 9(5) VALUE 00000.
       
       01 BANKS OCCURS 100 TIMES.
           COPY "Opgave10_BANK.cpy".
       01 TOP-3 OCCURS 3 TIMES.
           05 CSTM-ID          PIC X(15).
           05 CSTM-NAME        PIC X(30).
           05 CSTM-TOTAL-FINAL PIC S9(11)V99.

       01 TOP-5-STORES OCCURS 5 TIMES.
           05 STR-NAME         PIC X(20).
           05 STR-TURNOVER     PIC 9(11)V99 VALUE ZEROS.

       01 MONTHS OCCURS 12 TIMES.
           05 M-IN                PIC S9(11)V99 VALUE 0000000000.00.
           05 M-OUT               PIC S9(11)V99 VALUE 0000000000.00.
           
       01 STORES OCCURS 13 TIMES.
           05 STORE-NAME       PIC X(20).
           05 NUM-TRANS        PIC 9(5) VALUE ZEROS.
           05 TURNOVER         PIC 9(11)V99 VALUE ZEROS.

       01 IX                   PIC 9(4) VALUE 0001.
       01 IX2                  PIC 9(4) VALUE 0001.
       01 PREV-CPR             PIC X(15) VALUE SPACES.
       01 FILE-NAME            PIC X(100).
       01 BANK-ID              PIC 9(4).
       01 TOTAL-IN             PIC S9(11)V99 VALUE ZEROS.
       01 TOTAL-OUT            PIC S9(11)V99 VALUE ZEROS.
       01 TOTAL-FINAL          PIC S9(11)V99 VALUE 0000050000.00.
       01 MONEY-NUMERIC        PIC S9(11)V99 VALUE ZEROS.
       01 MONEY-DKK            PIC S9(11)V99 VALUE ZEROS.
       01 MONEY-FORMAT         PIC +Z,ZZZ,ZZ9.99 VALUE ZEROS.
       01 MONEY-FORMAT-DKK     PIC +Z,ZZZ,ZZ9.99.
       01 CUR-MONTH            PIC 9(2).
       01 STORE-ID             PIC 9(2).
       01 STORE-IDF            PIC X(20).
       01 CURR-CHAR    PIC X(1) VALUE SPACE.
       01 PREV-CHAR    PIC X(1) VALUE SPACE.
       01 COUNT-IN             PIC 9(5).
       01 COUNT-OUT            PIC 9(5).
       01 COUNT-OV             PIC 9(5).

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

       MOVE 0001 TO IX
       OPEN INPUT STORE-FILE
       PERFORM UNTIL IX > 0013
           READ STORE-FILE
               NOT AT END 
                   MOVE S-NAME TO STORE-NAME(IX)
           ADD 1 TO IX
       END-PERFORM
       CLOSE STORE-FILE

       OPEN INPUT TRAN-FILE
       OPEN OUTPUT CUSTOMER-FILE
       PERFORM UNTIL END-OF-TRAN-FILE = "Y"
           READ TRAN-FILE
               AT END
                   MOVE "Y" TO END-OF-TRAN-FILE
               NOT AT END
                   IF SNDR-CPR NOT = PREV-CPR
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

                   PERFORM GET-STORE-ID

                   IF TRAN-TYPE = " Indbetaling         "
                       ADD 1 TO COUNT-IN
                   ELSE IF TRAN-TYPE = " Udbetaling          "
                       ADD 1 TO COUNT-OUT
                   ELSE
                       ADD 1 TO COUNT-OV
                   END-IF

                   MOVE AMOUNT TO MONEY-DKK
                   IF CRNCY = "USD"
                       MULTIPLY 6.8 BY MONEY-DKK
                   ELSE
                       IF CRNCY = "EUR"
                           MULTIPLY 7.5 BY MONEY-DKK
                       END-IF
                   END-IF
                   MOVE MONEY-DKK TO MONEY-FORMAT-DKK
                   
                   ADD 1 TO NUM-TRANS(STORE-ID)
                   

                   MOVE AMOUNT TO MONEY-FORMAT
                   MOVE MONEY-DKK TO MONEY-NUMERIC
                   MOVE TRAN-TIME(6:2) TO CUR-MONTH
                   IF MONEY-NUMERIC >= 0
                       ADD MONEY-NUMERIC TO TOTAL-IN
                       ADD MONEY-NUMERIC TO M-IN(CUR-MONTH)
                       ADD MONEY-NUMERIC TO TURNOVER(STORE-ID)
                   ELSE
                       ADD MONEY-NUMERIC TO TOTAL-OUT
                       ADD MONEY-NUMERIC TO M-OUT(CUR-MONTH)
                       SUBTRACT MONEY-NUMERIC FROM TURNOVER(STORE-ID)
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

                   MOVE SNDR-CPR TO PREV-CPR
       END-PERFORM

       PERFORM WRITE-FINAL

       CLOSE CUSTOMER-FILE

       CLOSE TRAN-FILE
       
       MOVE CSTM-TOTAL-FINAL(1) TO MONEY-FORMAT
       DISPLAY "TOP 1" CSTM-ID(1) CSTM-NAME(1) " " MONEY-FORMAT " DKK"
       MOVE CSTM-TOTAL-FINAL(2) TO MONEY-FORMAT
       DISPLAY "TOP 2" CSTM-ID(2) CSTM-NAME(2) " " MONEY-FORMAT " DKK"
       MOVE CSTM-TOTAL-FINAL(3) TO MONEY-FORMAT
       DISPLAY "TOP 3" CSTM-ID(3) CSTM-NAME(3) " " MONEY-FORMAT " DKK"

       DISPLAY "----------------------------------------------"

       PERFORM DISPLAY-MONTHS
       
       DISPLAY "----------------------------------------------"

       PERFORM DISPLAY-STORES
       
       DISPLAY "----------------------------------------------"
       DISPLAY "Indbetalinger: " COUNT-IN
       DISPLAY "Udbetalinger: " COUNT-OUT
       DISPLAY "Overfoersler: " COUNT-OV
       

       STOP RUN.
      *__________________________PARAGRAPHS_____________________________

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
           
           PERFORM CALC-TOP3-CUSTOMERS
           
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
       
       GET-STORE-ID.
           MOVE SPACES TO STORE-IDF
           PERFORM STRIP-STORE
           EVALUATE STORE-IDF
               WHEN "Supermarked"
                   MOVE 01 TO STORE-ID
               WHEN "Tjbutik"
                   MOVE 02 TO STORE-ID
               WHEN "Elektronikbutik"
                   MOVE 03 TO STORE-ID
               WHEN "Restaurant"
                   MOVE 04 TO STORE-ID
               WHEN "Boghandel"
                   MOVE 05 TO STORE-ID
               WHEN "Apotek"
                   MOVE 06 TO STORE-ID
               WHEN "Tankstation"
                   MOVE 07 TO STORE-ID
               WHEN "Caf"
                   MOVE 08 TO STORE-ID
               WHEN "Biograf"
                   MOVE 09 TO STORE-ID
               WHEN "Mbelbutik"
                   MOVE 10 TO STORE-ID
               WHEN "Blomsterhandler"
                   MOVE 11 TO STORE-ID
               WHEN "Bageri"
                   MOVE 12 TO STORE-ID
               WHEN "Fitnesscenter"
                   MOVE 13 TO STORE-ID
               WHEN OTHER
                   DISPLAY "ERROR"
           END-EVALUATE
       EXIT.
       
       DISPLAY-MONTHS.
           DISPLAY "Maned | Indbetalinger (DKK) | Udbetalinger (DKK)"
           DISPLAY "JAN.  | " M-IN(1) "     | " M-OUT(1)
           DISPLAY "FEB.  | " M-IN(2) "     | " M-OUT(2)
           DISPLAY "MAR.  | " M-IN(3) "     | " M-OUT(3)
           DISPLAY "APR.  | " M-IN(4) "     | " M-OUT(4)
           DISPLAY "MAY.  | " M-IN(5) "     | " M-OUT(5)
           DISPLAY "JUN.  | " M-IN(6) "     | " M-OUT(6)
           DISPLAY "JUL.  | " M-IN(7) "     | " M-OUT(7)
           DISPLAY "AUG.  | " M-IN(8) "     | " M-OUT(8)
           DISPLAY "SEP.  | " M-IN(9) "     | " M-OUT(9)
           DISPLAY "OCT.  | " M-IN(10) "     | " M-OUT(10)
           DISPLAY "NOV.  | " M-IN(11) "     | " M-OUT(11)
           DISPLAY "DEC.  | " M-IN(12) "     | " M-OUT(12)
       EXIT.

       DISPLAY-STORES.
           DISPLAY "Butik                | Trans. | Omsaetning"
           MOVE 0001 TO IX

           PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 13
               DISPLAY STORE-NAME(IX) " | "
                   NUM-TRANS(IX) " | " TURNOVER(IX)
               IF TURNOVER(IX) > STR-TURNOVER(1)
                   MOVE TOP-5-STORES(4) TO TOP-5-STORES(5)
                   MOVE TOP-5-STORES(3) TO TOP-5-STORES(4)
                   MOVE TOP-5-STORES(2) TO TOP-5-STORES(3)
                   MOVE TOP-5-STORES(1) TO TOP-5-STORES(2)
                   MOVE STORE-NAME(IX) TO STR-NAME(1)
                   MOVE TURNOVER(IX) TO STR-TURNOVER(1)
               ELSE IF TURNOVER(IX) > STR-TURNOVER(2)
                   MOVE TOP-5-STORES(4) TO TOP-5-STORES(5)
                   MOVE TOP-5-STORES(3) TO TOP-5-STORES(4)
                   MOVE TOP-5-STORES(2) TO TOP-5-STORES(3)
                   MOVE STORE-NAME(IX) TO STR-NAME(2)
                   MOVE TURNOVER(IX) TO STR-TURNOVER(2)
               ELSE IF TURNOVER(IX) > STR-TURNOVER(3)
                   MOVE TOP-5-STORES(4) TO TOP-5-STORES(5)
                   MOVE TOP-5-STORES(3) TO TOP-5-STORES(4)
                   MOVE STORE-NAME(IX) TO STR-NAME(3)
                   MOVE TURNOVER(IX) TO STR-TURNOVER(3)
               ELSE IF TURNOVER(IX) > STR-TURNOVER(4)
                   MOVE TOP-5-STORES(4) TO TOP-5-STORES(5)
                   MOVE STORE-NAME(IX) TO STR-NAME(4)
                   MOVE TURNOVER(IX) TO STR-TURNOVER(4)
               ELSE IF TURNOVER(IX) > STR-TURNOVER(5)
                   MOVE STORE-NAME(IX) TO STR-NAME(5)
                   MOVE TURNOVER(IX) TO STR-TURNOVER(5)
                   END-IF
                   END-IF
                   END-IF
                   END-IF
               END-IF
           END-PERFORM
           DISPLAY "----------------------------------------------"
           DISPLAY "TOP 5 STORES"
           DISPLAY TOP-5-STORES(1)
           DISPLAY TOP-5-STORES(2)
           DISPLAY TOP-5-STORES(3)
           DISPLAY TOP-5-STORES(4)
           DISPLAY TOP-5-STORES(5)
       EXIT.
       
       STRIP-STORE.
       MOVE 0001 TO IX
       MOVE 0001 TO IX2
       PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 20
           MOVE CURR-CHAR TO PREV-CHAR
           MOVE STORE(IX:1) TO CURR-CHAR
           IF CURR-CHAR NOT = SPACE OR PREV-CHAR NOT = SPACE
               IF (CURR-CHAR >= "A" AND CURR-CHAR <= "Z")
                   OR (CURR-CHAR >= "a" AND CURR-CHAR <= "z")
                   MOVE CURR-CHAR TO STORE-IDF(IX2:1)
                   ADD 1 TO IX2
               END-IF
           END-IF
       END-PERFORM
       EXIT.

       CALC-TOP3-CUSTOMERS.
       IF TOTAL-FINAL > CSTM-TOTAL-FINAL(1)
               MOVE TOP-3(2) TO TOP-3(3)
               MOVE TOP-3(1) TO TOP-3(2)
               MOVE SNDR-ACC-ID TO CSTM-ID(1)
               MOVE SNDR-NAME TO CSTM-NAME(1)
               MOVE TOTAL-FINAL TO CSTM-TOTAL-FINAL(1)
           ELSE
               IF TOTAL-FINAL > CSTM-TOTAL-FINAL(2)
                   MOVE TOP-3(2) TO TOP-3(3)
                   MOVE SNDR-ACC-ID TO CSTM-ID(2)
                   MOVE SNDR-NAME TO CSTM-NAME(2)
                   MOVE TOTAL-FINAL TO CSTM-TOTAL-FINAL(2)
               ELSE
                   IF TOTAL-FINAL > CSTM-TOTAL-FINAL(3)
                       MOVE SNDR-ACC-ID TO CSTM-ID(3)
                       MOVE SNDR-NAME TO CSTM-NAME(3)
                       MOVE TOTAL-FINAL TO CSTM-TOTAL-FINAL(3)
                   END-IF
               END-IF
           END-IF
       EXIT.
