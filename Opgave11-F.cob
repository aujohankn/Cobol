       IDENTIFICATION DIVISION.
       PROGRAM-ID. OPGAVE-ELLEVE-FULD.

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
           SELECT STATS-FILE ASSIGN TO "Statistik.txt"
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

       FD STATS-FILE.
       01 STATS-RECORD         PIC X(200).

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
           05 M-IN             PIC S9(11)V99 VALUE 0000000000.00.
           05 M-OUT            PIC S9(11)V99 VALUE 0000000000.00.
           05 USD              PIC S9(11)V99 VALUE 0000000000.00.
           05 EUR              PIC S9(11)V99 VALUE 0000000000.00.
           05 DKK              PIC S9(11)V99 VALUE 0000000000.00.
           
       01 STORES OCCURS 13 TIMES.
           05 STORE-NAME       PIC X(20).
           05 NUM-TRANS        PIC 9(5) VALUE ZEROS.
           05 TURNOVER         PIC 9(11)V99 VALUE ZEROS.

       01 IX                   PIC 9(4) VALUE 0001.
       01 IXF                  PIC ZZZ9.
       01 IX2                  PIC 9(4) VALUE 0001.
       01 PREV-CPR             PIC X(15) VALUE SPACES.
       01 FILE-NAME            PIC X(100).
       01 BANK-ID              PIC 9(4).
       01 TOTAL-IN             PIC S9(11)V99 VALUE ZEROS.
       01 TOTAL-OUT            PIC S9(11)V99 VALUE ZEROS.
       01 TOTAL-FINAL          PIC S9(11)V99 VALUE 0000050000.00.
       01 MONEY-NUMERIC        PIC S9(11)V99 VALUE ZEROS.
       01 MONEY-DKK            PIC S9(11)V99 VALUE ZEROS.
       01 MONEY-FORMAT         PIC +ZZ,ZZZ,ZZZ,ZZ9.99 VALUE ZEROS.
       01 MONEY-FORMAT-2       PIC ZZ,ZZZ,ZZZ,ZZ9.99 VALUE ZEROS.
       01 MONEY-FORMAT-3       PIC ZZ,ZZZ,ZZZ,ZZ9.99 VALUE ZEROS.
       01 MONEY-FORMAT-USD     PIC ZZ,ZZZ,ZZZ,ZZ9.99 VALUE ZEROS.
       01 MONEY-FORMAT-EUR     PIC ZZ,ZZZ,ZZZ,ZZ9.99 VALUE ZEROS.
       01 MONEY-FORMAT-DKK     PIC ZZ,ZZZ,ZZZ,ZZ9.99 VALUE ZEROS.
       01 CUR-MONTH            PIC 9(2).
       01 STORE-ID             PIC 9(2).
       01 STORE-IDF            PIC X(20).
       01 CURR-CHAR            PIC X(1) VALUE SPACE.
       01 PREV-CHAR            PIC X(1) VALUE SPACE.
       01 TYPE-COUNTS OCCURS 12 TIMES.
           05 COUNT-IN         PIC 9(5).
           05 COUNT-OUT        PIC 9(5).
           05 COUNT-OV         PIC 9(5).
       01 OTT                  PIC X(20).
       01 MONTH-NAME           PIC X(20).

       01 TOTAL-ALL-TRANS      PIC 9(17)V99 VALUE ZEROS. 
       01 TOTAL-ALL-TRANSF     PIC Z,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99 VALUE ZEROS.
       01 STAT-AVG             PIC 9(15)V99 VALUE ZEROS. 
       01 STAT-AVGF            PIC ZZ,ZZZ,ZZZ,ZZZ,ZZ9.99 VALUE ZEROS.

       01 STAT-STD-P           PIC 9(15)V99 VALUE ZEROS. 
       01 STAT-VAR             PIC 9(17)V99 VALUE ZEROS. 
       01 STAT-VARF            PIC Z,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99 VALUE ZEROS.
       01 STAT-STD-TOTAL       PIC 9(15)V99 VALUE ZEROS.
       01 STAT-STD-TOTALF      PIC ZZ,ZZZ,ZZZ,ZZZ,ZZ9.99 VALUE ZEROS.
       01 STAT-MIN             PIC S9(11)V99 VALUE 99999999999.99.
       01 STAT-MAX             PIC S9(11)V99 VALUE ZEROS.
       01 STAT-ABOVE-MEAN      PIC 9(6) VALUE ZEROS.
       01 STAT-BELOW-MEAN      PIC 9(6) VALUE ZEROS.
       01 STATF                PIC ZZZZZ9.

       PROCEDURE DIVISION.
      
      * BANKS
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

      * STORES
       OPEN INPUT STORE-FILE
       PERFORM UNTIL IX > 0013
           READ STORE-FILE
               NOT AT END 
                   MOVE S-NAME TO STORE-NAME(IX)
           ADD 1 TO IX
       END-PERFORM
       CLOSE STORE-FILE
      
      * TRANSACTIONS
       OPEN INPUT TRAN-FILE
       OPEN OUTPUT CUSTOMER-FILE
       PERFORM UNTIL END-OF-TRAN-FILE = "Y"
           READ TRAN-FILE
               AT END
                   MOVE "Y" TO END-OF-TRAN-FILE
               NOT AT END
      *        Write new customer
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

                   MOVE TRAN-TIME(6:2) TO CUR-MONTH

                   PERFORM GET-STORE-ID

                   ADD 1 TO NUM-TRANS(STORE-ID)

      *        Count transactions pr month
                   IF TRAN-TYPE = " Indbetaling         "
                       ADD 1 TO COUNT-IN(CUR-MONTH)
                   ELSE IF TRAN-TYPE = " Udbetaling          "
                       ADD 1 TO COUNT-OUT(CUR-MONTH)
                       ELSE
                           ADD 1 TO COUNT-OV(CUR-MONTH)
                       END-IF
                   END-IF

      *        Handle money
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
                       ADD MONEY-NUMERIC TO TOTAL-ALL-TRANS
                       ADD MONEY-NUMERIC TO TOTAL-IN
                       ADD MONEY-NUMERIC TO M-IN(CUR-MONTH)
                       ADD MONEY-NUMERIC TO TURNOVER(STORE-ID)
                       IF CRNCY = "USD"
                           ADD MONEY-NUMERIC TO USD(CUR-MONTH)
                       ELSE IF CRNCY = "EUR"
                           ADD MONEY-NUMERIC TO EUR(CUR-MONTH)
                           ELSE
                               ADD MONEY-NUMERIC TO DKK(CUR-MONTH)
                           END-IF
                       END-IF
                   ELSE
                       SUBTRACT MONEY-NUMERIC FROM TOTAL-ALL-TRANS
                       ADD MONEY-NUMERIC TO TOTAL-OUT
                       ADD MONEY-NUMERIC TO M-OUT(CUR-MONTH)
                       SUBTRACT MONEY-NUMERIC FROM TURNOVER(STORE-ID)
                       IF CRNCY = "USD"
                           SUBTRACT MONEY-NUMERIC FROM USD(CUR-MONTH)
                       ELSE IF CRNCY = "EUR"
                           SUBTRACT MONEY-NUMERIC FROM EUR(CUR-MONTH)
                           ELSE
                               SUBTRACT MONEY-NUMERIC
                                   FROM DKK(CUR-MONTH)
                           END-IF
                       END-IF
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
       
       OPEN OUTPUT STATS-FILE
       MOVE SPACES TO STATS-RECORD
       MOVE "TOP 3 KUNDER" TO STATS-RECORD
       WRITE STATS-RECORD
       MOVE SPACES TO STATS-RECORD

       MOVE 0001 TO IX
       PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 3
           MOVE CSTM-TOTAL-FINAL(IX) TO MONEY-FORMAT
           MOVE IX TO IXF
           STRING
               FUNCTION TRIM(IXF)
               ". "
               CSTM-ID(IX)
               " "
               CSTM-NAME(IX)
               " "
               MONEY-FORMAT
               " DKK"
               INTO STATS-RECORD
           END-STRING
           WRITE STATS-RECORD
           MOVE SPACES TO STATS-RECORD
       END-PERFORM

       MOVE SPACES TO STATS-RECORD
       WRITE STATS-RECORD

       MOVE "TOP 5 BUTIKKER | OMSAETNING" TO STATS-RECORD
       WRITE STATS-RECORD
       MOVE SPACES TO STATS-RECORD

       PERFORM CALC-TOP5-STORES
       MOVE 0001 TO IX
       PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 5
           MOVE ZEROS TO MONEY-FORMAT-2
           MOVE STR-TURNOVER(IX) TO MONEY-FORMAT-2
           MOVE IX TO IXF
           STRING
               FUNCTION TRIM(IXF)
               ". "
               STR-NAME(IX)
               " "
               MONEY-FORMAT-2
               " DKK"
           INTO STATS-RECORD
           END-STRING
           WRITE STATS-RECORD
           MOVE SPACES TO STATS-RECORD
       END-PERFORM

       WRITE STATS-RECORD
       
       MOVE 0001 TO IX
       MOVE SPACES TO STATS-RECORD
       STRING "Maaned               | "
              "Indbetalinger     | Udbetalinger       | "
              "USD (DKK)         | EUR (DKK)         | DKK            "
              "   | Hyppigste transaktion"
           INTO STATS-RECORD
       END-STRING
       WRITE STATS-RECORD
       MOVE SPACES TO STATS-RECORD
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

           IF (COUNT-IN(IX) > COUNT-OUT(IX))
               AND (COUNT-IN(IX) > COUNT-OV(IX))
               MOVE "Indbetaling" TO OTT
           ELSE
               IF (COUNT-OUT(IX) > COUNT-IN(IX))
               AND (COUNT-OUT(IX) > COUNT-OV(IX))
                   MOVE "Udbetaling" TO OTT
               ELSE
                   MOVE "Overfoersel" TO OTT
               END-IF
           END-IF
           
           MOVE M-IN(IX) TO MONEY-FORMAT-2
           MOVE M-OUT(IX) TO MONEY-FORMAT-3

           STRING
               MONTH-NAME
               " | "
               MONEY-FORMAT-2
               " | "
               "-" MONEY-FORMAT-3
               " | "
               MONEY-FORMAT-USD
               " | "
               MONEY-FORMAT-EUR
               " | "
               MONEY-FORMAT-DKK
               " | "
               OTT
               INTO STATS-RECORD
           END-STRING 

           WRITE STATS-RECORD

           MOVE SPACES TO STATS-RECORD
               
       END-PERFORM
       WRITE STATS-RECORD

      * Display Extra Statistics
       MOVE TOTAL-ALL-TRANS TO TOTAL-ALL-TRANSF

       STRING
           "TOTAL TRANSACTIONS:   54994"
           INTO STATS-RECORD
       END-STRING
       WRITE STATS-RECORD
       MOVE SPACES TO STATS-RECORD

       STRING
           "GROSS MONEY MOVED:   " TOTAL-ALL-TRANSF " DKK"
           INTO STATS-RECORD
       END-STRING
       WRITE STATS-RECORD
       MOVE SPACES TO STATS-RECORD

       MOVE TOTAL-ALL-TRANS TO STAT-AVG
       DIVIDE 54994 INTO STAT-AVG
       MOVE STAT-AVG TO STAT-AVGF

       STRING
           "AVERAGE TRANSACTION:   " STAT-AVGF " DKK"
           INTO STATS-RECORD
       END-STRING
       WRITE STATS-RECORD
       MOVE SPACES TO STATS-RECORD



       MOVE "N" TO END-OF-TRAN-FILE
       OPEN INPUT TRAN-FILE
       PERFORM UNTIL END-OF-TRAN-FILE = "Y"
           READ TRAN-FILE
               AT END
                   MOVE "Y" TO END-OF-TRAN-FILE
               NOT AT END
                   MOVE AMOUNT TO MONEY-DKK
                   IF CRNCY = "USD"
                       MULTIPLY 6.8 BY MONEY-DKK
                   ELSE
                       IF CRNCY = "EUR"
                           MULTIPLY 7.5 BY MONEY-DKK
                       END-IF
                   END-IF
                   IF MONEY-DKK > STAT-AVG
                       ADD 1 TO STAT-ABOVE-MEAN
                   ELSE
                       ADD 1 TO STAT-BELOW-MEAN
                   END-IF
                   IF MONEY-DKK > STAT-MAX
                       MOVE MONEY-DKK TO STAT-MAX
                   END-IF
                   IF MONEY-DKK < STAT-MIN
                       MOVE MONEY-DKK TO STAT-MIN
                   END-IF
                   MOVE MONEY-DKK TO STAT-STD-P
                   SUBTRACT STAT-AVG FROM STAT-STD-P
                   MULTIPLY STAT-STD-P BY STAT-STD-P
                   ADD STAT-STD-P TO STAT-VAR
       END-PERFORM
       CLOSE TRAN-FILE

       MOVE STAT-MIN TO MONEY-FORMAT
       STRING
           "MIN TRANSACTION:   " MONEY-FORMAT " DKK"
           INTO STATS-RECORD
       END-STRING
       WRITE STATS-RECORD
       MOVE SPACES TO STATS-RECORD
       
       MOVE STAT-MAX TO MONEY-FORMAT-2
       STRING
           "MAX TRANSACTION:   " MONEY-FORMAT-2 " DKK"
           INTO STATS-RECORD
       END-STRING
       WRITE STATS-RECORD
       MOVE SPACES TO STATS-RECORD

       MOVE STAT-VAR TO STAT-VARF
       STRING
           "SUM OF SQUARED DEVIATIONS: " STAT-VARF " DKK^2"
           INTO STATS-RECORD
       END-STRING
       WRITE STATS-RECORD
       MOVE SPACES TO STATS-RECORD

       DIVIDE 54994 INTO STAT-VAR
       MOVE STAT-VAR TO STAT-VARF
       STRING
           "VARIANCE FOR ALL TRANSACTIONS: " STAT-VARF " DKK^2"
           INTO STATS-RECORD
       END-STRING
       WRITE STATS-RECORD
       MOVE SPACES TO STATS-RECORD

       MOVE FUNCTION SQRT(STAT-VAR) TO STAT-VAR
       MOVE STAT-VAR TO STAT-VARF
       STRING
           "STANDARD DEVIATION:   " STAT-VARF " DKK"
           INTO STATS-RECORD
       END-STRING
       WRITE STATS-RECORD
       MOVE SPACES TO STATS-RECORD
       
       MOVE STAT-ABOVE-MEAN TO STATF
       STRING
           "TRANSACTIONS ABOVE MEAN: " STATF
           INTO STATS-RECORD
       END-STRING
       WRITE STATS-RECORD
       MOVE SPACES TO STATS-RECORD
       
       MOVE STAT-BELOW-MEAN TO STATF
       STRING
           "TRANSACTIONS BELOW MEAN: " STATF
           INTO STATS-RECORD
       END-STRING
       WRITE STATS-RECORD
       MOVE SPACES TO STATS-RECORD

       STRING
           " "
           INTO STATS-RECORD
       END-STRING
       WRITE STATS-RECORD
       MOVE SPACES TO STATS-RECORD

       CLOSE STATS-FILE

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
       

       CALC-TOP5-STORES.
           MOVE 0001 TO IX
           PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 13
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
