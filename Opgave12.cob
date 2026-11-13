       IDENTIFICATION DIVISION.
       PROGRAM-ID. OPGAVE-TOLV.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CUSTOMER-FILE ASSIGN TO "KundeOplysninger.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT SANCTION-FILE ASSIGN TO "SanctionList.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT REPORT-FILE ASSIGN TO "SanctionReport.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT CONFIG-FILE ASSIGN TO "MatchConfig.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT STATS-FILE ASSIGN TO "SanctionStats.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD CUSTOMER-FILE.
       01 CUSTOMER-RECORD.
           05 C-ID         PIC X(11).
           05 C-NAME       PIC X(20).
           05 C-BDAY       PIC X(10).
           05 C-ADDR       PIC X(30).
           05 C-RESI       PIC X(2).
       
       FD SANCTION-FILE.
       01 SANCTION-RECORD.
           05 S-ID         PIC X(5).
           05 S-NAME       PIC X(20).
           05 S-ALI1       PIC X(20).
           05 S-ALI2       PIC X(20).
           05 S-ALI3       PIC X(20).
           05 S-ALI4       PIC X(20).
           05 S-ALI5       PIC X(20).
           05 S-BDAY       PIC X(10).
           05 S-RESI       PIC X(2).

       FD REPORT-FILE.
       01 REPORT-RECORD    PIC X(150).

       FD CONFIG-FILE.
       01 CONFIG-RECORD.
           05 C-W-NAME      PIC X(5).
           05 C-W-ALIA      PIC X(5).
           05 C-W-BDAY      PIC X(5).
           05 C-W-LAND      PIC X(5).

       FD STATS-FILE.
       01 STATS-RECORD     PIC X(100).

       WORKING-STORAGE SECTION.
              
       01 END-OF-SANCTION-FILE PIC X VALUE "N".
       01 END-OF-CUSTOMER-FILE PIC X VALUE "N".
       01 SANCTIONS OCCURS 200 TIMES.
           02 S-ALIASES    PIC X(20) VALUE SPACES OCCURS 6 TIMES.
           02 S-BDAY-F     PIC X(6).
           02 S-LAND       PIC X(2).
           02 S-NO-ALIASES PIC 9(1) VALUE 1.
           02 S-NO-MATCHES PIC 9(3) VALUE ZEROS.

      * INDEX VARIABLES
       01 S-IX              PIC 9(4) VALUE 0001.
       01 A-IX             PIC 9(2) VALUE 01.
      
      * PERCENT VARIABLES
       01 PCT-NAME         PIC 9(3)V99 VALUE ZEROS.
       01 PCT-ALIA         PIC 9(3)V99 VALUE ZEROS.
       01 PCT-BEST-ALIA    PIC 9(3)V99 VALUE ZEROS.
       01 PCT-BDAY         PIC 9(3)V99 VALUE ZEROS.
       01 PCT-LAND         PIC 9(3)V99 VALUE ZEROS.
       01 PCT-TOTAL        PIC 9(3)V99 VALUE ZEROS.
       01 PCT-FORMAT       PIC ZZ9.99.

      * LEVENSHTEIN VARIABLES
       01 L-STR1           PIC X(20) VALUE SPACES.
       01 L-STR1-LENGTH    PIC 9(2) VALUE 20.
       01 L-STR2           PIC X(20) VALUE SPACES.
       01 L-STR2-LENGTH    PIC 9(2) VALUE 20.
       01 L-RESULT         PIC 9(2) VALUE ZEROS.
       01 L-PREV-ROW PIC 9(2) VALUE ZEROS OCCURS 21 TIMES.
       01 L-CURR-ROW PIC 9(2) VALUE ZEROS OCCURS 21 TIMES.
       01 I PIC 9(2) VALUE ZEROS.
       01 J PIC 9(2) VALUE ZEROS.
       01 I-1 PIC 9(2) VALUE ZEROS.
       01 J-1 PIC 9(2) VALUE ZEROS.
       01 L-IX PIC 9(2) VALUE ZEROS.
       01 M PIC 9(2) VALUE ZEROS.
       01 N PIC 9(2) VALUE ZEROS.

       01 BEST-ALIAS-MATCH PIC X(20) VALUE SPACES.
       01 BEST-ALIAS-LDIST     PIC 9(2) VALUE 20.
       
       01 W-NAME      PIC 9(5) VALUE ZEROS.
       01 W-ALIA      PIC 9(5) VALUE ZEROS.
       01 W-BDAY      PIC 9(5) VALUE ZEROS.
       01 W-LAND      PIC 9(5) VALUE ZEROS.

       01 STAT-S-LAND PIC 9(3) VALUE ZEROS OCCURS 10 TIMES.
       01 STAT-TOP3-MATCH PIC 9(3)V99 VALUE ZEROS OCCURS 3 TIMES.
       01 STAT-TOP3-CUSTOMER PIC X(30) VALUE SPACES OCCURS 3 TIMES.
       01 STAT-TOP3-SANCTION PIC X(100) VALUE SPACES OCCURS 3 TIMES.
       01 STAT-TOP3-S-MATCH PIC 9(3) VALUE ZEROS OCCURS 3 TIMES.
       01 STAT-FORMAT  PIC X(50) VALUE SPACES.
       01 STAT-TOTAL-MATCHES PIC 9(4) VALUE ZEROS.

       01 SUS-THRESHOLD PIC 9(3)V99 VALUE 066.00.

       01 NUMBER-FORMAT PIC ZZZZZ9.
       
       PROCEDURE DIVISION.
      
       OPEN INPUT CONFIG-FILE
           READ CONFIG-FILE
               MOVE C-W-NAME TO W-NAME
               MOVE C-W-ALIA TO W-ALIA
               MOVE C-W-BDAY TO W-BDAY
               MOVE C-W-LAND TO W-LAND
       CLOSE CONFIG-FILE

       OPEN INPUT SANCTION-FILE
       PERFORM UNTIL END-OF-SANCTION-FILE = "Y"
           READ SANCTION-FILE
               AT END
                   MOVE "Y" TO END-OF-SANCTION-FILE
               NOT AT END
                   MOVE S-NAME TO S-ALIASES(S-IX, 1)
                   MOVE S-ALI1 TO S-ALIASES(S-IX, 2)
                   IF S-ALI2(1:1) IS NOT = "-"
                       MOVE S-ALI2 TO S-ALIASES(S-IX, 3)
                       ADD 1 TO S-NO-ALIASES(S-IX)
                   END-IF
                   IF S-ALI3(1:1) IS NOT = "-"
                       MOVE S-ALI3 TO S-ALIASES(S-IX, 4)
                       ADD 1 TO S-NO-ALIASES(S-IX)
                   END-IF
                   IF S-ALI4(1:1) IS NOT = "-"
                       MOVE S-ALI4 TO S-ALIASES(S-IX, 5)
                       ADD 1 TO S-NO-ALIASES(S-IX)
                   END-IF
                   IF S-ALI5(1:1) IS NOT = "-"
                       MOVE S-ALI5 TO S-ALIASES(S-IX, 6)
                       ADD 1 TO S-NO-ALIASES(S-IX)
                   END-IF
                   STRING
                       S-BDAY(6:2)
                       S-BDAY(9:2)
                       S-BDAY(3:2)
                       INTO S-BDAY-F(S-IX)
                   END-STRING
                   MOVE S-RESI TO S-LAND(S-IX)

                   ADD 1 TO S-IX
           END-READ
       END-PERFORM
       CLOSE SANCTION-FILE

       MOVE 0001 TO S-IX

       OPEN INPUT CUSTOMER-FILE
       OPEN OUTPUT REPORT-FILE

       DISPLAY "Writing to SanctionReport.txt"

       PERFORM UNTIL END-OF-CUSTOMER-FILE = "Y"
           READ CUSTOMER-FILE
               AT END
                   MOVE "Y" TO END-OF-CUSTOMER-FILE
               NOT AT END
                   PERFORM VARYING S-IX FROM 1 BY 1 UNTIL S-IX > 200
                       MOVE SPACES TO L-STR1
                       MOVE SPACES TO L-STR2
    
      *                CALCULATE NAME MATCH
                       MOVE C-NAME TO L-STR1
                       MOVE S-ALIASES(S-IX, 1) TO L-STR2
                       PERFORM LEVENSHTEIN-DISTANCE
                       COMPUTE PCT-NAME = (1 - (L-RESULT / M)) * 100
    
      *                CALCULATE ALIAS MATCH
                       MOVE ZEROS TO PCT-ALIA
                       MOVE 20 TO BEST-ALIAS-LDIST
                       PERFORM VARYING A-IX FROM 2 BY 1
                           UNTIL A-IX > S-NO-ALIASES(S-IX) + 1
                           MOVE C-NAME TO L-STR1
                           MOVE S-ALIASES(S-IX, A-IX) TO L-STR2
                           PERFORM LEVENSHTEIN-DISTANCE
                           IF L-RESULT < BEST-ALIAS-LDIST
                               MOVE L-RESULT TO BEST-ALIAS-LDIST
                               MOVE S-ALIASES(S-IX, A-IX)
                                   TO BEST-ALIAS-MATCH
                               COMPUTE PCT-BEST-ALIA =
                                   (1 - L-RESULT / M) * 100
                               END-COMPUTE
                           END-IF
                           COMPUTE PCT-ALIA =
                               PCT-ALIA +
                               ((1 - L-RESULT / M) *
                               (100 / S-NO-ALIASES(S-IX)))
                           END-COMPUTE
                       END-PERFORM
    
      *                CALCULATE BIRTHDAY MATCH
                       MOVE C-BDAY TO L-STR1
                       MOVE S-BDAY-F(S-IX) TO L-STR2
                       PERFORM LEVENSHTEIN-DISTANCE
                       COMPUTE PCT-BDAY = (1 - (L-RESULT / M)) * 100
          
      *                CALCULATE LAND MATCH
                       MOVE C-RESI TO L-STR1
                       MOVE S-LAND(S-IX) TO L-STR2
                       PERFORM LEVENSHTEIN-DISTANCE
                       COMPUTE PCT-LAND = (1 - (L-RESULT / M)) * 100
    
      *                CALCULATE TOTAL MATCH
                       COMPUTE PCT-TOTAL =
                           (PCT-NAME * (W-NAME / 10000)) +
                           (PCT-ALIA * (W-ALIA / 10000)) +
                           (PCT-BDAY * (W-BDAY / 10000)) +
                           (PCT-LAND * (W-LAND / 10000))
                       END-COMPUTE
    
                       IF PCT-TOTAL >= SUS-THRESHOLD
                           ADD 1 TO STAT-TOTAL-MATCHES
                           ADD 1 TO S-NO-MATCHES(S-IX)
                           PERFORM COUNT-LAND
                           MOVE SPACES TO REPORT-RECORD
                           
                           STRING "Kunde: " C-NAME
                           INTO REPORT-RECORD
                           END-STRING
                           WRITE REPORT-RECORD
                           MOVE SPACES TO REPORT-RECORD

                           STRING "Foedselsdag: " C-BDAY
                           INTO REPORT-RECORD
                           END-STRING
                           WRITE REPORT-RECORD
                           MOVE SPACES TO REPORT-RECORD

                           STRING "Adresse: "
                               FUNCTION TRIM(C-ADDR)
                               ", " C-RESI
                           INTO REPORT-RECORD
                           END-STRING
                           WRITE REPORT-RECORD
                           MOVE SPACES TO REPORT-RECORD
                           WRITE REPORT-RECORD

                           MOVE PCT-NAME TO PCT-FORMAT
                           STRING "Match fundet med: "
                           INTO REPORT-RECORD
                           END-STRING
                           WRITE REPORT-RECORD
                           MOVE SPACES TO REPORT-RECORD

                           MOVE PCT-NAME TO PCT-FORMAT
                           STRING
                               FUNCTION TRIM(S-ALIASES(S-IX, 1))
                               ", "
                               S-BDAY-F(S-IX)
                           INTO REPORT-RECORD
                           END-STRING
                           WRITE REPORT-RECORD
                           MOVE SPACES TO REPORT-RECORD

                           MOVE PCT-BEST-ALIA TO PCT-FORMAT
                           STRING
                               "Antal Aliaser: "
                               S-NO-ALIASES(S-IX) ", "
                               "Bedste alias: "
                               FUNCTION TRIM(BEST-ALIAS-MATCH)
                               ", Bedste alias match-procent: "
                               PCT-FORMAT " %"
                           INTO REPORT-RECORD
                           END-STRING
                           WRITE REPORT-RECORD
                           MOVE SPACES TO REPORT-RECORD

                           WRITE REPORT-RECORD

                           MOVE PCT-NAME TO PCT-FORMAT
                           STRING "Navn Match: " PCT-FORMAT " %"
                           INTO REPORT-RECORD
                           END-STRING
                           WRITE REPORT-RECORD
                           MOVE SPACES TO REPORT-RECORD

                           MOVE PCT-ALIA TO PCT-FORMAT
                           STRING
                               "Alias Match: "
                               PCT-FORMAT
                               " %"
                           INTO REPORT-RECORD
                           END-STRING
                           WRITE REPORT-RECORD
                           MOVE SPACES TO REPORT-RECORD

                           MOVE PCT-BDAY TO PCT-FORMAT
                           STRING "Foedselsdag Match: " PCT-FORMAT " %"
                           INTO REPORT-RECORD
                           END-STRING
                           WRITE REPORT-RECORD
                           MOVE SPACES TO REPORT-RECORD

                           MOVE PCT-LAND TO PCT-FORMAT
                           STRING "Land Match: " PCT-FORMAT " %"
                           INTO REPORT-RECORD
                           END-STRING
                           WRITE REPORT-RECORD
                           MOVE SPACES TO REPORT-RECORD

                           PERFORM CALC-TOP3-SUSCUS

                           MOVE PCT-TOTAL TO PCT-FORMAT
                           STRING "Total Match: " PCT-FORMAT " %"
                           INTO REPORT-RECORD
                           END-STRING
                           WRITE REPORT-RECORD

                           STRING
                           "------------------------------------------"
                           "------------------------------------------"
                           INTO REPORT-RECORD
                           END-STRING
                           WRITE REPORT-RECORD
                           MOVE SPACES TO REPORT-RECORD

                           WRITE REPORT-RECORD
                       END-IF
                   END-PERFORM
           END-READ
       END-PERFORM

       WRITE REPORT-RECORD
       MOVE SPACES TO REPORT-RECORD

       CLOSE CUSTOMER-FILE
       CLOSE REPORT-FILE
       DISPLAY "Finished Writing to SanctionReport.txt"
       DISPLAY "Writing Statistics to SanctionStats.txt"

       OPEN OUTPUT STATS-FILE

       MOVE SPACES TO STATS-RECORD
       MOVE SUS-THRESHOLD TO PCT-FORMAT
       STRING
           "Minimum match % for counting as a match: "
           PCT-FORMAT
           " %"
           INTO STATS-RECORD
       END-STRING
       WRITE STATS-RECORD
       MOVE SPACES TO STATS-RECORD
       
       MOVE STAT-TOTAL-MATCHES TO NUMBER-FORMAT
       STRING
           "Total Matches found: "
           FUNCTION TRIM(NUMBER-FORMAT)
           INTO STATS-RECORD
       END-STRING
       WRITE STATS-RECORD
       MOVE SPACES TO STATS-RECORD

       MOVE "WEIGHTS: " TO STATS-RECORD
       WRITE STATS-RECORD
       MOVE SPACES TO STATS-RECORD

       COMPUTE W-NAME = W-NAME / 100
       COMPUTE W-ALIA = W-ALIA / 100
       COMPUTE W-BDAY = W-BDAY / 100
       COMPUTE W-LAND = W-LAND / 100

       MOVE W-NAME TO PCT-FORMAT
       STRING
           " - Name:                   "
           PCT-FORMAT
           "%, "
           INTO STATS-RECORD
       END-STRING

       WRITE STATS-RECORD
       MOVE SPACES TO STATS-RECORD
       MOVE W-ALIA TO PCT-FORMAT
       STRING
           " - Aliases:                "
           PCT-FORMAT
           "%, "
           INTO STATS-RECORD
       END-STRING
       WRITE STATS-RECORD
       MOVE SPACES TO STATS-RECORD
       MOVE W-BDAY TO PCT-FORMAT
       STRING
           " - Birthday:               "
           PCT-FORMAT "%, "
           INTO STATS-RECORD
       END-STRING
       WRITE STATS-RECORD
       MOVE SPACES TO STATS-RECORD
       MOVE W-LAND TO PCT-FORMAT
       STRING
           " - Country of residence:   "
           PCT-FORMAT "%"
           INTO STATS-RECORD
       END-STRING
       
       WRITE STATS-RECORD
       MOVE SPACES TO STATS-RECORD

       WRITE STATS-RECORD

       PERFORM SUS-PR-COUNTRY
       
       MOVE SPACES TO STATS-RECORD
       WRITE STATS-RECORD
       
       MOVE "TOP 3 SUS KUNDER" TO STATS-RECORD
       WRITE STATS-RECORD
       MOVE SPACES TO STATS-RECORD
       MOVE STAT-TOP3-MATCH(1) TO PCT-FORMAT
       STRING
           "TOP 1: "
           FUNCTION TRIM(STAT-TOP3-CUSTOMER(1))
           ", "
           PCT-FORMAT "%"
           INTO STATS-RECORD
       END-STRING
       WRITE STATS-RECORD
       MOVE SPACES TO STATS-RECORD
       MOVE STAT-TOP3-MATCH(2) TO PCT-FORMAT
       STRING
           "TOP 2: "
           FUNCTION TRIM(STAT-TOP3-CUSTOMER(2))
           ", "
           PCT-FORMAT "%"
           INTO STATS-RECORD
       END-STRING
       WRITE STATS-RECORD
       MOVE SPACES TO STATS-RECORD
       MOVE STAT-TOP3-MATCH(3) TO PCT-FORMAT
       STRING
           "TOP 3: "
           FUNCTION TRIM(STAT-TOP3-CUSTOMER(3))
           ", "
           PCT-FORMAT "%"
           INTO STATS-RECORD
       END-STRING
       WRITE STATS-RECORD
       MOVE SPACES TO STATS-RECORD

       MOVE 0001 TO S-IX
       PERFORM VARYING S-IX FROM 1 BY 1 UNTIL S-IX > 200
           PERFORM CALC-TOP3-SANC
       END-PERFORM

       MOVE SPACES TO STATS-RECORD
       WRITE STATS-RECORD

       MOVE "TOP 3 SANKTIONSPOSTER MATCHES" TO STATS-RECORD
       WRITE STATS-RECORD
       MOVE SPACES TO STATS-RECORD
       MOVE STAT-TOP3-S-MATCH(1) TO NUMBER-FORMAT
       STRING
           "TOP 1: "
           FUNCTION TRIM(STAT-TOP3-SANCTION(1))
           ", "
           FUNCTION TRIM(NUMBER-FORMAT)
           " matches"
           INTO STATS-RECORD
       END-STRING
       WRITE STATS-RECORD
       MOVE SPACES TO STATS-RECORD
       MOVE STAT-TOP3-S-MATCH(2) TO NUMBER-FORMAT
       STRING
           "TOP 2: "
           FUNCTION TRIM(STAT-TOP3-SANCTION(2))
           ", "
           FUNCTION TRIM(NUMBER-FORMAT)
           " matches"
           INTO STATS-RECORD
       END-STRING
       WRITE STATS-RECORD
       MOVE SPACES TO STATS-RECORD
       MOVE STAT-TOP3-S-MATCH(3) TO NUMBER-FORMAT
       STRING
           "TOP 3: "
           FUNCTION TRIM(STAT-TOP3-SANCTION(3))
           ", "
           FUNCTION TRIM(NUMBER-FORMAT)
           " matches"
           INTO STATS-RECORD
       END-STRING
       WRITE STATS-RECORD
       MOVE SPACES TO STATS-RECORD

       CLOSE STATS-FILE

       STOP RUN.
      *_________________________________________________________________       
       LEVENSHTEIN-DISTANCE.
           MOVE 20 TO L-STR1-LENGTH
           MOVE 20 TO L-STR2-LENGTH
           PERFORM UNTIL L-STR1(L-STR1-LENGTH:1) NOT = " "
               SUBTRACT 1 FROM L-STR1-LENGTH
           END-PERFORM
           PERFORM UNTIL L-STR2(L-STR2-LENGTH:1) NOT = " "
               SUBTRACT 1 FROM L-STR2-LENGTH
           END-PERFORM

           MOVE L-STR1-LENGTH TO M
           MOVE L-STR2-LENGTH TO N

           MOVE 01 TO J

           PERFORM VARYING J FROM 1 BY 1 UNTIL J > L-STR2-LENGTH + 1
               MOVE J TO L-PREV-ROW(J)
               SUBTRACT 1 FROM L-PREV-ROW(J)
           END-PERFORM

           MOVE 02 TO I
           
           PERFORM VARYING I FROM 2 BY 1 UNTIL I > M + 1
               MOVE I TO I-1
               SUBTRACT 1 FROM I-1
               MOVE I TO L-CURR-ROW(1)
               SUBTRACT 1 FROM L-CURR-ROW(1)
               MOVE 02 TO J
               PERFORM VARYING J FROM 2 BY 1 UNTIL J > N + 1
                   MOVE J TO J-1
                   SUBTRACT 1 FROM J-1
                   
                   IF L-STR1(I-1:1) = L-STR2(J-1:1)
                       MOVE L-PREV-ROW(J-1) TO L-CURR-ROW(J)
                   ELSE
                       MOVE
                       FUNCTION MIN(L-CURR-ROW(J-1),
                           L-PREV-ROW(J), L-PREV-ROW(J-1))
                       TO L-CURR-ROW(J)
                       ADD 1 TO L-CURR-ROW(J)
                   END-IF
               END-PERFORM
               MOVE 01 TO L-IX
               PERFORM VARYING L-IX FROM 1 BY 1
               UNTIL L-IX > N + 1
                   MOVE L-CURR-ROW(L-IX) TO L-PREV-ROW(L-IX)
               END-PERFORM
           END-PERFORM
           MOVE L-CURR-ROW(L-STR2-LENGTH + 1) TO L-RESULT

       EXIT.

       COUNT-LAND.
           EVALUATE C-RESI
            WHEN "DK"
            ADD 1 TO STAT-S-LAND(1)
            WHEN "US"
            ADD 1 TO STAT-S-LAND(2)
            WHEN "GB"
            ADD 1 TO STAT-S-LAND(3)
            WHEN "DE"
            ADD 1 TO STAT-S-LAND(4)
            WHEN "FR"
            ADD 1 TO STAT-S-LAND(5)
            WHEN "ES"
            ADD 1 TO STAT-S-LAND(6)
            WHEN "SE"
            ADD 1 TO STAT-S-LAND(7)
            WHEN "NO"
            ADD 1 TO STAT-S-LAND(8)
            WHEN "IT"
            ADD 1 TO STAT-S-LAND(9)
            WHEN "CA"
            ADD 1 TO STAT-S-LAND(10)
       EXIT.

       SUS-PR-COUNTRY.
           MOVE SPACES TO STATS-RECORD
       MOVE "Suspicious customers pr Country" TO STATS-RECORD
       WRITE STATS-RECORD
       MOVE SPACES TO STATS-RECORD
       MOVE STAT-S-LAND(1) TO NUMBER-FORMAT
       STRING
           "DK: "
           FUNCTION TRIM(NUMBER-FORMAT)
           INTO STATS-RECORD
       END-STRING
       WRITE STATS-RECORD
       MOVE SPACES TO STATS-RECORD
       
       MOVE STAT-S-LAND(2) TO NUMBER-FORMAT
       STRING
           "US: "
           FUNCTION TRIM(NUMBER-FORMAT)
           INTO STATS-RECORD
       END-STRING
       WRITE STATS-RECORD
       MOVE SPACES TO STATS-RECORD
       
       MOVE STAT-S-LAND(3) TO NUMBER-FORMAT
       STRING
           "GB: "
           FUNCTION TRIM(NUMBER-FORMAT)
           INTO STATS-RECORD
       END-STRING
       WRITE STATS-RECORD
       MOVE SPACES TO STATS-RECORD
       
       MOVE STAT-S-LAND(4) TO NUMBER-FORMAT
       STRING
           "DE: "
           FUNCTION TRIM(NUMBER-FORMAT)
           INTO STATS-RECORD
       END-STRING
       WRITE STATS-RECORD
       MOVE SPACES TO STATS-RECORD
       
       MOVE STAT-S-LAND(5) TO NUMBER-FORMAT
       STRING
           "FR: "
           FUNCTION TRIM(NUMBER-FORMAT)
           INTO STATS-RECORD
       END-STRING
       WRITE STATS-RECORD
       MOVE SPACES TO STATS-RECORD

       MOVE STAT-S-LAND(6) TO NUMBER-FORMAT
       STRING
           "ES: "
           FUNCTION TRIM(NUMBER-FORMAT)
           INTO STATS-RECORD
       END-STRING
       WRITE STATS-RECORD
       MOVE SPACES TO STATS-RECORD

       MOVE STAT-S-LAND(7) TO NUMBER-FORMAT
       STRING
           "SE: "
           FUNCTION TRIM(NUMBER-FORMAT)
           INTO STATS-RECORD
       END-STRING
       WRITE STATS-RECORD
       MOVE SPACES TO STATS-RECORD

       MOVE STAT-S-LAND(8) TO NUMBER-FORMAT
       STRING
           "NO: "
           FUNCTION TRIM(NUMBER-FORMAT)
           INTO STATS-RECORD
       END-STRING
       WRITE STATS-RECORD
       MOVE SPACES TO STATS-RECORD

       MOVE STAT-S-LAND(9) TO NUMBER-FORMAT
       STRING
           "IT: "
           FUNCTION TRIM(NUMBER-FORMAT)
           INTO STATS-RECORD
       END-STRING
       WRITE STATS-RECORD
       MOVE SPACES TO STATS-RECORD

       MOVE STAT-S-LAND(10) TO NUMBER-FORMAT
       STRING
           "CA: "
           FUNCTION TRIM(NUMBER-FORMAT)
           INTO STATS-RECORD
       END-STRING
       WRITE STATS-RECORD
       MOVE SPACES TO STATS-RECORD
       EXIT.

       CALC-TOP3-SUSCUS.
       IF PCT-TOTAL > STAT-TOP3-MATCH(1)
               MOVE STAT-TOP3-MATCH(2) TO STAT-TOP3-MATCH(3)
               MOVE STAT-TOP3-MATCH(1) TO STAT-TOP3-MATCH(2)
               MOVE PCT-TOTAL TO STAT-TOP3-MATCH(1)
               MOVE STAT-TOP3-CUSTOMER(2) TO STAT-TOP3-CUSTOMER(3)
               MOVE STAT-TOP3-CUSTOMER(1) TO STAT-TOP3-CUSTOMER(2)
               STRING
                   C-ID
                   ", "
                   C-NAME
                   INTO STAT-TOP3-CUSTOMER(1)
               END-STRING
           ELSE
               IF PCT-TOTAL > STAT-TOP3-MATCH(2)
                   MOVE STAT-TOP3-MATCH(2) TO STAT-TOP3-MATCH(3)
                   MOVE PCT-TOTAL TO STAT-TOP3-MATCH(2)
                   MOVE STAT-TOP3-CUSTOMER(2) TO STAT-TOP3-CUSTOMER(3)
                   STRING
                       C-ID
                       ", "
                       C-NAME
                       INTO STAT-TOP3-CUSTOMER(2)
                   END-STRING
               ELSE
                   IF PCT-TOTAL > STAT-TOP3-MATCH(3)
                       MOVE PCT-TOTAL TO STAT-TOP3-MATCH(3)
                       STRING
                           C-ID
                           ", "
                           C-NAME
                           INTO STAT-TOP3-CUSTOMER(3)
                       END-STRING
                   END-IF
               END-IF
           END-IF
       EXIT.

       CALC-TOP3-SANC.
       IF S-NO-MATCHES(S-IX) > STAT-TOP3-S-MATCH(1)
               MOVE STAT-TOP3-S-MATCH(2) TO STAT-TOP3-S-MATCH(3)
               MOVE STAT-TOP3-S-MATCH(1) TO STAT-TOP3-S-MATCH(2)
               MOVE S-NO-MATCHES(S-IX) TO STAT-TOP3-S-MATCH(1)
               MOVE STAT-TOP3-SANCTION(2) TO STAT-TOP3-SANCTION(3)
               MOVE STAT-TOP3-SANCTION(1) TO STAT-TOP3-SANCTION(2)
               STRING
                   S-IX
                   ", "
                   S-ALIASES(S-IX, 1)
                   INTO STAT-TOP3-SANCTION(1)
               END-STRING
           ELSE
               IF S-NO-MATCHES(S-IX) > STAT-TOP3-S-MATCH(2)
                   MOVE STAT-TOP3-S-MATCH(2) TO STAT-TOP3-S-MATCH(3)
                   MOVE S-NO-MATCHES(S-IX) TO STAT-TOP3-S-MATCH(2)
                   MOVE STAT-TOP3-SANCTION(2) TO STAT-TOP3-SANCTION(3)
                   STRING
                       S-IX
                       ", "
                       S-ALIASES(S-IX, 1)
                       INTO STAT-TOP3-SANCTION(2)
                   END-STRING
               ELSE
                   IF S-NO-MATCHES(S-IX) > STAT-TOP3-S-MATCH(3)
                       MOVE S-NO-MATCHES(S-IX) TO STAT-TOP3-S-MATCH(3)
                       STRING
                           S-IX
                           ", "
                           S-ALIASES(S-IX, 1)
                           INTO STAT-TOP3-SANCTION(3)
                       END-STRING
                   END-IF
               END-IF
           END-IF
       EXIT.
