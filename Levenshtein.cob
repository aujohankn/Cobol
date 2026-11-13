       IDENTIFICATION DIVISION.
       PROGRAM-ID. LEVDIST.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       FILE SECTION.


      

       WORKING-STORAGE SECTION.
              

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
       01 IX PIC 9(2) VALUE ZEROS.
       
       01 M PIC 9(2) VALUE ZEROS.
       01 N PIC 9(2) VALUE ZEROS.

       01 DISPLAYSTRING PIC X(50) VALUE SPACES.
       
       PROCEDURE DIVISION.
      
       MOVE "kitten" TO L-STR1
       MOVE "sitting" TO L-STR2
       
       DISPLAY L-STR1 ", " L-STR2
       PERFORM LEVENSHTEIN-DISTANCE
       DISPLAY "L-DIST: " L-RESULT

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

           DISPLAY "M: " M ", N: " N

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
               DISPLAY "Initialize the first.. " L-CURR-ROW(1)

               MOVE 02 TO J
               PERFORM VARYING J FROM 2 BY 1 UNTIL J > N + 1
                   MOVE J TO J-1
                   SUBTRACT 1 FROM J-1
                   
                   IF L-STR1(I-1:1) = L-STR2(J-1:1)
                       DISPLAY "CHARACTERS MATCH" J-1 ", " L-STR2(J-1:1)
                       MOVE L-PREV-ROW(J-1) TO L-CURR-ROW(J)
                   ELSE
                       DISPLAY "CHOOSE MIN"
                       DISPLAY L-CURR-ROW(J-1)
                       DISPLAY L-PREV-ROW(J)
                       DISPLAY L-PREV-ROW(J-1)
                       MOVE
                       FUNCTION MIN(L-CURR-ROW(J-1),
                           L-PREV-ROW(J), L-PREV-ROW(J-1))
                       TO L-CURR-ROW(J)
                       ADD 1 TO L-CURR-ROW(J)
                       DISPLAY L-CURR-ROW(J) " " J
                   END-IF
               END-PERFORM

               MOVE 01 TO IX
               PERFORM VARYING IX FROM 1 BY 1
               UNTIL IX > N + 1
                   MOVE L-CURR-ROW(IX) TO L-PREV-ROW(IX)
                   DISPLAY L-CURR-ROW(IX) ", " IX
               END-PERFORM
               STRING "["
                   L-PREV-ROW(1) ", "
                   L-PREV-ROW(2) ", "
                   L-PREV-ROW(3) ", "
                   L-PREV-ROW(4) ", "
                   L-PREV-ROW(5) ", "
                   L-PREV-ROW(6) ", "
                   L-PREV-ROW(7) ", "
                   L-PREV-ROW(8)
                   "]"
                   INTO DISPLAYSTRING
               DISPLAY DISPLAYSTRING
               MOVE SPACES TO DISPLAYSTRING
               
           END-PERFORM

           MOVE L-CURR-ROW(L-STR2-LENGTH + 1) TO L-RESULT

       EXIT.
