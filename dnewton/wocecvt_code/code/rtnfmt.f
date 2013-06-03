      SUBROUTINE RTNFMT(CARD,SPEC,FMT,*)
C -- BECAUSE READING AN INTERNAL FILE USING LIST DIRECTED INPUT IS
C    A NON-ANSI EXTENSION THIS ROUTINE WAS DEVELOPED TO CIRCUMVENT
C    THE RESTRICTION. "RTNFMT" RETURNS A RUNTIME FORMAT GIVEN THE
C    INTERNAL FILE, AND A SPECIFICATION OF THE DATA TYPES YOU EXPECT.
C    THE RETURNED RUNTIME FORMAT CAN THEN BE USED TO READ THE INTERNAL
C    FILE.
C author: David Newton. Scripps Instn. of Oceanog.
C
      CHARACTER CARD*(*),SPEC*(*),FMT*(*)
      CHARACTER C1*1,C2*2,C3*3,C4*4,C5*5,C6*6,PF*1
      INTEGER SON,NCIC,NCIS,NCIF,I,J,IRB,PIF,FW
      LOGICAL SEPER
      INTRINSIC LEN
C
C -- SON == START OF NUMBER       PIF == POSITION IN FORMAT
C    NCIC == NO. OF CHARS. IN CARD    NCIS == NO. CHARS. IN SPEC
C    NCIF == NO. OF CHARS. IN FMT     FW == FIELD WIDTH
C
      SEPER(C1) = C1 .EQ. ',' .OR. C1 .EQ. ' '
C
C
      NCIC = LEN(CARD)
      NCIS = LEN(SPEC)
      NCIF = LEN(FMT)
C
C -- BLANK OUT THE "FMT" STRING.
C
      DO 5 I=1,NCIF
         FMT(I:I) = ' '
 5    CONTINUE
C
C -- SPECIFICATION FOR EXPECTED FORMATS MUST HAVE "(" AT START AND ")"
C    AT END.  I.E.   '(FFIFIA)'
C
      IF(SPEC(1:1) .NE. '(') GO TO 200
      DO 10 I=2,NCIS
         IRB = I
         IF(SPEC(I:I) .EQ. ')' ) GO TO 15
 10   CONTINUE
C     -- NO ENDING ")"
      GO TO 200
C
 15   IRB = IRB - 1
      FMT(1:1) = '('
      PIF = 2
      I = 0
      DO 100 J=2,IRB
         PF = SPEC(J:J)
         IF(PF .EQ. 'F') THEN
C            -- WORKING WITH A REAL NUMBER. FIND ITS BEGINNING.
 20          I = I + 1
             IF(I .GT. NCIC) GO TO 300
             IF(SEPER(CARD(I:I))) GO TO 20
C            -- FOUND START OF NUMBER.
             SON = I
             IF(J .GT. 2) THEN
                 FMT(PIF:PIF) = ','
                 PIF = PIF + 1
                 IF(PIF .GE. NCIF) GO TO 300
             ENDIF
C
             IF(SON .GT. 99) THEN
                 WRITE(C6,'(''T'',I3,'',F'')') SON
                 FMT(PIF:PIF+5) = C6
                 PIF = PIF + 6
                 IF(PIF .GE. NCIF) GO TO 300
             ELSE IF(SON .GT. 9) THEN
                 WRITE(C5,'(''T'',I2,'',F'')') SON
                 FMT(PIF:PIF+4) = C5
                 PIF = PIF + 5
                 IF(PIF .GE. NCIF) GO TO 300
             ELSE
                 WRITE(C4,'(''T'',I1,'',F'')') SON
                 FMT(PIF:PIF+3) = C4
                 PIF = PIF + 4
                 IF(PIF .GE. NCIF) GO TO 300
             ENDIF
C
C            -- NOW FIND THE WIDTH OF THE NUMBER.
C
 25          I = I + 1
CC             IF(I .GT. NCIC) GO TO 300
             IF(I .LE. NCIC) THEN
                 IF(.NOT. SEPER(CARD(I:I))) GO TO 25
             ENDIF
             FW = I - SON
             I = I - 1
C
             IF(FW .GT. 9) THEN
                 WRITE(C4,'(I2,''.0'')') FW
                 FMT(PIF:PIF+3) = C4
                 PIF = PIF + 4
                 IF(PIF .GE. NCIF) GO TO 300
             ELSE IF(FW .GT. 0) THEN
                 WRITE(C3,'(I1,''.0'')') FW
                 FMT(PIF:PIF+2) = C3
                 PIF = PIF + 3
                 IF(PIF .GE. NCIF) GO TO 300
             ELSE
                 WRITE(*,*)' FIELD WIDTH MUST BE > 0'
                 GO TO 300
             ENDIF
C
         ELSE IF(PF .EQ. 'I') THEN
C            -- WORKING WITH AN INTEGER. FIND ITS BEGINNING.
 30          I = I + 1
             IF(I .GT. NCIC) GO TO 300
             IF(SEPER(CARD(I:I))) GO TO 30
             SON = I
             IF(J .GT. 2) THEN
                 FMT(PIF:PIF) = ','
                 PIF = PIF + 1
                 IF(PIF .GE. NCIF) GO TO 300
             ENDIF
C
             IF(SON .GT. 99) THEN
                 WRITE(C6,'(''T'',I3,'',I'')') SON
                 FMT(PIF:PIF+5) = C6
                 PIF = PIF + 6
                 IF(PIF .GE. NCIF) GO TO 300
             ELSE IF(SON .GT. 9) THEN
                 WRITE(C5,'(''T'',I2,'',I'')') SON
                 FMT(PIF:PIF+4) = C5
                 PIF = PIF + 5
                 IF(PIF .GE. NCIF) GO TO 300
             ELSE
                 WRITE(C4,'(''T'',I1,'',I'')') SON
                 FMT(PIF:PIF+3) = C4
                 PIF = PIF + 4
                 IF(PIF .GE. NCIF) GO TO 300
             ENDIF
C
C            -- NOW FIND THE INTEGER'S FIELD WIDTH.
C
 35          I = I + 1
CC             IF(I .GT. NCIC) GO TO 300
             IF(I .LE. NCIC) THEN
                 IF(.NOT. SEPER(CARD(I:I))) GO TO 35
             ENDIF
             FW = I - SON
             I = I - 1
             IF(FW .GT. 9) THEN
                 WRITE(C2,'(I2)') FW
                 FMT(PIF:PIF+1) = C2
                 PIF = PIF + 2
                 IF(PIF .GE. NCIF) GO TO 300
             ELSE IF(FW .GT. 0) THEN
                 WRITE(C1,'(I1)') FW
                 FMT(PIF:PIF) = C1
                 PIF = PIF + 1
                 IF(PIF .GE. NCIF) GO TO 300
             ELSE
                 WRITE(*,*)' FIELD WIDTH MUST BE > 0'
                 GO TO 300
             ENDIF
C
         ELSE IF(PF .EQ. 'A') THEN
C            -- WORKING WITH AN ALPHANUMERIC. FIND ITS BEGINNING.
 50          I = I + 1
             IF(I .GT. NCIC) GO TO 300
             IF(CARD(I:I) .EQ. ' ') GO TO 50
             SON = I
             IF(J .GT. 2) THEN
                 FMT(PIF:PIF) = ','
                 PIF = PIF + 1
                 IF(PIF .GE. NCIF) GO TO 300
             ENDIF
C
             IF(SON .GT. 99) THEN
                 WRITE(C6,'(''T'',I3,'',A'')') SON
                 FMT(PIF:PIF+5) = C6
                 PIF = PIF + 6
                 IF(PIF .GE. NCIF) GO TO 300
             ELSE IF(SON .GT. 9) THEN
                 WRITE(C5,'(''T'',I2,'',A'')') SON
                 FMT(PIF:PIF+4) = C5
                 PIF = PIF + 5
                 IF(PIF .GE. NCIF) GO TO 300
             ELSE
                 WRITE(C4,'(''T'',I1,'',A'')') SON
                 FMT(PIF:PIF+3) = C4
                 PIF = PIF + 4
                 IF(PIF .GE. NCIF) GO TO 300
             ENDIF
C
C            -- NOW FIND THE STRINGS'S FIELD WIDTH.
C
 55          I = I + 1
CC             IF(I .GT. NCIC) GO TO 300
             IF(I .LE. NCIC) THEN 
                 IF(CARD(I:I) .NE. ' ') GO TO 55
             ENDIF
             FW = I - SON
             I = I - 1
             IF(FW .GT. 9) THEN
                 WRITE(C2,'(I2)') FW
                 FMT(PIF:PIF+1) = C2
                 PIF = PIF + 2
                 IF(PIF .GE. NCIF) GO TO 300
             ELSE IF(FW .GT. 0) THEN
                 WRITE(C1,'(I1)') FW
                 FMT(PIF:PIF) = C1
                 PIF = PIF + 1
                 IF(PIF .GE. NCIF) GO TO 300
             ELSE
                 WRITE(*,*)' FIELD WIDTH MUST BE > 0'
                 GO TO 300
             ENDIF
C
C
         ELSE
             WRITE(*,2000) PF
 2000        FORMAT(' SORRY CAN''T DO "',A1,'" FORMAT IN S/R RTNFMT')
             STOP 99
         ENDIF
C
 100  CONTINUE
C
C -- NORMAL RETURN. JUST ADD CLOSING PAREN.
C
      FMT(NCIF:NCIF) = ')'
      RETURN
C
C -- ERROR SECTION
C
 200  WRITE(*,*)' ERR IN "SPEC" (PARENS)'
C
C -- TAKE THE ALTERNATE RETURN.
C
 300  CONTINUE
CC      WRITE(*,*)' I,NCIC=',I,NCIC,' PIF,NCIF=',PIF,NCIF
CC      WRITE(*,*)' SPEC=',SPEC,' FMT=',FMT
CC      WRITE(*,*)' CARD=',CARD
      RETURN 1
      END
