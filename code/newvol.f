      REAL FUNCTION SPVOLY(SS,TT,PP)
C
C  ********  FUNCTION SPVOLY  ---CALCULATES THE SPECIFIC
C  ********  VOLUME ANOMALY IN 10**2 GM/L AS A FUNCTION OF SALINITY
C  ********  IN PPT, POTENTIAL TEMPERATURE IN DEGREES C AND TEMPERATURE IN
C  ********  DEGREES C.
C
C
C -- NOTE: POTENTIAL TEMPERATURE NO LONGER USED.  DMN 10/22/84
C
C
C                       CALLED USER ROUTINES
C
C                               ALPHY
C
C
      REAL SS,TT,PP
      REAL S,T,P,CDMISS
      REAL ALPHY
      EXTERNAL ALPHY
C
      DATA CDMISS/3.2E4/
      S=SS
      T=TT
      P=PP
      IF ((S.EQ.CDMISS).OR.(T.EQ.CDMISS).OR.(P.EQ.CDMISS))  GO TO 100
      SPVOLY=(ALPHY(S, T, P)-ALPHY(35.0, 0., P))*1E5
 50   RETURN
 100  SPVOLY=CDMISS
      GO TO 50
      END
C
      REAL FUNCTION ALPHY(SS,TT,PP)
C
C  ********  FUNCTION ALPHA  ---THIS IS THE SPECIFIC VOLUME
C  ********  CALCULATION PROCEDURE, AND CALCULATES THE SPECIFIC
C  ********  VOLUME AS A FUNCTION OF PRESSURE, POTENTIAL TEMPERATURE,
C  ********  AND SALINITY.
C
C -- NOTE: TEMPERATURE USED INSTEAD OF POT. TEMP.  DMN 10/22/84
C
C
C                       CALLED USER ROUTINES
C
C                                SIGMP
C
C
      REAL SS,TT,PP
      REAL SX,PX,SIGSTP,CDMISS
      EXTERNAL SIGMP
      DATA CDMISS /3.2E4/
      SX=SS
      PX=PP
      CALL SIGMP(PX,PX,TT,SX,SIGSTP)
      IF (SIGSTP.EQ.CDMISS) GO TO 100
      ALPHY=1./(SIGSTP*1.E-3+1)
 50   RETURN
 100  ALPHY=CDMISS
      GO TO 50
      END
C ********************************************************
      SUBROUTINE POTMP(PRESS,TEMP,S,RP,POTEMP)
C
C     TITLE:
C     *****
C
C       POTMP  -- CALCULATE POTENTIAL TEMPERATURE FOR AN ARBITRARY
C                 REFERENCE PRESSURE
C
C     PURPOSE:
C     *******
C
C       TO CALCULATE POTENTIAL TEMPERATURE
C
C       REF: N.P. FOFONOFF
C            DEEP SEA RESEARCH
C            IN PRESS NOV 1976
C
C     PARAMETERS:
C     **********
C
C       PRESS-- PRESSURE IN DECIBARS
C       TEMP -- TEMPERATURE IN CELSIUS DEGREES
C       S    -- SALINITY PSS 78
C       RP   -- REFERENCE PRESSURE IN DECIBARS
C               (0.0 FOR STANDARD POTENTIAL TEMPERATURE)
C
C       POTEMP- POTENTIAL TEMPERATURE
C
        REAL PRESS,TEMP,S,RP,POTEMP
C
C     VARIABLES:
C     *********
C
      REAL S1,P,T,DP,R1,R2,R3,R4,R5,X,Q
      INTEGER N,I,J
C
C     SUBPROGRAMS:
C     ***********
C
      INTRINSIC INT,ABS,REAL
C
C     CONSTANTS:
C     *********
C
C       /*  MISSING DATA CODE  */
C
      REAL CDMISS
      DATA CDMISS /3.2E4/
C
C     CODE:
C     ****
C
      S1 = S-35.0
      P  = PRESS
      T  = TEMP
C
C      /*  CHECK FOR MISSING DATA AS PARAMETERS  */
C
      IF (S.EQ.CDMISS.OR.P.EQ.CDMISS.OR.T.EQ.CDMISS) GO TO 40
C
      DP = RP - P
      N  = INT(ABS(DP)/1000.) + 1
      DP = DP/REAL(N)
C
      DO 10 I=1,N
         DO 20 J=1,4
C
            R1 = ((-2.1687E-16*T+1.8676E-14)*T-4.6206E-13)*P
            R2 = (2.7759E-12*T-1.1351E-10)*S1
            R3 = ((-5.4481E-14*T+8.733E-12)*T-6.7795E-10)*T
            R4 = (R1+(R2+R3+1.8741E-8))*P+(-4.2393E-8*T+1.8932E-6)*S1
            R5 = R4+((6.6228E-10*T-6.836E-8)*T+8.5258E-6)*T+3.5803E-5
C
            X  = DP*R5
C
            GO TO (100,200,300,400),J
C
 100        CONTINUE
            T = T+.5*X
            Q = X
            P = P + .5*DP
            GO TO 20
C
 200        CONTINUE
            T = T + .29289322*(X-Q)
            Q = .58578644*X + .121320344*Q
            GO TO 20
C
 300        CONTINUE
            T = T + 1.707106781*(X-Q)
            Q = 3.414213562*X - 4.121320344*Q
            P = P + .5*DP
            GO TO 20
C
 400        CONTINUE
            T = T + (X-2.0*Q)/6.0
 20       CONTINUE
 10     CONTINUE
C
        POTEMP = T
 30     RETURN
C
C       /*  SOME INPUT PARAMETER IS MISSING   */
C
 40   POTEMP=CDMISS
      GO TO 30
C
C       END POTMP
C
        END
      SUBROUTINE SIGMP(REFPRS,PRESS,TEMP,SALTY,SIGMA)
C
C     TITLE:
C     *****
C
C       SIGMP  -- CALCULATE DENSITY USING INTERNATIONAL EQUATION OF
C                 STATE
C
C                 FROM TEXT FURNISHED BY J. GIESKES
C
C     PARAMETERS:
C     **********
C
C       PRESS  -- PRESSURE IN DECIBARS
C       TEMP   -- TEMPERATURE IN CELSIUS DEGREES
C       SALTY  -- SALINITY PSS 78
C       REFPRS -- REFERENCE PRESSURE
C                 REFPRS = 0. : SIGMA THETA
C                 REFPRS = PRESS: SIGMA Z
C       SIGMA  -- Kg/M*3 - 1000.0
C
        REAL PRESS,TEMP,SALTY,REFPRS,SIGMA
C
C     VARIABLES:
C     *********
C
        REAL KST0,KW,RHOW,KSTP,POTEMP,TERMA,TERMB,BARS
C
C     SUBPROGRAMS:
C     ***********
C
      EXTERNAL POTMP
      INTRINSIC ABS
C
C     CONSTANTS:
C     *********
C       /*  MISSING DATA CODE  */
C
      REAL CDMISS
      DATA CDMISS /3.2E4/
C
C     CODE:
C     ****
C
C
C      /*  CHECK FOR MISSING DATA  */
C
      IF (TEMP.EQ.CDMISS.OR.PRESS.EQ.CDMISS.OR.SALTY.EQ.CDMISS) GO TO 20
C     /* CALCULATE POTENTIAL TEMPERATURE */
C
      POTEMP = TEMP
      IF(PRESS.NE.REFPRS) CALL POTMP(PRESS,TEMP,SALTY,REFPRS,POTEMP)
C
C     /* REFERENCE PRESSURE IN BARS */
C
      BARS = REFPRS*.10
C
C     /* CALCULATE DENSITY AT GIVEN SALINITY, TEMP AND PRESSURE=0.0 */
C
      RHOW = -.00909529+POTEMP*(1.001685E-4+POTEMP*(-1.120083E-6+
     1       POTEMP*6.536332E-9))
      RHOW = 999.842594+POTEMP*(.06793952+POTEMP*RHOW)
C       /* RHOW = DENSITY OF PURE WATER Kg/M**3 */
C
      KW  = POTEMP*(-.0040899+POTEMP*(7.6438E-5+POTEMP*(-8.2467E-7
     1       +POTEMP*5.3875E-9)))
C           /* PURE WATER SECANT BULK MODULUS */
C
      KW  = (KW+.824493)*SALTY
C
      KST0 = (-.00572466+POTEMP*(1.0227E-4+POTEMP*(-1.6546E-6)))
     1       * ABS(SALTY)**1.5
C       /* K(S,T,0) */
C
      SIGMA= RHOW + KW + KST0 + 4.8314E-4*SALTY*SALTY
C       /* SIGMA THETA Kg/M**3 */
C
C     SELECT  /* IF REFERENCE PRESSURE=0.0, DONE */
C       (REFERENCE PRESSURE = 0.0):
          IF(REFPRS.EQ.0.0) GO TO 999
C         /* NO ACTION */
C       (OTHRWISE):  /* CALCULATE PRESSURE EFFECT */
C
C         /* RHO(S,T,0)/(1.0-P/K(S,T,P)) */
C
         KW = POTEMP*(148.4206+POTEMP*(-2.327105+POTEMP
     1        *(.01360477+POTEMP*(-5.155288E-5))))
         KW = KW + 19652.21
C
         KST0 = (54.6746+POTEMP*(-.603459+POTEMP*(.0109987+POTEMP
     1          *(-6.167E-5))))*SALTY
C
         KST0 = KST0 + KW+(.07944+POTEMP*(.016483+POTEMP*(-5.3009E-4)))
     1          *ABS(SALTY)**1.5
C
C        /* CALCULATE PRESSURE TERMS */
C
         TERMA = 3.239908+POTEMP*(.00143713+POTEMP*(1.16092E-4+POTEMP
     1           *(-5.77905E-7)))
         TERMA = TERMA + (.0022838+POTEMP*(-1.0981E-5+POTEMP
     1                 *(-1.6078E-6)))*SALTY
         TERMA = TERMA + 1.91075E-4*ABS(SALTY)**1.5
C
         TERMB = 8.50935E-5+POTEMP*(-6.12293E-6+POTEMP*5.2787E-8)
         TERMB = TERMB + (-9.9348E-7+POTEMP*(2.0816E-8+POTEMP
     1                 * 9.1697E-10))*SALTY
C
         KSTP  = KST0+BARS*(TERMA + BARS*TERMB)
C          /* SECANT BULK MODULUS K(S,T,P) */
C
         SIGMA = SIGMA/(1.0-BARS/KSTP)
C
C     END SELECT
C
 999  CONTINUE
C
      SIGMA  = SIGMA - 1000.
C
 10   RETURN
C
C      /*  INPUT DATA IS MISSING   */
C
 20   SIGMA = CDMISS
      GO TO 10
C
C     END SIGMP
C
      END
