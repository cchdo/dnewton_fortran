      SUBROUTINE PZCON(CNVRSN,GRAV,DYNZ,PRSDB,DEPTH)
C
C    TITLE:
C    *****
C     PZCON  -- CONVERT PRESSURE IN DECIBARS TO DEPTH IN METERS
C               (OR VISA VERSA)
C
C    SOURCE:
C    ******
C     PACODF HYDROGRAPHIC DATA LIBRARY
C
C     NOTE: THIS COPY TRANSLATED TO ANSI STANDARD FORTRAN 77  BY D.M. NEWTON
C           FOR J.REID.
C
C    PURPOSE:
C    *******
C     TO CALCULATE DEPTH IN METERS (MANTYLA,SAUNDERS)
C     FROM PRESSURE IN DECIBARS (OR PRESSURE FROM DEPTH).
C
C     REF: JOURN. PHYSICAL OCEAN.,VOL 11 NO. 4, APRIL 1981.
C           (SAUNDERS)
C          PRIVATE CORRESPONDENCE 1982-1983
C           (MANTYLA)
C
C    METHOD:
C    ******
C     A STANDARD OCEAN (SAL=35.0,T=0.0) IS USED PLUS A DYNAMIC
C     HEIGHT CORRECTION TO ACCOUNT FOR DEVIATIONS FROM THE STANDARD
C     OCEAN. PRESSURE TO DEPTH CONVERSION IS EFFECTED AS:
C
C     Z = P/(B*C) + DYNZ/B
C
C     WHERE:
C      P = IN SITU PRESSURE (DECIBARS)
C      B = IN SITU GRAVITY AS A FUNCTION OF LATITUDE AND DEPTH
C          DEPTH APPROXIMATED FROM PRESSURE. GRAVITY IN DECIMETERS/SEC**2
C      C = IN SITU MEAN DENSITY RHO(35.0,0.0,P)
C      DYNZ = DYNAMIC DEPTH IN DYNAMIC METERS
C      Z = DEPTH IN METERS
C
C      PARAMETERS:
C      **********
C       CNVRSN -> CONVERSION TO BE PERFORMED
C                 'D' = PRESSURE TO DEPTH
C                 'P' = DEPTH TO PRESSURE
C       GRAV   -> ACCELERATION OF GRAVITY (DECIMETERS/SEC**2) AT
C                 STATION LATITUDE AT THE SURFACE.
C       DYNZ   -> DYNAMIC DEPTH IN DYNAMIC METERS (0.0 AT SURFACE)
C       PRSDB  -> PRESSURE IN DECIBARS (CNVRSN='D')
C              <- PRESSURE IN DECIBARS (CNVRSN='P')
C       DEPTH  <- DEPTH IN METERS (CNVRSN='D')
C              -> DEPTH IN METERS (CNVRSN='P')
C
      CHARACTER CNVRSN*1
      REAL GRAV,DYNZ,PRSDB,DEPTH
C
C      VARIABLES:
C      *********
C
      REAL A,B,C
      DOUBLE PRECISION DD,DG,DH,DA,DB,DC
C
C      CONSTANTS:
C      *********
C
      REAL NULL
      PARAMETER (NULL=32000.)
C
C      FUNCTIONS:
C      *********
C
      INTRINSIC DBLE,SNGL,DSQRT
C
C  -- AND AT LAST THE CODE.
C
      IF(CNVRSN .EQ. 'D') THEN
C         -- DEPTH CORRECTION FOR GRAVITY (FROM PRESSURE)
          A = 2.2E-6 * PRSDB
C         -- IN SITU GRAVITY
          B = GRAV + 0.05 * A
C         -- IN SITU MEAN DENSITY RHO(35.0,0.0,P)
          C = 1.0285 + A
C         -- PRESSURE TO DEPTH CONVERSION FOR STANDARD OCEAN
          DEPTH = PRSDB / (B*C)
C
          IF(DYNZ .NE. NULL) THEN
C             -- DYNAMIC DEPTH CORRECTION
              DEPTH = DEPTH + DYNZ/B
          ENDIF
C
      ELSE IF(CNVRSN .EQ. 'P') THEN
          DD = DBLE(DEPTH)
          DG = DBLE(GRAV)
          DH = DBLE(DYNZ)
          DA = 2.4D-13 * DD
          DB = DD * (1.13135D-7 + 2.2D-6*DG) - 1.0D0
          DC = DG * DD
C
          IF(DYNZ .NE. NULL) THEN
              DB = DB - 2.2D-6 * DH
              DC = DC - DH
          ENDIF
C
          DC = 1.0285D0 * DC
C
          PRSDB = 0.0
          IF(DA .NE. 0.0D0) THEN
              PRSDB = SNGL((-DB-DSQRT(DB*DB-4.0D0*DA*DC))/(DA+DA))
          ENDIF
      ELSE
          WRITE(*,*)' INCORRECT CNVRSN ARGUMENT TO PZCON ****'
          STOP 99
      ENDIF
C
      RETURN
C
      END
C
C **********************************************************************
C
      REAL FUNCTION GRAV(LAT)
C
C -- THIS ROUTINE CALCULATES SURFACE GRAVITY IN DECIMETERS/SEC**2
C    GIVEN LATITUDE.
C    FOR PURPOSES OF PRESSURE TO DEPTH CONVERSION THE LATITUDE NEED
C    ONLY BE WITHIN 2 DEGREES OF THE ACTUAL LATITUDE. ADDITIONAL TERMS
C    IN THE LATITUDE TO GRAVITY FORMULA ARE IGNORED.
C
      REAL LAT,DEGRAD,SLATSQ
      PARAMETER (DEGRAD=.0174532925)
      INTRINSIC SIN
C
      SLATSQ = SIN(LAT*DEGRAD)
      SLATSQ = SLATSQ * SLATSQ
      GRAV = .9780318 + 5.1859E-3 * SLATSQ
      RETURN
C
      END