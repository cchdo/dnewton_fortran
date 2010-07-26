      SUBROUTINE RGTIEH(DAT,QUAL,FN,TPREC,SPREC,FMT1,FMT2,FMT3,
     *                  RECIND,LINE)
C
C -- ROUTINE TO WRITE OUT A CORRECTLY FORMATTED IEH DATA LINE TO
C    THE INTERNAL FILE LINE.
c author: David Newton. Scripps Instn. of Oceanog.
C
      REAL DAT(22)
      CHARACTER FN*1,RECIND*1,LINE*128,FMT1*(*),FMT2*(*),FMT3*(*)
      INTEGER TPREC,SPREC,QUAL(22)
C
      REAL NULL,A
C
      INTRINSIC NINT
C
C -- INDEXES FOR DAT AND QUAL ARRAYS.
C
C     1 == DEPTH                      11 == CHLOR-A
C     2 == TEMPERATURE                12 == CAST NUMBER
C     3 == SALINITY                   13 == PHAEOPHYTIN
C     4 == PRESSURE                   14 == C14 UPTAKE 1
C     5 == OXYGEN                     15 == C14 UPTAKE 2
C     6 == PO4                        16 == C14 DARK UPTAKE
C     7 == SIO3                       17 == C14 MEAN UPTAKE
C     8 == NO2 (NITRITE)              18 == ELAPSED INCUBATION (HHMM.0)
C     9 == NO3 (NITRATE)              19 == LIGHT PERCENT
C    10 == NH3 (AMMONIA)              20 == WILD COL. 1
C                                     21 == WILD COL. 2
C                                     22 == WILD COL. 3
C
C
      DATA NULL/3.2E4/
C
 3001 FORMAT(I1)
 3002 FORMAT(I2)
 3003 FORMAT(I3)
 3004 FORMAT(I4)
 3005 FORMAT(I5)
 3006 FORMAT(I6)
C
      LINE = ' '
C
C -- DEPTH.
      A = DAT(1)
      IF(A .EQ. NULL) THEN
C         -- NOTHING
      ELSE
          WRITE(LINE(1:5),3005) NINT(A)
      ENDIF
C
C -- FOOTNOTE.
      LINE(6:6) = FN
C
C -- TEMPERATURE.
      A = DAT(2)
      IF(A .EQ. NULL) THEN
          LINE(13:13) = '9'
      ELSE
          IF(TPREC .EQ. 3) THEN
              WRITE(LINE(7:11),3005) NINT(A*1000.)
              LINE(12:12) = '3'
          ELSE IF(TPREC .EQ. 2) THEN
              WRITE(LINE(7:10),3004) NINT(A*100.)
              LINE(12:12) = '2'
          ELSE IF(TPREC .EQ. 1) THEN
              WRITE(LINE(7:9),3003) NINT(A*10.)
              LINE(12:12) = '1'
          ELSE
C             -- ASSUME 3 PLACE TEMP.
              WRITE(LINE(7:11),3005) NINT(A*1000.)
              LINE(12:12) = '3'
          ENDIF
          IF(QUAL(2) .NE. 0) WRITE(LINE(13:13),3001) QUAL(2)
      ENDIF
C
C -- SALINITY
      A = DAT(3)
      IF(A .EQ. NULL) THEN
          LINE(20:20) = '9'
      ELSE
          IF(SPREC .EQ. 3) THEN
              WRITE(LINE(14:18),3005) NINT(A*1000.)
              LINE(19:19) = '3'
          ELSE IF(SPREC .EQ. 2) THEN
              WRITE(LINE(14:17),3004) NINT(A*100.)
              LINE(19:19) = '2'
          ELSE IF(SPREC .EQ. 1) THEN
              WRITE(LINE(14:16),3003) NINT(A*10.)
              LINE(19:19) = '1'
          ELSE
C             -- ASSUME 3 PLACE SAL.
              WRITE(LINE(14:18),3005) NINT(A*1000.)
              LINE(19:19) = '3'
          ENDIF
          IF(QUAL(3) .NE. 0) WRITE(LINE(20:20),3001) QUAL(3)
      ENDIF
C
C -- PRESSURE.
      A = DAT(4)
      IF(A .EQ. NULL) THEN
          LINE(27:27) = '9'
      ELSE
          WRITE(LINE(21:26),3006) NINT(A*10.)
          IF(QUAL(4) .NE. 0) WRITE(LINE(27:27),3001) QUAL(4)
      ENDIF
C
C -- OXYGEN.
      A = DAT(5)
      IF(A .EQ. NULL) THEN
          LINE(32:32) = '9'
      ELSE
          WRITE(LINE(28:31),3004) NINT(A*100.)
          IF(QUAL(5) .NE. 0) WRITE(LINE(32:32),3001) QUAL(5)
      ENDIF
C
C -- PO4.
      A = DAT(6)
      IF(A .EQ. NULL) THEN
          LINE(37:37) = '9'
      ELSE
          WRITE(LINE(33:36),3004) NINT(A*100.)
          IF(QUAL(6) .NE. 0) WRITE(LINE(37:37),3001) QUAL(6)
      ENDIF
C
C -- SIO3 (SILICATE).
      A = DAT(7)
      IF(A .EQ. NULL) THEN
          LINE(42:42) = '9'
      ELSE
          WRITE(LINE(38:41),3004) NINT(A*10.)
          IF(QUAL(7) .NE. 0) WRITE(LINE(42:42),3001) QUAL(7)
      ENDIF
C
C -- NO2 (NITRITE)
      A = DAT(8)
      IF(A .EQ. NULL) THEN
          LINE(47:47) = '9'
      ELSE
          WRITE(LINE(43:46),3004) NINT(A*100.)
          IF(QUAL(8) .NE. 0) WRITE(LINE(47:47),3001) QUAL(8)
      ENDIF
C
C -- NO3 (NITRATE).
      A = DAT(9)
      IF(A .EQ. NULL) THEN
          LINE(51:51) = '9'
      ELSE
          WRITE(LINE(48:50),3003) NINT(A*10.)
          IF(QUAL(9) .NE. 0) WRITE(LINE(51:51),3001) QUAL(9)
      ENDIF
C
C -- NH3 (AMMONIA).
      A = DAT(10)
      IF(A .EQ. NULL) THEN
          LINE(56:56) = '9'
      ELSE
          WRITE(LINE(52:55),3004) NINT(A*100.)
          IF(QUAL(10) .NE. 0) WRITE(LINE(56:56),3001) QUAL(10)
      ENDIF
C
C -- CHLOR-A
      A = DAT(11)
      IF(A .EQ. NULL) THEN
          LINE(61:61) = '9'
      ELSE
          WRITE(LINE(57:60),3004) NINT(A*100.)
          IF(QUAL(11) .NE. 0) WRITE(LINE(61:61),3001) QUAL(11)
      ENDIF
C
C -- CAST NUMBER
      A = DAT(12)
      IF(A .EQ. NULL) THEN
C         -- NOTHING
      ELSE
          WRITE(LINE(62:63),3002) NINT(A)
      ENDIF
C
      LINE(64:65) = 'Z*'
C
C -- PHAEO.
      A = DAT(13)
      IF(A .EQ. NULL) THEN
          LINE(70:70) = '9'
      ELSE
          WRITE(LINE(66:69),3004) NINT(A*100.)
          IF(QUAL(13) .NE. 0) WRITE(LINE(70:70),3001) QUAL(13)
      ENDIF
C
C -- C14 UPTAKE 1
      A = DAT(14)
      IF(A .EQ. NULL) THEN
          LINE(77:77) = '9'
      ELSE
          IF(A .LT. 1.0) THEN
              WRITE(LINE(71:75),3005) NINT(A*100.)
              LINE(76:76) = '2'
          ELSE
              WRITE(LINE(71:74),3004) NINT(A*10.)
              LINE(76:76) = '1'
          ENDIF
          IF(QUAL(14) .NE. 0) WRITE(LINE(77:77),3001) QUAL(14)
      ENDIF
C
C -- C14 UPTAKE 2.
      A = DAT(15)
      IF(A .EQ. NULL) THEN
          LINE(84:84) = '9'
      ELSE
          IF(A .LT. 1.0) THEN
              WRITE(LINE(78:82),3005) NINT(A*100.)
              LINE(83:83) = '2'
          ELSE
              WRITE(LINE(78:81),3004) NINT(A*10.)
              LINE(83:83) = '1'
          ENDIF
          IF(QUAL(15) .NE. 0) WRITE(LINE(84:84),3001) QUAL(15)
      ENDIF
C
C -- DARK C14 UPTAKE.
      A = DAT(16)
      IF(A .EQ. NULL) THEN
          LINE(89:89) = '9'
      ELSE
          IF(A .LT. 1.0) THEN
              WRITE(LINE(85:87),3003) NINT(A*100.)
              LINE(88:88) = '2'
          ELSE
              WRITE(LINE(85:86),3002) NINT(A*10.)
              LINE(88:88) = '1'
          ENDIF
          IF(QUAL(16) .NE. 0) WRITE(LINE(89:89),3001) QUAL(16)
      ENDIF
C
C -- MEAN C14 UPTAKE.
      A = DAT(17)
      IF(A .EQ. NULL) THEN
          LINE(96:96) = '9'
      ELSE
          IF(A .LT. 1.0) THEN
              WRITE(LINE(90:94),3005) NINT(A*100.)
              LINE(95:95) = '2'
          ELSE
              WRITE(LINE(90:93),3004) NINT(A*10.)
              LINE(95:95) = '1'
          ENDIF
          IF(QUAL(17) .NE. 0) WRITE(LINE(96:96),3001) QUAL(17)
      ENDIF
C
C -- ELAPSED TIME.
      A = DAT(18)
      IF(A .EQ. NULL) THEN
C         -- NOTHING
      ELSE
          WRITE(LINE(97:100),'(I4.4)') NINT(A)
      ENDIF
C
C -- LIGHT PERCENT.
      A = DAT(19)
      IF(A .EQ. NULL) THEN
C         -- NOTHING
      ELSE
          IF(A .LT. 1.0) THEN
              WRITE(LINE(101:103),'(F3.2)') A
          ELSE
              WRITE(LINE(101:103),3003) NINT(A*10.)
          ENDIF
      ENDIF
C
C -- WILD COL. 1
      A = DAT(20)
      IF(A .EQ. NULL) THEN
          LINE(111:111) = '9'
      ELSE
          WRITE(LINE(104:110),FMT1) A
          IF(QUAL(20) .NE. 0) WRITE(LINE(111:111),3001) QUAL(20)
      ENDIF
C
C -- WILD COL. 2
      A = DAT(21)
      IF(A .EQ. NULL) THEN
          LINE(119:119) = '9'
      ELSE
          WRITE(LINE(112:118),FMT2) A
          IF(QUAL(21) .NE. 0) WRITE(LINE(119:119),3001) QUAL(21)
      ENDIF
C
C -- WILD COL. 3
      A = DAT(22)
      IF(A .EQ. NULL) THEN
          LINE(127:127) = '9'
      ELSE
          WRITE(LINE(120:126),FMT3) A
          IF(QUAL(22) .NE. 0) WRITE(LINE(127:127),3001) QUAL(22)
      ENDIF
C
C -- RECORD INDICATOR.
C
      LINE(128:128) = RECIND
C
      RETURN
      END
