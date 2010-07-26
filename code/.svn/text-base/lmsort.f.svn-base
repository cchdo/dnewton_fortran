      SUBROUTINE LMSORT(KEY,N,INDX)
C
C -- ROUTINE TO DO A "LIST MERGE" SORT.
C     THIS ALGORITHM ONE OF THE FASTEST STABLE SORTS. 
C       ADDITIONALLY IT TAKES ADVANTAGE OF ANY EXISTING ORDER.
C     
C    KEY    ->  INTEGER ARRAY TO BE SORTED. RETURNED UNCHANGED.
C    N      ->  NUMBER OF ELEMENTS IN KEY.
C    INDX  <->  INTEGER ARRAY.  INDX(1)=1,INDX(2)=2,INDX(3)=3,...
C                INDX(N)=N   PRIOR TO CALLING THEN   
C                (KEY(INDX(I),I=1,N)) WOULD PRODUCE THE SORTED LIST IN
C                 ASCENDING ORDER.
C          ** INDX MUST BE INITIALIZED PRIOR TO CALLING THIS ROUTINE **
C
C     REF:  D.E. KNUTH, THE ART OF COMPUTER PROGRAMMING, VOL. 3 SORTING
C             AND SEARCHING, (READING, MASS.: ADDISON-WESLEY PUBLISHING
C             CO., 1973)
C
      INTEGER N,KEY(N),INDX(N)
C
      INTEGER MAX
      PARAMETER (MAX=10001)
      INTEGER L(0:MAX),I,K,P,Q,S,T
C
      INTRINSIC SIGN
C
      IF(N .GT. MAX-1) THEN
          WRITE(*,*)' TOO MANY KEYS TO SORT IN SR LMSORT.'
          WRITE(*,*)' N=',N,'MAX=',MAX-1
          STOP 99
      ENDIF
C
C -- SET UP THE LINK FIELDS.
C     LOOKING FOR PORTIONS THAT ARE ALREADY ASCENDING.
C
      L(0) = 1
      T = N + 1
      DO 10 P=1,N-1
         IF(KEY(INDX(P)) .LE. KEY(INDX(P+1))) THEN
             L(P) = P + 1
         ELSE
             L(T) = -(P+1)
             T = P
         ENDIF
 10   CONTINUE
      L(T) = 0
      L(N) = 0
      L(N+1) = ABS(L(N+1))
C
C -- BEGIN A NEW PASS.
C
 20   CONTINUE
      S = 0
      T = N + 1
      P = L(S)
      Q = L(T)
      IF(Q .EQ. 0) GO TO 100
C
 30   CONTINUE
      IF(KEY(INDX(P)) .LE. KEY(INDX(Q))) THEN
          L(S) = SIGN(P,L(S))
          S = P
          P = L(P)
          IF(P .GT. 0) GO TO 30
          L(S) = Q
          S = T
 55       CONTINUE
             T = Q
             Q = L(Q)
          IF(Q .GT. 0) GO TO 55
      ELSE
          L(S) = SIGN(Q,L(S))
          S = Q
          Q = L(Q)
          IF(Q .GT. 0) GO TO 30
          L(S) = P
          S = T
 75       CONTINUE
             T = P
             P = L(P)
          IF(P .GT. 0) GO TO 75
      ENDIF
C
C -- TEST IF DONE.
C
      P = -P
      Q = -Q
      IF(Q .EQ. 0) THEN
          L(S) = SIGN(P,L(S))
          L(T) = SIGN(0,L(T))
          GO TO 20
      ELSE
          GO TO 30
      ENDIF
C
C -- GET TO HERE WITH LIST SORTED.
C
 100  CONTINUE
C
C -- TRAVERSE THE LINKED LIST AND REARRANGE INDX.
C
      P = L(0)
      K = 1
C
C     -- NORMAL RETURN
 120  IF(K .EQ. N+1) RETURN
C
 130  IF(P .LT. K) THEN
          P = L(P)
          GO TO 130
      ENDIF
C     -- EXCHANGE INDX(K) AND INDX(P).
      I = INDX(K)
      INDX(K) = INDX(P)
      INDX(P) = I
C     -- EXCHANGE L THE SAME WAY.
      I = L(K)
      L(K) = L(P)
      L(P) = I
C     -- CHANGE L(K) AND P   AND INCREMENT K.
      I = L(K)
      L(K) = P
      P = I
      K = K + 1
      GO TO 120
C
      END
