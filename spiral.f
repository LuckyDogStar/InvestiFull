      SUBROUTINE DUAL (D,M,N,IER, ORDER, WRITTENA)
      IMPLICIT INTEGER (A-Z)
      DIMENSION D(M,M),A(N,N)
      DIMENSION V(3,N)
      CHARACTER(len=20) :: datafile
      datafile="adjacency.txt" 
C Given a fullerene dual adjacency matrix D, this subroutine
C constructs the corresponding fullerene adjacency matrix A.
C IER = 0 on return if the construction is successful.
      I = 0
      DO 3 L = 1,M
        DO 2 K = 1,L
          IF (D(K,L) .EQ. 0) GO TO 2
          DO 1 J = 1,K
            IF (D(J,K).EQ.0 .OR. D(J,L).EQ.0) GO TO 1
            I = I+1
            IF (I .GT. N) GO TO 1
            V(1,I) = J! Associate the three mutually adjacent
            V(2,I) = K! dual vertices (fullerene faces) J,K,L
            V(3,I) = L! with fullerene vertex I
1         CONTINUE
2       CONTINUE
3     CONTINUE
      IER = I-N
      IF (IER .NE. 0) RETURN! D contains IER > 0 separating triangles
      DO 7 J = 1,N! and is therefore NOT a fullerene dual
        DO 6 I = 1,J
          K = 0
          DO 5 JJ = 1,3
            DO 4 II = 1,3
              IF (V(II,I) .EQ. V(JJ,J)) THEN
                K = K+1
              ENDIF
4           CONTINUE
5         CONTINUE
          IF (K .EQ. 2) THEN
            A(I,J) = 1! Fullerene vertices I and J are adjacent
            A(J,I) = 1! if they have 2 dual vertices in common
          ELSE
            A(I,J) = 0
            A(J,I) = 0
          ENDIF
6       CONTINUE
7     CONTINUE
      IF (ORDER .EQ. 120) THEN
        IF (WRITTENA .EQ. 0) THEN
          OPEN (unit=23,file=datafile)
          DO 18 F=1,N
           DO 17 X=1,N
             WRITE(23, "(1x, I2)", advance="no")(A(F, X))
17         CONTINUE
           WRITE(23,*)
18        CONTINUE
          close (unit=23)
          WRITTENA=1
        ENDIF
      ENDIF
      RETURN
      END


      SUBROUTINE UNWIND (D,M,S,GROUP,NMR,IER, ORDER)
      IMPLICIT INTEGER (A-Z)
      CHARACTER*3 GROUP
      PARAMETER (NMAX = 6000)
      PARAMETER (LMAX = 3*NMAX/2)
      PARAMETER (MMAX = NMAX/2+2)
      DIMENSION D(M,M),S(M),NMR(6)
      DIMENSION P(MMAX),R(MMAX)
      DIMENSION V(3,NMAX),E(2,LMAX)
      DIMENSION VP(NMAX,120),EP(LMAX,120),FP(MMAX,120)
      DIMENSION MV(12),ME(12),MF(12),MS(12)
C  This subroutine unwinds a fullerene dual adjacency matrix D
C into each of its constituent spirals and checks that none has
C  a lexicographically smaller code than the input spiral S. The
C  idealized point group and NMR signature of the fullerene are
C  also calculated if this test is passed, in which case the input
C  spiral is canonical and IER = 0 on return. Otherwise IER = 13.
      IF (M .GT. MMAX) STOP! Increase NMAX
      SO = 0
      DO 10 I1 = 1,M! Begin multiple loop over all
       P(1) = I1! 6*N possible spiral starts
       FLAG1 = 0! with initial faces I1,I2,I3
       IF (S(P(1)) .NE. S(1)) THEN
         IF (S(P(1)) .GT. S(1)) GO TO 10
         FLAG1 = 1
       ENDIF
       DO 9 I2 = 1,M
         IF (D(I1,I2) .EQ. 0) GO TO 9
         P(2) = I2
         FLAG2 = FLAG1
         IF (FLAG2.EQ.0 .AND. S(P(2)).NE.S(2)) THEN
           IF (S(P(2)) .GT. S(2)) GO TO 9
           FLAG2 = 2
         ENDIF
         DO 8 I3 = 1,M
          IF (D(I1,I3).EQ.0 .OR. D(I2,I3).EQ.0) GO TO 8
          IF (SO .EQ. 0) THEN
            SO = 1! Store a face permutation for
            DO 1 K = 1,M! each symmetry operation in FP,
              FP(K,SO) = K! with the identity operation
1           CONTINUE! (here) in column 1
            GO TO 8
          ENDIF
          P(3) = I3
          FLAG3 = FLAG2
          IF (FLAG3.EQ.0 .AND. S(P(3)).NE.S(3)) THEN
            IF (S(P(3)) .GT. S(3)) GO TO 8
            FLAG3 = 3
          ENDIF
          DO 2 J = 1,M
            R(J) = 0
2        CONTINUE
         R(P(1)) = 2
         R(P(2)) = 2
         R(P(3)) = 2
         I = 1
         DO 6 J = 4,M
3          IF (R(P(I)) .EQ. S(P(I))) THEN
             I = I+1
             IF (I .EQ. J-1) GO TO 8
             GO TO 3
           ENDIF
           IF = P(I)! These are the first (IF)
           IL = P(J-1)! and last (IL) open faces
           DO 5 IJ = 1,M! in the preceding spiral
             IF (D(IJ,IF).EQ.0 .OR. D(IJ,IL).EQ.0) GO TO 5
             IF (R(IJ) .GT. 0) GO TO 5
             P(J) = IJ
             IF (FLAG3.EQ.0 .AND. S(P(J)).NE.S(J)) THEN
               IF (S(P(J)) .GT. S(J)) GO TO 8
               FLAG3 = J! This spiral has a smaller
             ENDIF! code than S, but it may not
             DO 4 K = 1,J-1! close properly. Flag it
               IF (D(P(J),P(K)) .EQ. 1) THEN
                 R(P(J)) = R(P(J))+1
                 R(P(K)) = R(P(K))+1
               ENDIF
4            CONTINUE
             GO TO 6
5          CONTINUE
           GO TO 8
6        CONTINUE
         IF (FLAG3 .EQ. 0) THEN
           SO = SO+1! Arrive here once for each
           DO 7 K = 1,M! spiral with the same code as
             FP(K,SO) = P(K)! S, which is once for each
7          CONTINUE! symmetry operation SO
         ELSE
           IER = 13! The flagged spiral has closed,
           RETURN! so call it a day
         ENDIF
8       CONTINUE
9      CONTINUE
10    CONTINUE
      IER = 0! Spiral S is canonical, and
      ORDER = SO! SO is the point group order.
      N = 0! Now calculate GROUP and NMR:
      L = 0
      DO 13 K = 2,M
        DO 12 J = 1,K-1
          IF (D(J,K) .EQ. 0) GO TO 12
            DO 11 I = 1,J-1
              IF (D(I,J).EQ.0 .OR. D(I,K).EQ.0) GO TO 11
              N = N+1
              V(1,N) = I! Associate the three mutually
              V(2,N) = J! adjacent faces I,J,K
              V(3,N) = K! with vertex N
11          CONTINUE
            L = L+1
            E(1,L) = J! And the two mutually adjacent
            E(2,L) = K! faces J,K with edge L
12      CONTINUE
13    CONTINUE
      DO 18 SO = 1,ORDER
        DO 15 J = 1,N
          J1 = FP(V(1,J),SO)
          J2 = FP(V(2,J),SO)
          J3 = FP(V(3,J),SO)
          I1 = MIN(J1,J2,J3)
          I3 = MAX(J1,J2,J3)
          I2 = J1+J2+J3-I1-I3
            DO 14 I = 1,N
              IF (V(1,I).EQ.I1.AND.V(2,I).EQ.I2.AND.V(3,I).EQ.I3) THEN
                VP(J,SO) = I! Store a vertex permutation for
                GO TO 15! each symmetry operation in VP
              ENDIF
14          CONTINUE
15      CONTINUE
        DO 17 J = 1,L
          J1 = FP(E(1,J),SO)
          J2 = FP(E(2,J),SO)
          I1 = MIN(J1,J2)
          I2 = J1+J2-I1
          DO 16 I = 1,L
            IF (E(1,I).EQ.I1 .AND. E(2,I).EQ.I2) THEN
              EP(J,SO) = I! And similarly an edge permutation
              GO TO 17! in EP
            ENDIF
16        CONTINUE
17      CONTINUE
18    CONTINUE
      DO 19 K = 1,12
        MV(K) = 0
        ME(K) = 0
        MF(K) = 0
19    CONTINUE
      DO 21 J = 1,N
        IF (VP(J,1) .EQ. 0) GO TO 21
          VP(J,1) = 0
          K = 1
            DO 20 SO = 2,ORDER
              I = VP(J,SO)
              IF (VP(I,1) .EQ. 0) GO TO 20
                VP(I,1) = 0
                K = K+1
20          CONTINUE
            K = ORDER/K! Count vertex orbits with
            MV(K) = MV(K)+1! site group order K in MV(K)
21    CONTINUE
      DO 22 J = 1,N
        VP(J,1) = J
22    CONTINUE
      DO 24 J = 1,L
        IF (EP(J,1) .EQ. 0) GO TO 24
          EP(J,1) = 0
          K = 1
            DO 23 SO = 2,ORDER
              I = EP(J,SO)
              IF (EP(I,1) .EQ. 0) GO TO 23
                EP(I,1) = 0
                K = K+1
23          CONTINUE
            K = ORDER/K! And edge orbits with
            ME(K) = ME(K)+1! site group order K in ME(K)
24    CONTINUE
      DO 25 J = 1,L
        EP(J,1) = J
25    CONTINUE
      DO 27 J = 1,M
        IF (FP(J,1) .EQ. 0) GO TO 27
          FP(J,1) = 0
          K = 1
          DO 26 SO = 2,ORDER
            I = FP(J,SO)
            IF (FP(I,1) .EQ. 0) GO TO 26
              FP(I,1) = 0
              K = K+1
26        CONTINUE
          K = ORDER/K! And face orbits with
          MF(K) = MF(K)+1! site group order K in MF(K)
27    CONTINUE
      DO 28 J = 1,M
        FP(1,J) = J
28    CONTINUE
      DO 29 K = 1,12! And ALL special point orbits
        MS(K) = MV(K)+ME(K)+MF(K)! with site group order K in MS(K)
29    CONTINUE
      DO 30 J = 1,6
        NMR(J) = 0
30    CONTINUE
      J = 0
      DO 31 K = 6,1,-1! Use the vertex orbit counts
        IF (MV(K) .EQ. 0) GO TO 31! to calculate the NMR pattern
          J = J+1
          NMR(J) = MV(K)
          J = J+1
          NMR(J) = ORDER/K
31    CONTINUE
      GROUP = '???'! And, finally, the full
      IF (ORDER .EQ. 1) THEN! special point orbit counts
        GROUP = ' C1'! (in conjunction with the
      ELSE IF (ORDER .EQ. 2) THEN! point group order) to assign
        IF (MS(2) .EQ. 0) THEN! the point group
          GROUP = ' Ci'
        ELSE IF (MS(2) .EQ. 2) THEN
          GROUP = ' C2'
        ELSE IF (MS(2) .GT. 2) THEN
          GROUP = ' Cs'
        ENDIF
      ELSE IF (ORDER .EQ. 3) THEN
        GROUP = ' C3'
      ELSE IF (ORDER .EQ. 4) THEN
        IF (MS(4) .EQ. 0) THEN
          IF (MS(2) .EQ. 1) THEN
            GROUP = ' S4'
          ELSE IF (MS(2) .EQ. 3) THEN
            GROUP = ' D2'
          ELSE IF (MS(2) .GT. 3) THEN
            GROUP = 'C2h'
          ENDIF
        ELSE IF (MS(4) .EQ. 2) THEN
          GROUP = 'C2v'
        ENDIF
      ELSE IF (ORDER .EQ. 6) THEN
        IF (MS(6) .EQ. 0) THEN
          IF (MS(2) .EQ. 0) THEN
            GROUP = ' S6'
          ELSE IF (MS(2) .EQ. 2) THEN
            GROUP = ' D3'
          ELSE IF (MS(2) .GT. 2) THEN
            GROUP = 'C3h'
          ENDIF
        ELSE IF (MS(6) .EQ. 2) THEN
          GROUP = 'C3v'
        ENDIF
      ELSE IF (ORDER .EQ. 8) THEN
        IF (MS(4) .EQ. 1) THEN
          GROUP = 'D2d'
        ELSE IF (MS(4) .EQ. 3) THEN
          GROUP = 'D2h'
        ENDIF
      ELSE IF (ORDER .EQ. 10) THEN
        GROUP = ' D5'
      ELSE IF (ORDER .EQ. 12) THEN
        IF (MS(6) .EQ. 0) THEN
          GROUP = ' T'
        ELSE IF (MS(6) .EQ. 1) THEN
          IF (MS(4) .EQ. 0) THEN
            IF (MS(2) .EQ. 2) THEN
              GROUP = ' D6'
            ELSE IF (MS(2) .GT. 2) THEN
              GROUP = 'D3d'
            ENDIF
          ELSE IF (MS(4) .EQ. 2) THEN
            GROUP = 'D3h'
          ENDIF
        ENDIF
      ELSE IF (ORDER .EQ. 20) THEN
        IF (MS(4) .EQ. 0) THEN
          GROUP = 'D5d'
        ELSE IF (MS(4) .EQ. 2) THEN
          GROUP = 'D5h'
        ENDIF
      ELSE IF (ORDER .EQ. 24) THEN
        IF (MS(12) .EQ. 0) THEN
          IF (MS(6) .EQ. 0) THEN
            GROUP = ' Th'
          ELSE IF (MS(6) .EQ. 2) THEN
            GROUP = ' Td'
          ENDIF
        ELSE IF (MS(12) .EQ. 1) THEN
          IF (MS(4) .EQ. 0) THEN
            GROUP = 'D6d'
          ELSE IF (MS(4) .EQ. 2) THEN
            GROUP = 'D6h'
          ENDIF
        ENDIF
      ELSE IF (ORDER .EQ. 60) THEN
        GROUP = ' I'
      ELSE IF (ORDER .EQ. 120) THEN
        GROUP = ' Ih'
      ENDIF
      RETURN
      END



      SUBROUTINE WINDUP (S,M,D,IPR,IER, ORDER, WRITTEND)
      IMPLICIT INTEGER (A-Z)
      PARAMETER (NMAX = 6000)
      PARAMETER (MMAX = NMAX/2+2)
      DIMENSION S(M),D(M,M)
      DIMENSION R(MMAX),C(MMAX,6)
      CHARACTER(len=20) :: dfilename
      dfilename="dual.txt" 
C This subroutine attempts to wind up an input spiral S into
C a fullerene dual (face) adjacency matrix D. It returns with
C IER = P if the spiral shorts or is discovered to be open-ended
C after P pentagons have been added. Otherwise IER = 0 on return.
      IF (M .GT. MMAX) STOP! Increase NMAX
      J = 1
      C(1,1) = 2
      C(2,1) = 1

      R(1) = 2  
      R(2) = 2

      E = 1

      P = 6-S(1) + 6-S(2)
      DO 5 K = 3,M-1
        P = P + 6-S(K)
        R(K) = 1
        I = K-1
1       IF (IPR.EQ.1 .AND. S(I).EQ.5 .AND. S(K).EQ.5) GO TO 10
          IF (R(K) .GE. S(K)) GO TO 10
            C(I,R(I)) = K! Connect face K to the last open face I
            C(K,R(K)) = I! in the preceding spiral
            R(I) = R(I)+1
            R(K) = R(K)+1
            IF (R(I) .GT. S(I)) THEN 
              L = I-1! If this closes face I update I and go
              DO 2 I = L,J+1,-1! back to connect face K to the new I
                IF (R(I) .LE. S(I)) GO TO 1
2             CONTINUE
              GO TO 10
            ENDIF
3           IF (IPR.EQ.1 .AND. S(J).EQ.5 .AND. S(K).EQ.5) GO TO 10
            IF (R(K) .GE. S(K)) GO TO 10
            C(J,R(J)) = K! Connect face K to the first open face J
            C(K,R(K)) = J! in the preceding spiral
            R(J) = R(J)+1
            R(K) = R(K)+1
            IF (R(J) .GT. S(J)) THEN
              L = J+1! If this closes face J update J and go
              DO 4 J = L,I-1,+1! back to connect face K to the new J
                IF (R(J) .LE. S(J)) GO TO 3
4             CONTINUE
              GO TO 10
            ENDIF
 
            H = K-P
            E = E+R(K)-1! Use Euler’s theorem to streamline the
            V = 3+2*P+3*H-E! search. F is a lower bound on the # of
            F = (V+1)/2+1! additional faces required for closure
            IF (F .GT. M-K) GO TO 10
5     CONTINUE

      P = 12
      R(M) = 1
      DO 6 K = J,M-1
        IF (R(K) .LT. S(K)) GO TO 10
        IF (R(K) .GT. S(K)) GO TO 6
        IF (R(M) .GT. S(M)) GO TO 10
        IF (IPR.EQ.1 .AND. S(K).EQ.5 .AND. S(M).EQ.5) GO TO 10
        C(K,R(K)) = M! Connect face M to all remaining
        C(M,R(M)) = K! open faces (including face M-1)
        R(K) = R(K)+1
        R(M) = R(M)+1
6     CONTINUE

      IF (R(M) .LE. S(M)) GO TO 10
      P = 0! Successful spiral
      DO 9 J = 1,M! Form dual adjacency matrix in D
        DO 7 I = 1,M
          D(I,J) = 0
7       CONTINUE
        DO 8 K = 1,S(J)
          I = C(J,K)
          D(I,J) = 1
8       CONTINUE
9     CONTINUE
10    IER = P
      IF (ORDER  .EQ. 120) THEN
       IF (WRITTEND .EQ. 0) THEN
         OPEN (unit=20,file=dfilename)
         DO 18 G=1,M
           DO 17 J=1,M
             WRITE(20, "(1x, I2)", advance="no")(D(G, J))
17         CONTINUE
           WRITE(20,*)
18       CONTINUE
         close (unit=20)
         WRITTEND = 1
       ENDIF
      ENDIF
      RETURN
      END



      PROGRAM SPIRAL
      IMPLICIT INTEGER (A-Z)
      CHARACTER*3 GROUP
      PARAMETER (NMAX = 6000)
      PARAMETER (MMAX = NMAX/2+2)
      DIMENSION D(MMAX*MMAX),S(MMAX), A(NMAX, NMAX)
      DIMENSION NMR(6)
      INTEGER  WRITTEND
      INTEGER WRITTENA
C This program catalogues fullerenes with a given number of
C vertices using the spiral algorithm and a uniqueness test
C based on equivalent spirals. The required input is N and IPR,
C where N is the nuclearity of the fullerene and IPR = 0 for
C general and 1 for isolated-pentagon isomers. The resulting
C output is a catalogue of the isomers found containing their
C idealized point groups, canonical spirals, and NMR patterns.
      READ (5,*) N,IPR
      WRITTEND = 0
      WRITTENA = 0
      IF (N .GT. NMAX) STOP! Increase NMAX
      IF (2*(N/2) .NE. N) STOP! N must be even
      IF (IPR.NE.0 .AND. IPR.NE.1) STOP! IPR must be 0 or 1
      IF (IPR .EQ. 0) THEN
        IF (N .LT. 100) WRITE (6,601) N
601     FORMAT(1X,'GENERAL FULLERENE ISOMERS OF C',I2,':'/1X,77('-'))
        IF (N .GE. 100) WRITE (6,602) N
602     FORMAT(1X,'GENERAL FULLERENE ISOMERS OF C',I3,':'/1X,77('-'))
      ELSE
        IF (N .LT. 100) WRITE (6,603) N
603     FORMAT(1X,'ISOLATED-PENTAGON ISOMERS OF C',I2,':'/1X,77('-'))
        IF (N .GE. 100) WRITE (6,604) N
604     FORMAT(1X,'ISOLATED-PENTAGON ISOMERS OF C',I3,':'/1X,77('-'))
      ENDIF
      L = 0
      M = N/2+2
      JPR = IPR+1
      DO 1 J1 = 1,M-11*JPR! Open loop over spiral
      DO 2 J2 = J1+JPR,M-10*JPR! combinations
      DO 3 J3 = J2+JPR,M-9*JPR
      DO 4 J4 = J3+JPR,M-8*JPR
      DO 5 J5 = J4+JPR,M-7*JPR
      DO 6 J6 = J5+JPR,M-6*JPR
      DO 7 J7 = J6+JPR,M-5*JPR
      DO 8 J8 = J7+JPR,M-4*JPR
      DO 9 J9 = J8+JPR,M-3*JPR
      DO 10 J10 = J9+JPR,M-2*JPR
      DO 11 J11 = J10+JPR,M-JPR
      DO 12 J12 = J11+JPR,M
      DO 14 J = 1,M! Form spiral code in S
        S(J) = 6
14    CONTINUE
      S(J1) = 5
      S(J2) = 5
      S(J3) = 5
      S(J4) = 5
      S(J5) = 5
      S(J6) = 5
      S(J7) = 5
      S(J8) = 5
      S(J9) = 5
      S(J10) = 5
      S(J11) = 5
      S(J12) = 5
      CALL WINDUP (S,M,D,IPR,IER, ORDER, WRITTEND)! Wind up spiral into dual
      IF (IER .EQ. 12) GO TO 12! and check for closure
      IF (IER .EQ. 11) GO TO 11
      IF (IER .EQ. 10) GO TO 10
      IF (IER .EQ. 9) GO TO 9
      IF (IER .EQ. 8) GO TO 8
      IF (IER .EQ. 7) GO TO 7
      IF (IER .EQ. 6) GO TO 6
      IF (IER .EQ. 5) GO TO 5
      IF (IER .EQ. 4) GO TO 4
      IF (IER .EQ. 3) GO TO 3
      IF (IER .EQ. 2) GO TO 2
      IF (IER .EQ. 1) GO TO 1
      CALL UNWIND (D,M,S,GROUP,NMR,IER, ORDER)! Unwind dual into spirals
      CALL DUAL (D,M,N,IER, ORDER, WRITTENA)
      IF (IER .EQ. 13) GO TO 13! and check for uniqueness
        K = 0
        L = L+1! Spiral S is canonical
        DO 15 J = 1,6
          IF (NMR(J) .EQ. 0) GO TO 16
          K = J
15      CONTINUE
16      WRITE (6,605) L,GROUP,
     + J1,J2,J3,J4,J5,J6,J7,J8,J9,J10,J11,J12,(NMR(J),J=1,K)
605     FORMAT(1X,I8,2X,A3,1X,12I3,2X,3(I3,' x',I3,:,','))
13    CONTINUE
12    CONTINUE! Close loop over spiral
11    CONTINUE! combinations
10    CONTINUE
9     CONTINUE
8     CONTINUE
7     CONTINUE
6     CONTINUE
5     CONTINUE
4     CONTINUE
3     CONTINUE
2     CONTINUE
1     CONTINUE
      WRITE (6,606)
606   FORMAT(1X,77('-'))
607   FORMAT(  I2, x) 
      STOP
      END 
