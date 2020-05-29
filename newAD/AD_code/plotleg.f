      PROGRAM PLOTLEG
C
C     Creates a lipha data file for an angular distribution curve.
C     Input a file generated from AD as well as the values for A0, 
C     A2, and A4.
C

      REAL  THET(10), YEXP(10), YERR(10)
      REAL  THETA, A0, A2, A4, P2, P4, INTENSITY
      REAL  DISTRB(10,3)
      CHARACTER  FILENAM*80, FILE*80
      INTEGER  COL, ROW, NUM, DIM, LTYPE, PTYPE, STYPE
      INTEGER  LNUM, LDIM, LLTYPE, LPTYPE, LSTYPE, n, i
      DATA  DIM /3/, LTYPE /0/, PTYPE/11/, STYPE /3/
      DATA  LNUM /90/, LDIM /2/, LLTYPE /1/, LPTYPE /0/, LSTYPE/3/
      
C
      PRINT *, 'Enter the input data filename:'
      READ '(A)', FILE
      write(6,*) 'Enter the output filename:'
      READ '(A)', FILENAM
      write(6,*) 'Enter values for A0, a2, and a4:'
      READ *, A0, A2, A4
      OPEN(2, FILE = FILENAM, FORM = 'FORMATTED', STATUS = 'UNKNOWN')
      OPEN(3, FILE = FILE, STATUS = 'OLD')

      READ (3,*)
      READ (3,*)
      READ (3,*)
      DO 11 ROW = 1,10
         READ (3,*,END=1)  THET(ROW), YEXP(ROW), YERR(ROW)
 11   CONTINUE
 1    CLOSE (3)
      NUM = ROW - 1

      WRITE (2,100) NUM, DIM, LTYPE, PTYPE, STYPE
      DO 21  i = 1,NUM
         WRITE (2,200) THET(i), YEXP(i), YERR(i)
 21   CONTINUE
      
      WRITE (2,100) LNUM, LDIM, LLTYPE, LPTYPE, LSTYPE
      DO 31 I = 1,90
        THETA = I*3.14159/180.
        X = COS(THETA)
        P2 = 0.5*(3*X**2-1)
        P4 = 0.125*(35*X**4-30*X**2+3)
        INTENSITY = A0*(1. + A2*P2 + A4*P4)
        WRITE(2,300) I, INTENSITY
 31   CONTINUE
 100  FORMAT(I3, T4, I3, T8, I3, T12, I3, T16, I3)
 200  FORMAT(F5.1, 2X, F8.3, 2X, F8.3)
 300  FORMAT(1X, I3, 2X, F10.3)
      CLOSE(3)
      CLOSE(2)
      STOP
      END


















