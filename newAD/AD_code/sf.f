	PROGRAM sf
      IMPLICIT INTEGER(A-Z)
	PARAMETER (MAXSIZ=400000)
      CHARACTER ANS*1,LABEL*6,FMT*17,swt*1
	CHARACTER COMMAND*40,clearcmd*40
      INTEGER*4 IDATA(MAXSIZ),DUM(10)
	integer odev,tdev
      COMMON /SPC/ NUMSPC,LABEL(50),NUMCHN(50),IPOINT(50),
     + MRKL(50),MRKR(50),ISPC
      COMMON /ZX/ MAXCHN,NUMRUN,IACC,NTOTAL,IFAST,ISCRN,KZERO
      DATA IDEV /0/, ODEV /0/
	DATA FMT/'(1X,9(I6,'',''),I6)'/,clearcmd/'clear'/
C  
9000  FORMAT(/,13x,' *** RUN ',I5,' ***',/,
     +' ---------------------------------------------',/,
     +' Display        eXit              Change run #',/,
     +' Read           % UNIX comnd      Next run #',/,
     +' Write          spEEd             Manipulate',/,
     +' I/o device     List chan',/,
     +' --------------------------------------------')
9010  FORMAT(/,' Enter run #:')
C
C
      MAXCHN = MAXSIZ
      NUMRUN = 0
	IFAST = 2
	ISCRN = 512
	write (6,*) ' PROGRAM Scope Fit'
	GO TO 40
C
40    CONTINUE
        write (6,9000) NUMRUN
50    inputsize = 1
	CALL GRAB (ANS,INPUTSIZE,INPUTSIZE)
C
      IF ( ANS .EQ. 'R' .or. ans .eq. 'r') THEN
        CALL READD(IDATA,IDEV)
        GO TO 40
C
      ELSE IF ( ANS .EQ. 'W' .or. ans .eq. 'w') THEN
	write(6,*)' TYPE RUN #; FIRST,LAST SPEC #S'
	read (5,*) NUMRUN,IFIRST,ILAST
        CALL WRITED(IDATA,ODEV,IFIRST,ILAST)
        GO TO 40
C
      ELSE IF ( ANS .EQ. 'C' .or. ans .eq. 'c') THEN
        write (6,9010)
        read (5,*) NUMRUN
C
      ELSE IF ( ANS .EQ. 'N' .or. ans .eq. 'n') THEn
        NUMRUN = NUMRUN + 1
C
      ELSE IF ( ANS .EQ. 'I' .or. ans .eq. 'i') THEN
	CALL DEVIO (2,IDEV,ODEV,MDEV,TDEV)
C
	ELSE IF (ANS .EQ. 'D'.or. ans .eq. 'd') THEN
	ISPC = 0
	CALL DISNSM(IDATA)
C
	ELSE IF (ANS .EQ. 'E'.or. ans .eq. 'e') THEN
	CALL SPEED
C
	ELSE IF (ANS .EQ. 'M'.or. ans .eq. 'm') THEN
	CALL MANIP(IDATA)
C
      ELSE IF( ANS .EQ. 'L' .or. ans .eq. 'l') THEN
	CALL LISTSP(IDATA)
C
      ELSE IF( ANS .EQ. 'X' .or. ans .eq. 'x') THEN
	STOP
C
	ELSE IF (ANS .EQ. '%') THEN
	CALL SPNCMD
C

        else if (ans .eq. 'A' .or. ans .eq. 'a') then
           call system(clearcmd)

      ELSE 
        write (6,*) '?'
        GO TO 40
      END IF
	GO TO 40
      END
C
C
C **  Dummy Project
C
	SUBROUTINE PROJ(IDATA,IG1,IG2)
	PARAMETER (MAXSIZ=400000)
      INTEGER*4 IDATA(MAXSIZ)
	character label*6
      COMMON /SPC/ NUMSPC,LABEL(50),NUMCHN(50),IPOINT(50),
     + MRKL(50),MRKR(50),ISPC
	return
	END
C

