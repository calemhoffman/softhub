	PROGRAM singlsort
c     Adapted to PC November 11, 2000   (SLT)
c     Debugged to read 'run*.evt' files from disk.
c     Haven't checked old read from tapes.  Also removed VMS tape read.
      IMPLICIT INTEGER(A-Z)
	PARAMETER (MAXSIZ=200000)
      CHARACTER ANS*1,LABEL*6,FMT*17,swt*1
	CHARACTER COMMAND*40,clearcmd*40
      INTEGER*4 IDATA(MAXSIZ),DUM(10)
	integer odev,tdev
      COMMON /SPC/ NUMSPC,LABEL(50),NUMCHN(50),IPOINT(50),
     + MRKL(50),MRKR(50),ISPC
      COMMON /ZX/ MAXCHN,NUMRUN,IACC,NTOTAL,IFAST,ISCRN,KZERO,MSIZE
      DATA IDEV /0/, ODEV /0/,MDEV/1/,TDEV/0/
	DATA FMT/'(1X,9(I6,'',''),I6)'/
        data clearcmd/'clear'/
C  
9000  FORMAT(/,6x,'SINGLES SORT    *** RUN ',I5,' ***',/,
     +' ---------------------------------------------',/,
     +' Display        eXit              Change run #',/,
     +' Read           % UNIX comnd      Next run #',/,
     +' Write          spEEd             Manipulate',/,
     +' I/o device     List chan         sOrt',/,
     +' Setup          Zero              cleAr',/,
     +' --------------------------------------------')
9010  FORMAT(/,' Enter run #:')
C
C
      MAXCHN = MAXSIZ
      NUMRUN = 0
	IFAST = 2
	ISCRN = 512
	write (6,*) ' PROGRAM Singles SORT'
	GO TO 40
1	write (6,*) 'Zeroing spectra'
	DO 22 I = 1,MAXSIZ
22	IDATA(I)=0
C
40    CONTINUE
        write (6,9000) NUMRUN
50    inputsize = 1
	CALL GRAB (ANS,INPUTSIZE,INPUTSIZE)
C
      IF ( ANS .EQ. 'O' .or. ans .eq. 'o') THEN
	CALL SORT (IDATA,MDEV,swt)
	GO TO 40
C
      ELSE IF ( ANS .EQ. 'R' .or. ans .eq. 'r') THEN
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
      ELSE IF ( ANS .EQ. 'S' .or. ans .eq. 's') THEN
        CALL SETUP(swt)
C
      ELSE IF ( ANS .EQ. 'Z' .or. ans .eq. 'z') THEN
	GO TO 1
C
      ELSE IF ( ANS .EQ. 'I' .or. ans .eq. 'i') THEN
	CALL DEVIO (4,IDEV,ODEV,MDEV,TDEV)
C
	ELSE IF (ANS .EQ. 'P'.or. ans .eq. 'p') THEN
	  IG1 = -1
	  CALL PROJ(IDATA,IG1,IG2)
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
           call system (clearcmd)
      ELSE 
        write (6,*) '?'
        GO TO 40
      END IF
	GO TO 40
      END
C
C **  Setup ADC's
C
      SUBROUTINE SETUP(swt)
	CHARACTER FILNAME*24,LABEL*6,SWT*1
        LOGICAL DSKFLG
	COMMON /SPC/ NUMSPC,LABEL(50),NUMCHN(50),IPOINT(50),
     + MRKL(50),MRKR(50),ISPC
	COMMON /GATES/ ITG(4),CAL(2,4),ISIZE,IADC,it(6),is(6),DSKFLG
      COMMON /ZX/ MAXCHN,NUMRUN,IACC,NTOTAL,IFAST,ISCRN,KZERO,MSIZE
C
1	WRITE (6,*) ' Type number of ADCs '
	READ (5,*) IADC
	NUMSPC = IADC
C  SET UP  GAMMA SPECTRA TO SORT INTO
	IPOINT(1)=1
        ISIZE = 4096
	DO 8 I = 1, IADC
	WRITE (6,*) ' Type number of channels for spectrum ',I,
     +  '  (CR for last #)'
	READ (5,109) ISIZ
 109    format (i10)
        IF (ISIZ .GT. 0) ISIZE = ISIZ
	NUMCHN(I)=ISIZE
	LABEL(I)='GAMMA '
	IPOINT(I+1)=NUMCHN(I)+IPOINT(I)
8	CONTINUE
c        WRITE (6,*) 'Is the data on disk?  [T,F] '
c        READ (5,110) DSKFLG
c 110    FORMAT (L1)
	DSKFLG = .true.
        MSIZE = 1024
	RETURN
      END
C
C **  SORTS MULTIPARAMETER DATA INTO SINGLES
C
	SUBROUTINE SORT(IDATA,MDEV,swt)
	PARAMETER (MAXSIZ=200000)
	INTEGER*2 IBUF(2048),buffer(2044)
      INTEGER*4 IDATA(MAXSIZ)
      CHARACTER FILE*40,FILE1*40,RUN*5,LABEL*6,SWT*1
	COMMON /GATES/ ITG(4),CAL(2,4),ISIZE,IADC,it(6),is(6),DSKFLG
      COMMON /SPC/ NUMSPC,LABEL(50),NUMCHN(50),IPOINT(50),
     + MRKL(50),MRKR(50),ISPC
      COMMON /ZX/ MAXCHN,NUMRUN,IACC,NTOTAL,IFAST,ISCRN,KZERO,MSIZE
	INTEGER*2 IC(50),IAM(5),IAP(5),head
	REAL *4 EGM(5),EGP(5),FRAC(5)
	LOGICAL DSKFLG,contflg
	IF (IADC .NE. 0) GO TO 1
	write (6,*) ' MUST SETUP GATES FIRST! !'
	RETURN
1	IF (.NOT. DSKFLG) THEN
           WRITE (6,*) 'Type number of file marks to skip:'
           READ (5,*) kskip
        END IF
        write (6,*) ' ENTER NUMBER OF RECORDS TO SORT'
	IRMIN = 0
	WRITE (6,*) '[0 for ALL, -1 to abort]'
	read (5,*) IRMAX
	IF (IRMAX .LT. 0) return
        CONTFLG = .FALSE.
	kfirst = 1
	klast = 1
        IF (DSKFLG) THEN
 2	      write (6,*) 'Enter the first and last run numbers'
	      read (5,*) kfirst,klast
	      if(kfirst.gt.klast.or.kfirst.lt.0.or.klast.lt.0) goto 2
	ELSE
           write (6,*) 'Type 0 or 1 for tape drive'
           read (5,*) ktape
           if (ktape .eq. 0) file = "/dev/nrmt0l"
           if (ktape .eq. 1) file = "/dev/nrmt1l"
           call opentape(idev,file,kskip)
	   if (idev .lt. 0) go to 990
        ENDIF
	DO 400 krun = kfirst, klast
c	IREC=1
c	IF (DSKFLG) THEN
	   head = 0
	   k = 0
	   irec = 1
	   do 10 i = 1, 50
	      ic(i) = 0
 10	      continue
	   write (run,902) krun
 902	   format (I5)
	   FILE(1:3) = 'run'
	   FILE(4:8) = run
	   FILE(9:12) = '.evt'
	   FILE(13:40) = '                           '
	   call squeze(file,file1)
	   write (6,*) file1
	   OPEN(3,FILE=file1,FORM='UNFORMATTED',STATUS='OLD',ERR=990)
	   write (6,*) 'SORTING FILE',krun
	   IREC=1
 20	   do i=1,2044
	      buffer(i)=0
	   end do
	   iev = 1
      	   READ(3,end=800,err=980) buffer
 26	   head = buffer(iev)
	   iev = iev + 1
	   if(iev .gt. 2044) goto 60
	   IF(HEAD .LT. 0) THEN
	      NUMADC = IAND(15,HEAD)
	      do i = 1,iadc
		 ic(i) = 0
	      end do
	      IF(NUMADC .GE. 1 .AND. NUMADC .LE. iadc) THEN
		 K = (IAND(496,HEAD))/16
		 ic(k) = buffer(iev)
		 IEV = IEV + 1
		 if(iev .gt. 2044) goto 60
		 IF(NUMADC .GE. 2 .AND. NUMADC .LE. iadc) THEN
		    K = (IAND(15872,HEAD))/512
		    IC(K) = BUFFER(IEV)
		    IEV = IEV + 1
		    if(iev .gt. 2044) goto 60
		 endif
		 IF(NUMADC .GE. 3 .AND. NUMADC .LE. iadc) THEN
		    DO 30 I = 3, NUMADC
		       K = BUFFER(IEV)
		       IF(K .LE. 0) GOTO 26
		       IEV = IEV + 1
		       if(iev .gt. 2044) goto 60
		       IC(K) = BUFFER(IEV)
		       IF(IC(K) .LE. 0) GOTO 26
		       IEV = IEV + 1
		       if(iev .gt. 2044) goto 60
 30		    CONTINUE
		 ELSE IF(NUMADC .LE. 0 .OR. NUMADC .GE. (iadc+1)) THEN
		    GOTO 995
		 endif
	      ENDIF
	   ELSE IF(HEAD .GE. 0) THEN
	      iev = iev + 1
	      if(iev .gt. 2044) goto 60
	      GOTO 26
	   ENDIF
	   head = 0
	   if (iev .gt. 2044) goto 60

c	ELSE
c           icount = MSIZE
c           call readtape(idev,ibuf,icount)
c           if (icount .eq. MSIZE) go to 24
c           go to 800
c 24	   kount = icount/2
c	write (6,*) 'SORTING RUN',kRUN
c22	IEV=0
c        IF (CONTFLG) GO TO 42
c35	IEV=IEV+1
c	IF (IEV .GT. kount)  GO TO 60
c40	IHEAD = IBUF(IEV)
c	IF (IHEAD.GE.0) GO TO 35
c	JEVTYP = IAND(IHEAD,255)
c	IF (JEVTYP .EQ. 1) THEN
c          NNUM = 1
c 42       DO 45 K=NNUM,IADC
c	  IEV=IEV+1
c	  IF (IEV .GT. kount) THEN
c              CONTFLG = .TRUE.
c              NNUM = K
c              go to 60
c          end if
c          if (ibuf(iev) .lt. 0) go to 40
c	  IC(K) = IBUF(IEV)
c45	  CONTINUE
c	ELSE
c	  DO 41 K = 1, IADC
c41	  IC(K) = 0
c	  IF (IEV .GT. (kount-2)) GO TO 60
c	  J2 = JEVTYP/16
c	  J1 = IAND(JEVTYP,15)
c	  IEV = IEV + 1
c	  IC(J1) = IBUF(IEV)
c	  IEV = IEV + 1
c	  IC(J2) = IBUF(IEV)
c	END IF
c	ENDIF
C
C  SORTS "SINGLES" INTO SPECTRA 
C
	DO 51 J = 1, IADC
	IF (IC(J) .LE. 0 .OR. IC(J) .GT. NUMCHN(J)) GO TO 51
	ICH=IC(J)+IPOINT(J)
	IDATA(ICH)=IDATA(ICH)+1
51	CONTINUE
	GO TO 26	
c
60	IF (MOD(IREC,1000) .EQ. 0) WRITE (6,*) IREC, ' records sorted.'
        IREC=IREC+1
	if (irec .gt. irmax .and. irmax .ne. 0) go to 800
	GO TO 20
800	IREC = IREC - 1
	write (6,*) IREC,' RECORDS SORTED'
        IF (DSKFLG) THEN
	   CLOSE(3)
	ELSE
           call closetape(idev)
        END IF
 400	continue
	RETURN
 980	write(6,*) 'Error in reading file: ',file1
	CLOSE(3)
	RETURN
990	write (6,*) ' OPEN ERROR ON',FILE,file1
        IF (DSKFLG) THEN
	   CLOSE(3)
	ELSE
          call closetape(idev)
        END IF
	RETURN
 995	write (6,*)'Error in the # of ADCs in an event!'
	close(3)
	return
	END

C
C **  DUMMY PROJECT ROUTINE
C
	SUBROUTINE PROJ(IDATA,IG1,IG2)
	PARAMETER (MAXSIZ=200000)
	INTEGER*4 IDATA(MAXSIZ)
	RETURN
	END
C


