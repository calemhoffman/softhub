	PROGRAM trisort
c   modified 7-23-93 to read PC tapes.  Requires VMSFLG in .gte
c   modified 3-5-99 for 20 8192 channel gamma detectors.
c    Number of detector pairs increased to 110.  Must have 110 numbers!
      IMPLICIT INTEGER(A-Z)
	PARAMETER (MAXSIZ=200000)
	parameter (siz=3000) 
	parameter (dsiz=(siz*siz+siz)/2)
	real tri,sqr
	COMMON /BIG/tri(dsiz)
      CHARACTER ANS*1,LABEL*6,FMT*17,swt*1
	CHARACTER COMMAND*40
      INTEGER*4 IDATA(MAXSIZ),DUM(10),trutim,truet(50)
	integer*2 spc2(8192)
	integer odev,tdev
      COMMON /SPC/ NUMSPC,LABEL(50),NUMCHN(50),IPOINT(50),
     + MRKL(50),MRKR(50),ISPC,SPC2,TRUTIM,TRUET
      COMMON /ZX/ MAXCHN,NUMRUN,IACC,NTOTAL,IFAST,ISCRN,KZERO
      DATA IDEV /0/, ODEV /0/,MDEV/1/,TDEV/0/
	DATA FMT/'(1X,9(I6,'',''),I6)'/
C  
9000  FORMAT(/,13x,' *** RUN ',I5,' ***',/,
     +' ---------------------------------------------',/,
     +' Display        eXit              Change run #',/,
     +' Read           % UNIX comnd      Next run #',/,
     +' Write          spEEd             Manipulate',/,
     +' I/o device     List chan         sOrt',/,
     +' Setup          Zero              Tape twod',/,
     +'                Project',/,
     +' --------------------------------------------')
9010  FORMAT(/,' Enter run #:')
C
C
      MAXCHN = MAXSIZ
      NUMRUN = 0
	IFAST = 2
	ISCRN = 512
	write (6,*) ' PROGRAM TRISORT'
	GO TO 40
1	DO 21 I=1,dsiz
21	tri (I)=0.
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
	ELSE IF (ANS .EQ. 'T'.or. ans .eq. 't') THEN
	  CALL TAPEIO(TDEV)
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
	ELSE IF (ANS .EQ. 'Q') THEN
	write (6,*) ' I1, I2 ?'
	read (5,*) I1,I2
	write(6,*) (tri(K),K=I1,I2)
c
C
	ELSE IF (ANS .EQ. '%') THEN
	CALL SPNCMD
C
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
	INTEGER*2 kclvr(4,4)
      INTEGER*4 trutim,truet(50)
	integer*2 spc2(8192)
	COMMON /SPC/ NUMSPC,LABEL(50),NUMCHN(50),IPOINT(50),
     + MRKL(50),MRKR(50),ISPC,SPC2,TRUTIM,TRUET
	COMMON /GATES/ ITG(4),CAL(3,50),ISIZE,NUMGAM,IADC,it(110),
     + nclvr,kclvr,calnew
	COMMON /CALIBRAT/ ENC(8192,50)
      COMMON /ZX/ MAXCHN,NUMRUN,IACC,NTOTAL,IFAST,ISCRN,KZERO
C
1	write (6,*) ' Type S-ingles or T-wod sort'
	read (5,101) SWT
101	FORMAT(A1)
	IF (SWT .NE. 'S' .AND. SWT .NE. 'T' .and. swt .ne. 's'
     .  .and. swt .ne. 't') GO TO 1
	if (swt .eq. 't') swt = 'T'
	write (6,*) 'Type gate file name'
	read (5,109) FILNAME
109	FORMAT(A24)
100	OPEN (2,FILE=FILNAME,STATUS='OLD',err=990)
	READ (2,*,err=990) ITG
        write (6,*) 'MUL gates  =',itg
	READ (2,*,err=990) ISIZE,NUMGAM,IADC
	READ (2,*,err=990) ((CAL(i,k),i=1,3),k=1,iadc)
	READ (2,*,err=990) CALNEW
        write (6,*) 'ISIZE, NUMGAM, IADC = ',isize, numgam, iadc
	IPOINT(1)=1
	NUMSPC = IADC
	DO 20 I = 1, NUMSPC
	READ (2,21,err=990) NUMCHN(I),LABEL(I)
20	IPOINT(I+1) = IPOINT(I) + NUMCHN(I)
21	FORMAT (I4,1X,A6)
	read (2,*,err=990) nclvr
	write (6,*) 'NCLOVER =',nclvr
	if (nclvr .le. 0) go to 23
	do 22 n = 1, nclvr
 	   read (2,*,err=990) (kclvr(j,n), j=1,4)
 22	   write (6,*) 'Clover ADCs: ', (kclvr(j,n), j=1,4)
 23	read (2,*,err=990) it
	CLOSE (2,err=990)
	write (6,102) ITG
102	FORMAT(' MUL GATES:',4I6)
        write (6,*) " Calibration coefficients:"
	write (6,103) ((CAL(i,k),i=1,3),k=1,iadc)
103	FORMAT(3F11.7,10X,3F11.7)
	write (6,104) CALNEW
104	FORMAT (' NEW CALIBRATION:',F8.3,' keV/chan;    Sort into',
     +  ' triangle array ADCs # :')
	DO 13 j=1,iadc
	DO 13 i=1,8192
	  X = FLOAT (I) + 0.5
13	  ENC(I,J) = (CAL(1,J) + (CAL(2,J) + CAL(3,J) * X) * X) / CALNEW
	IF (SWT .EQ. 'S' .or. swt .eq. 's') RETURN
	write (6,105) it
105	format (10(2I2,3X))

C  SET UP a GAMMA SPECTRum TO SORT INTO
17	NUMSPC=1
	IPOINT(1)=1
	NUMCHN(1)=ISIZE
	LABEL(1)='GAMMA '
	RETURN
990	write (6,*) 'Open, Read or Close error'
	return
      END
C
C **  SORTS MULTIPARAMETER DATA INTO SINGLES OR A FOLDED 2-D ARRAY
C
	SUBROUTINE SORT(IDATA,MDEV,swt)
	integer siz,dsiz,siz1,free_run,no_headers
	parameter (siz=3000)
	parameter (dsiz=(siz*siz+siz)/2)
	PARAMETER (MAXSIZ=200000)
	INTEGER*2 IBUF(2048),buffer(2044),head
      INTEGER*4 IDATA(MAXSIZ),trutim,truet(50)
	integer*2 spc2(8192)
	real*4 ec(50), rann
	INTEGER*2 kclvr(4,4)
	COMMON /BIG/tri(dsiz)
      CHARACTER FILE*40,FILE1*40,RUN*5,LABEL*6,SWT*1
c        LOGICAL contflg
	COMMON /GATES/ ITG(4),CAL(3,50),ISIZE,NUMGAM,IADC,it(110),
     + nclvr,kclvr,calnew
	COMMON /CALIBRAT/ ENC(8192,50)
      COMMON /SPC/ NUMSPC,LABEL(50),NUMCHN(50),IPOINT(50),
     + MRKL(50),MRKR(50),ISPC,SPC2,TRUTIM,TRUET
      COMMON /ZX/ MAXCHN,NUMRUN,IACC,NTOTAL,IFAST,ISCRN,KZERO
	INTEGER*2 IC(50)
	rann = 0.13
	kruns=1
	siz1 =2*siz+1
	IF (IADC .NE. 0) GO TO 1
	write (6,*) ' MUST SETUP GATES FIRST! !'
	RETURN
c1	IF (.NOT. VMSFLG) THEN
c           WRITE (6,*) 'Type number of file marks to skip:'
c           READ (5,*) kskip
c        END IF
 1	write (6,*) ' ENTER NUMBER OF RECORDS TO SORT, 0 FOR ALL, -1 to
     +  ABORT'
	read (5,*) IRMAX
	IF (IRMAX .LT. 0) RETURN
2	write (6,*) 'Enter the first and last run numbers'
	read (5,*) kfirst,klast
	if(kfirst.gt.klast.or.kfirst.lt.0.or.klast.lt.0) goto 2
	DO 400 krun = kfirst, klast
	   head = 0
	   k = 0
	   irec = 1
	   do 10 i = 1, 50
	      ic(50) = 0
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
	      IF(NUMADC .GE. 1 .AND. NUMADC .LE. 20) THEN
		 K = (IAND(496,HEAD))/16
		 ic(k) = buffer(iev)
		 IEV = IEV + 1
		 if(iev .gt. 2044) goto 60
		 IF(NUMADC .GE. 2 .AND. NUMADC .LE. 20) THEN
		    K = (IAND(15872,HEAD))/512
		    IC(K) = BUFFER(IEV)
		    IEV = IEV + 1
		    if(iev .gt. 2044) goto 60
		 endif
		 IF(NUMADC .GE. 3 .AND. NUMADC .LE. 20) THEN
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
		 ELSE IF(NUMADC .LE. 0 .OR. NUMADC .GE. 21) THEN
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
	   do 46, k=1,iadc
	      ec(k) = 0.
	      kk = ic(k)
	      if(kk .le. 0 .or. kk .gt. 8060) goto 46
	      ec(k) = enc(kk, k)
 46	      continue
	   if (nclvr .le. 0) go to 49
	   do 48 n = 1, nclvr
	      im = kclvr(1,n)
	      do 47 j = 2, 4
		 in = kclvr(j,n)
		 ec(im) = ec(im) + ec(in)
 47		 continue
 48	      continue
 49	   continue

c	   write (1,*) numadc,(ic(k),k=1,iadc),(ec(k),k=1,iadc)

c	knterr = 0
c        CONTFLG = .FALSE.
c           write (6,*) 'Type 0 or 1 for tape drive'
c           read (5,*) ktape
c           if (ktape .eq. 0) file = "/dev/nrmt0l"
c           if (ktape .eq. 1) file = "/dev/nrmt1l"
c           call opentape(idev,file,kskip)
c	if (idev .lt. 0) go to 990
c	do 400 krun = 1, kruns
c	write (6,*) 'SORTING RUN, FILE',NUMRUN,krun
c	IREC=1
c           icount = 1024
c           call readtape(idev,ibuf,icount)
c           if (icount .eq. 1024) go to 22
c           go to 800
c22	kount = icount/2
c	write (4,*) (ibuf(isam),isam=1, kount)
c        IEV=0
c        IF (CONTFLG) GO TO 41
c35	IEV=IEV+1
c	IF (IEV .GT. kount)  GO TO 60
c40	IHEAD = IBUF(IEV)
c	IF (IHEAD.GE.0) GO TO 35
c	JEVTYP = IAND(IHEAD,255)
c          MULT = 0
c          NNUM = 1
c 41       DO 45 K=NNUM,IADC
c	  IEV=IEV+1
c	  IF (IEV .GT. kount) THEN
c              CONTFLG = .TRUE.
c              NNUM = K
c              GO TO 60
c          END IF
c	  IC(K) = IBUF(IEV)
c          IF (IC(K) .LT. 0) GO TO 40
c          IF (IC(K) .GT. 0) MULT = MULT + 1
c45	  CONTINUE
c	write (4,*) 'mult=',mult
c	write (4,*) (ic(isam),isam=1,20)
c        IF (MULT .GT. 4) GO TO 35


	IF (SWT .EQ. 'T') GO TO 100
C
C  SORTS "SINGLES" INTO SPECTRA IF SWT = 'S'
C
	DO 51 J = 1, IADC
	   ich = ec(j)
	   if ((ec(j) - ich) .gt. rann) ich = ich + 1
	   rann = rann + 0.73
	   if (rann .gt. 1.) rann = rann - 1.
	   if (ich .le. 0 .or. ich .gt. numchn(j)) go to 51
	   ich = ich + ipoint(j)
	   IDATA(ICH)=IDATA(ICH)+1
 51	CONTINUE
	GO TO 26
C
C  START SORTING INTO 2-D arrays tri(angular)
C
C   CONVERT GAMMA CHAN # TO ENERGY
c
C  SINCE EG'S ARE REAL, 1 COUNT IS SHARED OVER 4 ELEMENTS OF tri 
c
c   To sort into tri
C    WE MUST ALSO CONVERT EACH X,Y PAIR TO A SINGLE INDEX IN tri
c   loop over pairs of detectors
c
c100	IF (IC(12) .LT. ITG(1) .OR. IC(12) .GT. ITG(2)) GO TO 26
 100	do 150 jj=1,109,2
	INX = IT(JJ)
	IF (INX .LE. 0) GO TO 26
	ENXM = EC(INX)
	IF (ENXM .LE. 1.) GO TO 150
	IXM = ENXM
	IF ((ENXM - IXM) .GT. 0.5) IXM = IXM + 1
	ENXP = EC(INX) + CAL(2,INX) / CALNEW
	IXP = ENXP
	IF ((ENXP - IXP) .GT. 0.5) IXP = IXP + 1
c	write(1,*) jj,inx,ixm,ixp,enxm

	INY = IT(JJ+1)
	ENYM = EC(INY)
	IF (ENYM .LE. 1.) GO TO 150
	IYM = ENYM
	IF ((ENYM - IYM) .GT. 0.5) IYM = IYM + 1
	ENYP = EC(INY) + CAL(2,INY) / CALNEW
	IYP = ENYP
	IF ((ENYP - IYP) .GT. 0.5) IYP = IYP + 1
	

	IF (IXP .GT. SIZ .OR. IYP .GT. SIZ) GO TO 150
	FRAC2 = (ENXP - ENXM) * (ENYP - ENYM)
	FRAC2 = 1./FRAC2

c*****  TEST REMOVE LATER
c	if (((ec(inx) .gt. 952.).and.(ec(inx).lt.956.)).and.((ec(iny).gt. 952.)
c     +  .and.(ec(iny).lt.956.))) then
c	write (1,*) ec(inx),ec(iny),numadc,(ic(k),k=1,iadc),(ec(k),k=1,iadc)
c	endif
c*****  END TEST

C
	TM = ENXM
	TP = ENXP

	XI = FLOAT(IXM)
	IF (TP .GT. (XI+0.5) ) TP = XI + 0.5
	FXM = TP - TM

	TP = ENXP

	XI = FLOAT(IXP)
	IF (TM .LT. (XI-0.5) ) TM = XI - 0.5
	FXP = TP - TM
	IF (IXM .EQ. IXP) FXP = 0.
C
	TM = ENYM
	TP = ENYP

	XI = FLOAT(IYM)
	IF (TP .GT. (XI+0.5) ) TP = XI + 0.5
	FYM = TP - TM

	TP = ENYP

	XI = FLOAT(IYP)
	IF (TM .LT. (XI-0.5) ) TM = XI - 0.5
	FYP = TP - TM

	IF (IYM .EQ. IYP) FYP = 0.
c
	IX = MAX(IXM ,IYM )
	IY = MIN(IXM ,IYM )
	IND = IX - SIZ + (SIZ1-IY)*IY/2
	tri(IND) = tri(IND) +FXM * FYM * FRAC2
C
	IX = MAX(IXP,IYM )
	IY = MIN(IXP,IYM )
	IND = IX - SIZ + (SIZ1-IY)*IY/2
	tri(IND) = tri(IND) + FXP * FYM * FRAC2
C
	IX = MAX(IXM ,IYP)
	IY = MIN(IXM ,IYP)
	IND = IX - SIZ + (SIZ1-IY)*IY/2
	tri(IND) = tri(IND) + FXM * FYP * FRAC2
C
	IX = MAX(IXP,IYP)
	IY = MIN(IXP,IYP)
	IND = IX - SIZ + (SIZ1-IY)*IY/2
	tri(IND) = tri(IND) + FXP * FYP * FRAC2
c
150	continue
C
	go to 26
c
60	IF (MOD(IREC,1000) .EQ. 0) WRITE (6,*) IREC,' records sorted.'
        IREC=IREC+1
	IF (IREC .GT. IRMAX .AND. IRMAX .NE. 0) GO TO 800
	GO TO 20
800	IREC = IREC - 1
	write (6,*) IREC,' RECORDS SORTED'
	CLOSE(3)
 400	continue
	return
c           call closetape(idev)
c        if (knterr .eq. 0) return
c	write (6,*) ' ! ! W A R N I N G ! ! ',knterr,'  tape read error(s)'
c	RETURN
 980	write (6,*) 'Error in reading file:',file1
	close(3)
	return
990	write (6,*) ' OPEN ERROR ON',FILE,file1
c           call closetape(idev)
	CLOSE(3)
	RETURN
 995	write (6,*)'Error in the # of ADCs in an event!'
	close(3)
	return
	END

C
C **  TAPE SAVE AND RESTORE BIG
C
	SUBROUTINE TAPEIO(TDEV)
	CHARACTER FILE*80,RUN*5,SWT*1,filet*80,repent*1
	integer tdev
	integer siz,dsiz,siz1
	INTEGER*2 kclvr(4,4)
	parameter (siz=3000)
	parameter (dsiz=(siz*siz+siz)/2)
	COMMON /BIG/tri(dsiz)
	COMMON /GATES/ ITG(4),CAL(3,50),ISIZE,numgam,IADC,it(110),
     + nclvr,kclvr,calnew
      COMMON /ZX/ MAXCHN,NUMRUN,IACC,NTOTAL,IFAST,ISCRN,KZERO
	data filet/'/usr/users/tabor/triangle.twd'/
	IF (ISIZE .EQ. 0) ISIZE = siz
	ITYPE = 3
1	write (6,*) 'TYPE 1 TO SAVE OR 2 TO RESTORE TWOD ARRAY'
	write (6,*) '0 to abort or 3 to change file name'
	read (5,*) IRW
	if (irw .eq. 0) return
	IF (IRW .LT.1 .OR. IRW .GT. 3) GO TO 1
	if (irw .eq. 3) then
	  write (6,*) 'Type twod file name'
	  read (5,109) filet
109	  format (a80)
	  write (6,110) filet
110	  format ('New TWOD file name is ',a80)
	  goto 1
	endif
	if (tdev .eq. 0) file = filet
	if (tdev .eq. 1) file = '/dev/nrmt1/'
	if (tdev .eq. 2) file = '/dev/rmt1/'
	IF (IRW .GE. 2) GO TO 50
	write (6,111) file
111	format ('About to write twod array to file  ',a80)
	write (6,*) 'Do you wish to do this?  [Y or N] '
	read (5,112) repent
 112    format (a1)
	if (repent .ne. 'Y' .and. repent .ne. 'y') go to 1
	OPEN (2,FILE=FILE,FORM='UNFORMATTED',STATUS='unknown',ERR=990)
	write (6,*) ' WRITING ARRAY'
	WRITE (2,ERR=995) siz,dsiz,itype
	WRITE (2,ERR=995) tri
	CLOSE (2,ERR=995)
	RETURN
50	OPEN (2,FILE=FILE,FORM='UNFORMATTED',STATUS='OLD'
     .  ,ERR=990)
	write (6,*) ' READING ARRAY'
	IF (IRW .EQ. 2) READ (2,ERR=995) ISIZ,ICOUNT,ITYPE
	READ (2,ERR=995) tri
	CLOSE (2,ERR=995)
	IF (IRW .EQ. 3) RETURN
	write (6,*) 'Read TWOD array;  ISIZE, ICOUNT, ITYPE ='
	write (6,*) ISIZ,ICOUNT,ITYPE
	RETURN
990	write (6,992) FILE
992	FORMAT (' OPEN ERROR ON ',A80)
	RETURN
 995    write (6,993) file
 993    format (' READ, WRITE or CLOSE error on ',a80)
        return
	END
C
C **  PROJECT 1D SPECTRA FROM 2D ARRAY- IG1 = -1 IF CALLED FROM MAIN
C
	SUBROUTINE PROJ(IDATA,IG1,IG2)
	CHARACTER LABEL*6,Q*1,ANS*1,SWT*1
	integer siz,dsiz,siz1
	parameter (siz=3000)
	parameter (dsiz=(siz*siz+siz)/2)
	PARAMETER (MAXSIZ=200000)
      INTEGER*4 IDATA(MAXSIZ),truet(50),trutim
	integer*2 spc2(8192)
	INTEGER*2 kclvr(4,4)
	COMMON /BIG/tri(dsiz)
      COMMON /SPC/ NUMSPC,LABEL(50),NUMCHN(50),IPOINT(50),
     + MRKL(50),MRKR(50),ISPC,SPC2,TRUTIM,TRUET
	COMMON /GATES/ ITG(4),CAL(3,50),ISIZE,NUMGAM,IADC,it(110),
     + nclvr,kclvr,calnew
	if (isize .lt. 1) return
	ISIZE1 = ISIZE*2 + 1
	ADD = 0.
	IF (IG1 .GT. 0) GO TO 300
	jadc = 1
	ISPC = 1
1	write (6,*) ' Do you want the gate wide open [Y,N] ? '
c	read (5,101)Q
c101	FORMAT (A1)
        insize = 1
        CALL grab(Q,insize,insize)
	IF (Q .EQ. 'N' .or. q .eq. 'n') GO TO 100
	IF (Q .NE.'Y' .and. q .ne. 'y') GO TO 1
	IG1=1
	IG2=ISIZE
	IG3=0
	IG4=0
	write (6,*) ' Projecting . . .'
	GO TO 205
100	write (6,*) ' TYPE MIN & MAX CHAN #S FOR ADD GATE'
	read (5,*) IG1,IG2
	if (ig1 .lt. 1 .or. ig2 .lt. 1) return
	IF(IG1.GT.ISIZE.OR.IG2.GT.ISIZE)GOTO100
105	write (6,*) ' TYPE MIN & MAX CHAN #S FOR SUB GATE [or - cnts/chan ]'
	read (5,*) IG3,IG4
	IF(IG3.GT.ISize.OR.IG4.GT.ISIZE)GOTO105
102	write (6,*)' Type spectrum number and constant to add ?'
     +  ,'  [0 TO ABORT]'
	read (5,*) ISPC,ADD
205	IF (ISPC .EQ. 0) RETURN
	IF (ISPC .LT. 1) GO TO 102
	IF (ISPC .LE. NUMSPC) GO TO 200
	CALL CREATS (ISIZE,'GAMMA ',ISPC,IRES)
	IF (IRES .EQ. 1) RETURN
200	IGD=IG4-IG3
	FAC = FLOAT(IG2-IG1+1)/FLOAT(IGD+1)
	if(ig3.lt.0.and.truet(1).gt.0)fac=-float(ig2-ig1+1)*ig3/truet(1)
	if(ig3.lt.0.and.truet(1).le.0) fac = 0
c
c    project from triangular array
c
	kount = 0
        count1 = 0.
	DO 210 J=1,ISIZE
	TMP = 0.
	DO 220 I = IG1,IG2
	IX = MAX(I,J)
	IY = MIN(I,J)
	IND = IX - ISIZE + (ISIZE1-IY)*IY/2
	IF (IX .EQ. IY)	TMP = TMP + tri(IND)
220	TMP = TMP + tri(IND)
	if (ig3 .lt. 0) then
	  ich = ipoint(1) + j - 1
	  tmp = tmp - fac*float(idata(ich))
	else IF (IGD .gt. 0) then
	  DO 230 I=IG3,IG4
	  IX = MAX(I,J)
	  IY = MIN(I,J)
	  IND = IX - ISIZE + (ISIZE1-IY)*IY/2
	  IF (IX .EQ. IY) TMP = TMP - tri(IND)*FAC
230	  TMP = TMP - tri(IND)*FAC
	end if
225	TMP = TMP + ADD
	IF (TMP .LT. 0.) TMP=0.
	ICH = IPOINT(ISPC) + J - 1
	IDATA(ICH)=TMP
        count1 = count1 + tmp
210	kount = kount + idata(ich)
	truet(ispc) = kount
	write (6,*) kount,' counts in spectrum.'
        count1 = count1 / 2.
        write (6,*) count1,' pairs in array.'
	return
c
300	IG3=MRKL(ISPC)
	IG4=MRKR(ISPC)
	q = 'N'
	INPUTSIZE = 1
	CALL GRAB(ANS,INPUTSIZE,INPUTSIZE)
	SPC = ICHAR(ANS)
	ISPC = SPC-48
	GO TO 205
	END
C

