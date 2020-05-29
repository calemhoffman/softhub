
C
C  THIS ROUTINE MANIPULATES THE NSM DATA WITHIN SCOPE
C  CURRENTLY IT ALLOWS ADDING, subtracting, COMPRESSING,
c  moving, normalizing, gainshifting AND CONVERTING TO LOG
C
	SUBROUTINE MANIP(IDATA)
	PARAMETER (MAXSIZ=32768)
      CHARACTER ANSM*1,LABEL*6
	dimension temp(8192)
      INTEGER*4 IDATA(MAXSIZ),truet,trutim
	INTEGER*2 spc2(8192)
      COMMON /SPC/ NUMSPC,LABEL(50),NUMCHN(50),IPOINT(50),
     + MRKL(50),MRKR(50),ISPC,spc2,trutim,truet(50)
      COMMON /ZX/ MAXCHN,NUMRUN,IACC,NTOTAL,IFAST,ISCRN,KZERO
299	write (6,*) 'Add  Compress  Gainshift  Log',
     .' Move  Normalize  Root  Subtract  Title'
	INPUTSIZE=1
	CALL GRAB(ANSM,INPUTSIZE,INPUTSIZE)
	  IF (ANSM .EQ. 'A' .or. ansm .eq. 'S') THEN
300	If(ansm.eq.'A') write (6,*) ' SpecA + SpecB = Dest'
	If(ansm.eq.'S') write (6,*) ' SpecA - SpecB = Dest'
	write (6,*) ' TYPE SpecA, SpecB, Dest'
	read (5,*) i1,i2,id
	if(ansm.eq.'S')write (6,*)'Type constant to add to each channel'
	if(ansm.eq.'S') read (5,*) const
	IF (ID.LT.1 .OR. I1.LT.1 .OR. I2.LT.1) GO TO 300
	IF (I1.GT.NUMSPC .OR. I2.GT.NUMSPC) GO TO 300
	NUMPT = NUMCHN(I1)
	IF (NUMCHN(I2) .EQ. NUMPT) GO TO 310
302	write (6,*) ' Unequal spectrum sizes - Cannot combine them.'
	RETURN
310	CALL CHECK (NUMPT,LABEL(I1),ID,ISUCCS)
	IF (ISUCCS .EQ. 1) RETURN
318	DO 320 I=1,NUMPT
	NCHD=I + IPOINT(ID)-1
	NCH1=I + IPOINT(I1)-1
	NCH2=I + IPOINT(I2)-1
	if(ansm.eq.'A') idata(nchd) = idata(nch1) + idata(nch2)
	if(ansm.eq.'S') idata(nchd) = idata(nch1) - idata(nch2) + const
320	continue
	if(ansm.eq.'A') truet(id) = truet(i1) + truet(i2)
	if(ansm.eq.'S') truet(id) = truet(i1) - truet(i2)
C
	  ELSE IF (ANSM .EQ. 'C') THEN
	write (6,*) ' Spectrum number to compress?  [0 to ABORT] '
	read (5,*) IS
	IF (IS .LT. 1 .OR. IS .GT. NUMSPC) RETURN
	write (6,*)  ' Compression factor ? '
	read (5,*) ICOMP
	IF (ICOMP .LE. 1) RETURN
	NUMCHN(IS)=NUMCHN(IS)/ICOMP
	DO 330 I = 1, NUMCHN(IS)
	ITMP = 0
	DO 332 J = 1, ICOMP
	IPT = (I-1)*ICOMP + J
	IPT = IPT + IPOINT(IS) -1
	ITMP = ITMP + IDATA(IPT)
332	CONTINUE
	IPT = IPOINT(IS) + I - 1
	IDATA(IPT) = ITMP
330	CONTINUE
C
	  ELSE IF (ANSM .EQ. 'L') THEN
	write (6,*) ' Spectrum number to convert to log [0 to abort] ? '
	read (5,*) IS
	IF (IS .LT. 1 .OR. IS .GT. NUMSPC) RETURN
	I1 = IPOINT(IS)
	I2 = I1 + NUMCHN(IS)-1
	DO 340 I = I1,I2
	TMP = IDATA(I)
	IF (TMP .GT. 0) TMP = 1000.*ALOG(TMP)
340	IDATA(I) = TMP
c
	  ELSE IF (ANSM .EQ. 'R') THEN
	write (6,*) ' Spectrum number to take square root of',
     .  ' [0 to abort] ? '
	read (5,*) IS
	IF (IS .LT. 1 .OR. IS .GT. NUMSPC) RETURN
	I1 = IPOINT(IS)
	I2 = I1 + NUMCHN(IS)-1
	DO 350 I = I1,I2
	TMP = IDATA(I)
	IF (TMP .GT. 0) TMP = 10.*sqrt(TMP)
350	IDATA(I) = TMP
C
	  ELSE IF (ANSM .EQ. 'M') THEN
	write (6,*) 'Type source, destination spectrum numbers.'
	read (5,*) IS,ID
	IF (IS .LT. 1 .OR. IS .GT. NUMSPC) RETURN
	NUMPT = NUMCHN(IS)
	CALL CHECK (NUMPT,LABEL(IS),ID,ISUCCS)
	IF (ISUCCS .EQ. 1) RETURN
	DO 410 I = 1,NUMPT
	NCHD = I + IPOINT(ID) - 1
	NCH1 = I + IPOINT(IS) - 1
410	IDATA(NCHD) = IDATA(NCH1)
	truet(id) = truet(is)
c
	  else if (ansm .eq. 'N') then
	write (6,*) 'Type spectrum number to Normalize'
	read (5,*) is
	if (is .lt. 1 .or. is .gt. numspc) return
	write (6,*) 'Type factor to multiply counts in ',
     .'each channel by.'
	read (5,*) fac
	numpt = numchn(is)
	do 420 i = 1, numpt
	nch1 = i + ipoint(is) - 1
420	idata(nch1)= fac*float(idata(nch1))+ 0.5
	truet(is) = fac * truet(is)
C
	  Else if (ansm .eq. 'G') then
	write (6,*) ' Spectrum number to gainshift? [0 to abort]'
	read (5,*) is
	if (is .lt. 0 .or. is .gt. numspc) return
	write (6,*) ' Old calibration? - A + B * ch + C * ch^2'
	read (5,*) a,b,c
	write (6,*) ' What is new keV/ch ? [0 to abort]'
	read (5,*) calnew
	if (calnew .lt. 0.) return
	ilast = ipoint(is) + numchn(is) - 1
	ibegin = ipoint(is)
	numpt = numchn(is)
	do 75 i = 1,numpt
75	temp(i) = 0.
	do 80 ipt = ibegin,ilast
	oldm = ipt - ipoint(is) - 0.5
	oldm2 = oldm*oldm
	oldp = oldm + 1.
	oldp2 = oldp*oldp
	anewm = (a + b*oldm + c*oldm2)/calnew
	newm = anewm
	if ((anewm - newm) .gt. 0.5) newm = newm+1
	anewp = (a + b*oldp + c*oldp2)/calnew
	newp = anewp
	if ((anewp - newp) .gt. 0.5) newp = newp+1
	if (newm .le. 0) go to 80
	if (newp .gt. 8192) go to 80
	frac = 1./(anewp - anewm)
	counts = idata(ipt)
	do 70 i = newm,newp
	xi = float(i)
	tm = anewm
	if (tm .lt. (xi-0.5)) tm = xi - 0.5
	tp = anewp
	if (tp .gt. (xi+0.5)) tp = xi+0.5
70	temp(i) = temp(i) + counts*frac*(tp-tm)
80	continue
	write(6,*)'Number of channels in new specturm [0 for old #]?'
	read (5,*) numpt1
	if (numpt1 .le. 0) numpt1 = numpt
	call check(numpt1,label(is),is,isuccs)
	if (isuccs .eq. 1) return
	do 85 i = 1, numpt1
	j = i + ibegin
85	idata(j) = temp(i) + .5
	numchn(is) = numpt1
c
c   Change title of spectrum
c
	else if (ansm .eq. 'T') then
	write (6,*) 'Type spectrum number to be titled'
	read (5,*) id
	if (id .le. 0 .or. id .gt. numspc) return
	write (6,*) 'Type new 6-character title'
	read (5,303) label(id)
303	format (a6)
	
	  END IF
	RETURN
	END
C
C
C   THIS SUBROUTINE CHECKS WHETHER SPECTRUM ID NEEDS TO BE CREATED
C   (IN WHICH CASE IT CALLS CREATS)  OR WHETHER THE EXISTING SPECTRUM
C   IS LARGE ENOUGH OR CAN BE MADE LARGE ENOUGH
C
	SUBROUTINE CHECK (NUMPT,LABELN,ID,ISUCCS)
	PARAMETER (MAXSIZ=32768)
      CHARACTER ANSM*1,LABEL*6,LABELN*6
      INTEGER*4 IDATA(MAXSIZ),truet,trutim
	INTEGER*2 spc2(8192)
      COMMON /SPC/ NUMSPC,LABEL(50),NUMCHN(50),IPOINT(50),
     + MRKL(50),MRKR(50),ISPC,spc2,trutim,truet(50)
      COMMON /ZX/ MAXCHN,NUMRUN,IACC,NTOTAL,IFAST,ISCRN,KZERO
	ISUCCS = 0
	IF (ID .GT. NUMSPC) THEN
	  CALL CREATS(NUMPT,LABELN,ID,ISUCCS)
	  RETURN
	ELSE
	  IF (ID .EQ. NUMSPC) THEN
	    ISPACE = MAXCHN +1 - IPOINT(ID)
	  ELSE
	    ISPACE = IPOINT(ID+1) - IPOINT(ID)
	  ENDIF
	  IF (ISPACE .LT. NUMPT) THEN
	    write (6,*) 'Not enough room for spectrum ',ID
	    ISUCCS = 1
	    RETURN
	  ELSE
	    NUMCHN(ID)=NUMPT
	    RETURN
	  ENDIF
	ENDIF
	END
