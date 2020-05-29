C
C  DEVIO is called by I/o dev and allows the user to select devices
c  for each I/O function or the mount, initialize, etc a device.
C
	SUBROUTINE DEVIO(NDEV,IDEV,ODEV,MDEV,TDEV)
      CHARACTER ANS*1,ANSM*1
	CHARACTER MSGD*2,MSGr*22,MSGM*28,MSGI*18
	INTEGER IDEV,ODEV,MDEV,TDEV,SDEV
	DATA MSGD/'ls'/,MSGr/'mt f /dev/rmt32 rewind'/
	DATA MSGM/'MOU/OVER=OWNER MSA0: TANDEM%'/
	DATA MSGI/'INIT MSA0: TANDEM%'/
1	write (6,9010)
9010	FORMAT  (' CHOOSE:  D irectory   I nitialize   R ewind',
     . '   S elect')
	LEN = 1
	CALL GRAB (ANS,LEN,LEN)
      IF (ANS .EQ. 'S' .or. ans .eq. 's') THEN
9025  FORMAT(/20X,' DEVICES',/,
     +' -------------------------------------------------------------'/,
     +' 0 = disk           1 = tape32             2 = tape34',/)
9030  FORMAT(' Current input device  <',i2,'>',/,
     +' Select new input device or RETURN')
9035  FORMAT(/' Current output device  <',i2,'>',/,
     +' Select new output device or RETURN')
9040	FORMAT (/' Current multiparameter device <',i2,'>',/,
     +' Select new multiparameter device or RETURN')
9045	FORMAT (/' Current TWOD device <',i2,'>',/,
     +' Select new TWOD device or RETURN')
	write (6,9025)
        write (6,9030) IDEV
        CALL SETDEV(IDEV)
        write (6,9035) ODEV
        CALL SETDEV(ODEV)
	if (ndev .lt. 3) return
	write (6,9040) MDEV
	CALL SETDEV(MDEV)
	IF (NDEV .LT. 4) RETURN
	write (6,9045) TDEV
	CALL SETDEV(TDEV)
C
      ELSE IF (ANS .EQ. 'D' .or. ans .eq. 'd') THEN
	write (6,9025)
	write(6,*) 'Select device for directory listing'
	CALL SETDEV (SDEV)
	CALL System (MSGD)
C
      else if (ans .eq. 'R' .or. ans .eq. 'r') then
	call system(msgr)
c
      ELSE IF (ANS .EQ. 'I') THEN
	write (6,9025)
	write(6,*) 'Select device to initialize'
	CALL SETDEV (SDEV)
	write (6,110)SDEV
110	FORMAT (' !WARNING!  This will destroy all old data on ',i2
     . /' Do you still want to initialize it? [Y,N] ')
	CALL GRAB (ANSM,LEN,LEN)
	IF (ANSM .NE. 'Y') RETURN
	CALL System(MSGI)
      ELSE
	RETURN
      END IF
	RETURN
      END
C
C  SPNCMD allows the user to spawn any UNIX command typed without
C  leaving the main program. 
C
	SUBROUTINE SPNCMD
	CHARACTER COMMAND*80
	write(6,*) 'Type UNIX command to spawn'
	READ (5,9020)COMMAND
9020	FORMAT (A80)
	CALL System(COMMAND)
	RETURN
	END

C
	SUBROUTINE LISTSP(IDATA) 
C
C  LISTSP lists the counts in a spectrum.  It is currently optimized
c  to send as few characters to the terminal as possible for text
c  capture.  It can be used to transfer a spectrum to the PC.
C
	PARAMETER (MAXSIZ=32768)
      CHARACTER ANS*1,FMT*17,LABEL*6
      INTEGER*4 IDATA(MAXSIZ),DUM(10),truet,trutim
	integer*2 spc2(8192)
      COMMON /SPC/ NUMSPC,LABEL(50),NUMCHN(50),IPOINT(50),
     + MRKL(50),MRKR(50),ISPC,SPC2,TRUTIM,TRUET(50)
      COMMON /ZX/ MAXCHN,NUMRUN,IACC,NTOTAL,IFAST,ISCRN,KZERO
	DATA FMT/'(1X,9(I6,'',''),I6)'/
400	write(6,*) ' Type spectrum number to list'
	read (5,*) ISP
	IF (ISP .LT. 1 .OR. ISP .GT. NUMSPC) GO TO 400
	write(6,*) NUMCHN(ISP)
	LIM = NUMCHN(ISP)/10
	DO 410 I=1,LIM
	MAX = 1
	K2 = I*10 - 1 + IPOINT(ISP)
	K1 = K2 - 9
	J=1
	DO 420 K=K1,K2
	DUM(J)=IDATA(K)
	IF (DUM(J) .LT. 0) DUM(J)=0
	J=J+1
420	IF (IDATA(K) .GT. MAX) MAX = IDATA(K)
	MAX = ALOG10(FLOAT(MAX))+1
	ANS = CHAR(MAX+48)
	FMT(8:8)=ANS
	FMT(16:16)=ANS
	write (6,FMT)(DUM(J),J=1,10)
410	CONTINUE
	RETURN
	END
C
	SUBROUTINE CREATS(NUMPT,LABELN,ID,IRES)
C
C  This routine creates a new spectrum with NUMPT channels
c  and a label of LABELN.  The new spectrum number will be ID
c  if it is the next available one.  If not, ID will be changed
c  to the next available (NUMSPC + 1) and the user will be informed.
c  If there is not enough room left, CREATS will fail and return
c   with IRES = 1 .  If the file is created successfully, IRES = 0.
c
	CHARACTER LABEL*6,LABELN*6
c	INTEGER*2 NUMRUN,NUMSPC,NUMCHN
	integer*4 truet,trutim
	integer*2 spc2(8192)
      COMMON /SPC/ NUMSPC,LABEL(50),NUMCHN(50),IPOINT(50),
     + MRKL(50),MRKR(50),ISPC,SPC2,TRUTIM,TRUET(50)
      COMMON /ZX/ MAXCHN,NUMRUN,IACC,NTOTAL,IFAST,ISCRN,KZERO
	IRES = 0
	IF (ID .EQ. NUMSPC+1) GO TO 314
	ID = NUMSPC+1
	write(6,*) ' Destination spectrum number changed to ',ID
314	NUMSPC=NUMSPC+1
	NUMCHN(ID)=NUMPT
	LABEL(ID)=LABELN
	IPOINT(ID) = 1
	IF(NUMSPC.GT.1)IPOINT(ID)=NUMCHN(NUMSPC-1)+IPOINT(NUMSPC-1)
	ICHECK = IPOINT(ID) + NUMCHN(ID)
	IF (ICHECK .LE. MAXCHN) RETURN
	IRES = 1
	NUMSPC = NUMSPC - 1
        write (6,*) 'NOT ENOUGH ROOM FOR SPECTRUM ',ID
	RETURN
	END
C
C **  Set device and unit specifier for valid device
C
      SUBROUTINE SETDEV(NDEV)
      CHARACTER IOD*1,CR*1
	CR = CHAR(13) 
C
   10 	LEN = 1
	CALL GRAB (IOD,LEN,LEN)
      IF ( IOD .EQ. CR ) RETURN
      IF ((IOD .EQ. '0') .OR. (IOD .EQ. '1') .or. (iod .eq. '2')) THEN
        IF (IOD .EQ. '0') NDEV=0
        IF (IOD .EQ. '1') NDEV=1
	if (iod .eq. '2') ndev=2
      ELSE
        write (6,1000)
 1000   FORMAT (' UNRECOGNIZED DEVICE -- TRY AGAIN')
        GO TO 10
      ENDIF
      RETURN 
      END
C
C **
C
C   WRITED writes out NSM spectra in the "new" format.
c   This version includes an option to control the range
c   of spectra written out.  
c   In order to obtain the default of writing all defined spectra,
c   call it as  CALL WRITED (IDATA,ODEV,1,NUMSPC)
C
      SUBROUTINE WRITED(IDATA,ODEV,IFIRST,ILAST)
      CHARACTER FILE*40,file1*40,RUN*5,DAT*9,TIM*8,LABEL*6,str*24
	INTEGER*2 NUMSP1,ITYPE,SPC2(8192),numru1,numch1
	INTEGER*4 TRUET,TRUTIM,odev
      INTEGER OUNIT
	PARAMETER (MAXSIZ=32768)
      COMMON /SPC/ NUMSPC,LABEL(50),NUMCHN(50),IPOINT(50),
     + MRKL(50),MRKR(50),ISPC,SPC2,TRUTIM,TRUET(50)
      COMMON /ZX/ MAXCHN,NUMRUN,IACC,NTOTAL,IFAST,ISCRN,KZERO
      INTEGER*4 IDATA(MAXSIZ)
	DATA OUNIT/1/
	CALL fdate(str)
	tim(1:8) = str(12:19)
	dat(1:2) = str(9:10)
	dat(3:3) = '-'
	dat(4:6) = str(5:7)
	dat(7:7) = '-'
	dat(8:9) = str(23:24)
	numru1=numrun
      write (run,902) NUMRUN
902   FORMAT (I5)
      FILE(1:3) = 'RUN'
      FILE(4:8) = RUN
      FILE(9:40) = '.NSM                            '
	call squeze(file,file1)
	ICOUNT=0
      OPEN (UNIT=OUNIT,STATUS='NEW',FORM='UNFORMATTED',
     + FILE=FILE1,ERR=999)
	NUMSP1=ILAST-IFIRST+1
      WRITE(OUNIT) NUMRU1,NUMSP1,TRUTIM,DAT,TIM
      DO 100 I = IFIRST,ILAST
	numch1=numchn(i)
        J1 = IPOINT(I)
        J2 = J1 + NUMCHN(I) - 1
	ITYPE=2
	I1=1
	DO 110 J=J1,J2
	ITM=IDATA(J)
	IF (ITM .GT. 32767) GO TO 120
	SPC2(I1)=ITM
	I1=I1+1
110	CONTINUE
	ITYPE=1
	WRITE(OUNIT) NUMCH1,ITYPE,LABEL(I),TRUET(I)
	I1=I1-1
	WRITE(OUNIT) (SPC2(J),J=1,I1)
	GO TO 100
120	WRITE(OUNIT) NUMCH1,ITYPE,LABEL(I),TRUET(I)
	WRITE(OUNIT) (IDATA(J),J = J1 , J2 )
100   CONTINUE
      CLOSE(UNIT=OUNIT)
      write (6,9130) NUMRUN,NUMSP1,FILE1
9130  FORMAT(' RUN ',I5,' ( ',I4,' SPECTRA ) OUTPUT TO FILE ',A17)
      RETURN
999   write (6,9030) IOS,FILE1
9030  FORMAT(' OPEN ERROR',I8,'  ON FILE ',A17)
      RETURN
      END
C
C **
C
C  READD reads NSM spectra in either "new" or "old" format.  It
c  senses the format and switches automatically.  The creation
c  time and date, as well as the scalers, will be typed out if
c  a "new" format NSM is read.  Their absence indicates an "old" NSM.
c
      SUBROUTINE  READD(IDATA,IDEV)
	PARAMETER (MAXSIZ=32768)
      CHARACTER FILE*40,file1*40,RUN*5,DAT*9,TIM*8,LABEL*6,LAB(50)*2
	CHARACTER IOD*1
      INTEGER IUNIT
	INTEGER*2 ITYPE,SPC2(8192),numru1,numsp1,numch1
      INTEGER*4 IDATA(MAXSIZ),TRUET,TRUTIM
      COMMON /SPC/ NUMSPC,LABEL(50),NUMCHN(50),IPOINT(50),
     + MRKL(50),MRKR(50),ISPC,SPC2,TRUTIM,TRUET(50)
      COMMON /ZX/ MAXCHN,NUMRUN,IACC,NTOTAL,IFAST,ISCRN,KZERO
	DATA IUNIT/1/
	NUMOLD = NUMSPC
      write (6,9000)
9000  FORMAT (' Enter run #:')
      read (5,901) NUMRUN
901   FORMAT (I5)
      write (run,902) NUMRUN
902   FORMAT (I5)
      FILE(1:3) = 'RUN'
      FILE(4:8) = RUN
      FILE(9:40) = '.NSM                            '
	call squeze(file,file1)
      OPEN (UNIT=IUNIT,STATUS='OLD',FORM='UNFORMATTED',
     + FILE=FILE1,ERR=999)
      READ(IUNIT,ERR=9041) NUMRU1,NUMSP1,TRUTIM,DAT,TIM
	numrun = numru1
	numspc = numsp1
      J1 = 1
      DO 100 I = 1 , NUMSPC
	READ(IUNIT) NUMCH1,ITYPE,LABEL(I),TRUET(I)
	numchn(i) = numch1
        J2 = J1 + NUMCHN(I) - 1
        IF ( J2 .LE. MAXCHN ) GO TO 90
          write (6,9045) MAXCHN
9045      FORMAT (' TOTAL # OF CHANNELS EXCEEDS MAX OF ',I6 )
          NUMSPC = I - 1
          write (6,9046) NUMSPC
9046      FORMAT (' ONLY THE FIRST ',I4,' SPECTRA WERE READ')
          GO TO 150
90      IPOINT(I) = J1
	IF (ITYPE .EQ. 2) THEN
	  READ(IUNIT) ( IDATA(J), J = J1 , J2 )
	ELSE
	  MAX=NUMCHN(I)
	  READ(IUNIT) (SPC2(J),J=1,MAX)
	  I1=J1
	  DO 200 J=1,MAX
	  IDATA(I1)=SPC2(J)
200	  I1=I1+1
	END IF
        J1 = J2 + 1
100   CONTINUE
150   NTOTAL = J1 - 1
      CLOSE(UNIT=IUNIT)
      write (6,9130) NUMRUN,NUMSPC,FILE1,DAT,TIM
9130  FORMAT(' RUN ',I5,' ( ',I4,' SPECTRA) INPUT FROM FILE ',A17,/
     + ' DATE = ',A9,'    TIME = ',A8)
	write (6,9032) TRUTIM,(I,TRUET(I),I=1,NUMSPC)
9032	FORMAT(' TRUETIME =',I10,' ; LIVE TIMES =  ',
     +3(I2,':',I10,',')/6(I2,':',I10,',')/6(I2,':',I10,','))
      GO TO 300
999   write (6,9030)IOS,FILE1
9030  FORMAT(' OPEN ERROR',I8,'  ON FILE ',A17)
      RETURN
C
C  COMES HERE IF OLD DATA FILE
C
9041	READ (IUNIT) (LAB(I),NUMCHn(i),I=1,NUMSPC)
      J1 = 1
      DO 101 I = 1 , NUMSPC
	LABEL(I)=LAB(I)
        J2 = J1 + NUMCHN(I) - 1
        IF ( J2 .LE. MAXCHN ) GO TO 91
          write (6,9045) MAXCHN
          NUMSPC = I - 1
          write (6,9046) NUMSPC
          GO TO 151
91      IPOINT(I) = J1
	  READ(IUNIT) ( IDATA(J), J = J1 , J2 )
        J1 = J2 + 1
101   CONTINUE
151   NTOTAL = J1 - 1
      CLOSE(UNIT=IUNIT)
      write (6,9135) NUMRUN,NUMSPC,FILE1
9135  FORMAT(' RUN ',I5,' ( ',I4,' SPECTRA) INPUT FROM FILE ',A17)
C
300	IF (NUMSPC .GE. NUMOLD) RETURN
	NUM1 = NUMSPC + 1
301	write(6,*) ' Do you wish to keep old spectra ',num1,' to ',
     .  numold,' ? [Y,N]'
	LEN = 1
	CALL GRAB (IOD,LEN,LEN)
	IF (IOD .EQ. 'N') RETURN
	IF (IOD .NE. 'Y') GO TO 301
	KEEP = NUMSPC
	NUMSPC = NUMOLD
	NEWLIM = IPOINT(KEEP) + NUMCHN(KEEP)
	IF (NEWLIM .LE. IPOINT(NUM1)) RETURN
	DO 310 IS = NUM1+1,NUMOLD
310	IF (NEWLIM .LE. IPOINT(IS)) GO TO 320
	write(6,*) ' All the old spectra were overwritten !'
	NUMSPC = KEEP
	RETURN
320	IS1 = IS - 1
	write(6,*) ' Note that old spectra',num1,' to',is1,'
     .  were overwritten and should not be used'
      RETURN
      END
c
c  simple grab function to get started.  It reads one character in
c  Fortran so N is ignored.
c
	subroutine grab(a,n,m)
	character a*1
	byte getmeachar
	a = char(getmeachar() )
	return
	end
