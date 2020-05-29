      PROGRAM cmprs

C     12-11-99  Copies and compresses a tape to a harddrive.
C     The compression is done by removing zeroes from the data.
C
C     The format for the compressed files will be...
C     a negative number indicating the beginning of an event,
C     followed by the number of ADC's with hits, and last the
C     ADC# followed by the channel# for each hit in the event.
C
C     Modified May 14, 2002 to read new data on disk.  SLT
C
C     Structure of file.set:  destination directory  source directory (if from disk)
C                             number of ADCs
C                             new run number  old run number[only used if from disk]
C                             (repeat run numbers as needed)
C                             -1 0  (to end)

C     VARIABLE DEFINITIONS
C
c     row = # of rows in the setup file (ie. # of runs on tape)
c     rnnum = run# to be read/saved
c     set = a flag telling the main program that a setup file has be loaded
c     idev = device id
c     ans = a switch taken from the main menu
c     


      
      implicit integer(a-z)
      PARAMETER (MAXSIZE=2044)
      integer row, rnnum, set, nadc, positn
      integer*2 ihits, icnt, obuff(MAXSIZE)
      character ans*1,dirname*80,dironame*80
      common /runs/ row,rnnum(200),ronum(200),set,ihits,icnt,nadc,obuff
      common /fil/ positn,dirname,dironame
      SET = 0
      DATA IDEV /0/
C     

 9000 FORMAT(/,
     +     ' -------------------------------------',/,
     +     ' Setup          Transfer          eXit',/,
     +     ' -------------------------------------')
 	        
 40   write (6,*) ' PROGRAM COMPRESS'
      write (6,9000)
      inputsize = 1
      CALL GRAB (ANS,INPUTSIZE,INPUTSIZE)
C     
      IF ( ANS .EQ. 'T' .or. ans .eq. 't') THEN
         CALL TRANSFR
         GO TO 40
C     
      ELSE IF ( ANS .EQ. 'S' .or. ans .eq. 's') THEN
         CALL SETUP
         goto 40
C     
      ELSE IF( ANS .EQ. 'X' .or. ans .eq. 'x') THEN
         STOP
C
      else
         write (6,*) '?'
         goto 40
      endif
      end


C
C **  Setup ADC's
C

C
C ** Reads in a run file, in which each run on tape is given a run number
C     that it will be saved as.  Giving a run number of zero should skip
C     the respective run.  Furthermore, a negative number should be used
C     to signal the end of the file.
C

C
c     VARIABLE DEFINITIONS
c
c     irow = do loop integer
c     filname = the filename for the setup file
c

      SUBROUTINE SETUP
      PARAMETER (MAXSIZE=2044)
      CHARACTER FILNAME*24,dirname*80,dironame*80
      INTEGER*2 ICNT, obuff(MAXSIZE), buffer, hits, ihits
      integer row, irow, rnnum, set, nadc, positn, ronum
      common /runs/ row,rnnum(200),ronum(200),set,ihits,icnt,nadc,obuff
      common /fil/ positn,dirname,dironame
      do 1 irow = 1,200
         rnnum(irow)=0
 1    continue
      WRITE (6,*) 'Enter name of table containing run information:'
      READ (5,109) filname
 109  format(a24)
      OPEN (2,FILE=FILNAME,STATUS='OLD',err=995)
      read(2,*) dirname,dironame
      read (2,*) nadc
      do 3 irow = 1, 200
         READ (2,*,ERR=996) rnnum(irow),ronum(irow)
         row = irow - 1
         if(rnnum(irow) .lt. 0) goto 5
 4       continue
 3    continue
 5    CLOSE (2,ERR=996)
      SET = 1
      write (6,*) 'Reading from ',dirname
      write (6,*) 'Writing to ',dironame
      write (6,*) nadc, '  ADCs'
      write (6,*) ' New Run numbers:  '
      write (6,*) (rnnum(irow),irow=1,row)
      write (6,*) ' Old Run numbers: (only used if reading from disk)'
      write (6,*) (ronum(irow),irow=1,row)
      RETURN
 995  write(6,*) ' Error on file.'
      return
 996  WRITE(6,*) ' Error reading run# setup.'
      RETURN
      END



C
C     **  Store data  **
C

c     Stores the data from a buffer into a file whose generated in the
c     transfer subroutine.  
c     Store has three options:  
c           1)  Initialize the output buffer (obuff), if iflg is negative
c           2)  Write the passed buffer into the output buffer, if iflg is
c                   zero
c           3)  Write the output buffer to file and then re-initialize, if
c                   iflg is positive

c     hits = # of hits in a event; passed from TRANSFER
c     ihits = # of 2 byte integers that were passed from TRANSFER to be
c             written to the output buffer


      SUBROUTINE STORE(IFLG, hits, buffer)
      PARAMETER (MAXSIZE=2044)
      character dirname*80,dironame*80
      INTEGER*2 ICNT, obuff(MAXSIZE), buffer, hits, ihits
      integer iflg, row, rnnum, set, nadc, positn, ronum
      common /runs/ row,rnnum(200),ronum(200),set,ihits,icnt,nadc,obuff
      common /fil/ positn,dirname,dironame

      ihits = (2 * hits)-1
      IF (IHITS .le. 0) IHITS = 0
      if (ihits .eq. 1) ihits = 2
      IHITS = IHITS + ICNT
      IF (IFLG .LT. 0) THEN
         DO 10 I = 1, maxsize
            OBUFF(I) = 0
 10      CONTINUE
         ICNT = 1
      ELSE IF (IFLG .eq. 0 .AND. IHITS .GT. maxsize) THEN
         WRITE(3) OBUFF
         DO 13 I = 1, maxsize
            OBUFF(I) = 0
 13      continue
         ICNT = 1
         OBUFF(ICNT) = buffer
         ICNT = ICNT + 1
      ELSE IF (IFLG .eq. 0 .AND. IHITS .LE. maxsize) then
         OBUFF(ICNT) = buffer
         ICNT = ICNT + 1
      ELSE IF (IFLG .GT. 0) THEN
         write (6,*) 'saving partial buffer'
         WRITE(3) OBUFF
         ICNT = 1
      ENDIF
      RETURN
      end

C
C     **  Transfer and compress data
C

c     
c     Reads and manipulates data from a tape.  TRANSFER should generate a
c     header for each event based on the number of hits and the first 2
c     ADC that received the hits.
c     The header is a 16 bit word and has the following format:
c                bits 1-4 = # of hits
c                bits 5-9 = ADC# for the first hit
c                bits 10-14 = ADC# for the second hit
c                bit 15 = unused
c                bit 16 = sign (1 = -, 0 = +)
c                         A negative number indicates a header.
c
c     The entire event is written as...
c                'header' '#counts in hit#1' '#counts in hit#2'
c                   'ADC# for 3rd hit' '#counts in hit#3' .... 
c                   'ADC# for Nth hit' '#counts in hit#N'
c

c
c     kskip = # of runs to skip
c     irmax = maximum # of records to read
c     ADC = numbers of the ADCs hit
c     hitcnt = counts the # of hits in an event
c     sngl = counts the # of single hits in the sort
c     dbl = counts the # of double hits in the sort
c     trpl = counts the # of triple hits in the sort
c     quad = counts the # of quadruple hits in the sort
c     mult = counts the # of hits greater than 4 in the sort
c     ibuf = input buffer
c     tmpstore = stores the channel# for each hit in an event
c     adcnum = stores the ADC# for each hit
c     numhits = counter, counting the # of hits
c     tmphead = temp. stores header
c     header = buffer header
c     ic = stores the channel numbers (w/ zeroes) for an entire event
c

      SUBROUTINE TRANSFR

      PARAMETER (MAXSIZE=2044)
      CHARACTER DRVNAME*24,filname*24,file*40,file1*40,file2*40,run*5,
     +     dirname*80,dironame*80,fileo2*40
      INTEGER KSKIP, IRMAX, rnnum, row, ADC(50), hitcnt, sngl,
     +     dbl, trpl, quad, mult, set, nadc, iflag, iflg, positn, ronum
      INTEGER*2 IBUF(512), tmpstore(50), adcnum(50), numhits, tmphead,
     +     header, ihits, icnt, bufr, ihts, obuff(MAXSIZE)
      LOGICAL contflg
      INTEGER*2 IC(50)
      common /runs/ row,rnnum(200),ronum(200),set,ihits,icnt,nadc,obuff
      common /fil/ positn,dirname,dironame
      idev = 0

      IF (SET .EQ. 1) THEN
         IRMAX = 0
         kruns = row
         kskip = 0
         GOTO 19
      ENDIF
      WRITE (6,*) 'Type number of file marks to skip:'
      READ (5,*) kskip
      write (6,*) ' ENTER NUMBER OF RECORDS TO SORT, 0 FOR ALL, -1 to
     +     ABORT'
      read (5,*) IRMAX
      IF (IRMAX .EQ. -1) RETURN
      if (irmax .eq. 0) then
         write (6,*) 'How many complete runs do you want to sort?'
         read (5,*) kruns
      endif
      if (irmax .gt. 0) kruns = 1
 19   knterr = 0
      CONTFLG = .FALSE.
      write (6,*) 'Type 0 or 1 for tape drive; 2 for disk'
      read (5,*) ktape
      if (ktape .lt. 0 .or. ktape .gt. 2) go to 19
      if (ktape .eq. 0) file = "/dev/nst0"
      if (ktape .eq. 1) file = "/dev/nst1"
      if (ktape.eq.0 .or. ktape.eq.1) then
          call opentape(idev,file,kskip)
          if (idev .lt. 0) go to 990
          endif
      iflg = -1
      ihts = 0
      bufr = 0
      call store (iflg,ihts,bufr)
      IF (SET .NE. 1) THEN
         DO 21 I = 1, 200
            RNNUM(I)=I
 21      CONTINUE
      ENDIF
      hitcnt = 0
      sngl = 0
      dbl = 0
      trpl = 0
      quad = 0
      mult = 0
      do 23 i = 1, nadc
         ADC(I) = 0
 23   CONTINUE
      do 400 krun = 1, kruns
         write (run,902) rnnum(krun)
         if (rnnum(krun) .eq. 0) then
            write(6,*) ' SKIPPING FILE',KRUN
            call closetape(idev)
            call opentape(idev,file,1)
            write (6,*) ' SKIPPED FILE',krun
            goto 400
         endif
 902     format (i5)
         positn = 80
 110     if (dirname(positn:positn) .EQ. ' ') THEN
            POSITN = POSITN - 1
            GOTO 110
         ENDIF
         file2(:positn) = dirname
         positn = positn + 1
         positn2 = positn + 2
         file2(positn:positn2) = 'run'
         positn = positn2 + 1
         positn2 = positn2 + 5
         FILE2(positn:positn2) = run
         positn = positn2 + 1
         positn2 = positn2 + 4
         FILE2(positn:positn2) = '.evt'
         positn = positn2 + 1
 111     IF(POSITN .LE. 40) THEN
            file2(positn:positn) = ' '
            POSITN = POSITN + 1
            GOTO 111
         ENDIF
         call squeze(file2,file1)
c         write (6,*) file2,file1
         ICOUNT=0
         OPEN (UNIT=3,STATUS='NEW',FORM='UNFORMATTED',
     +        FILE=FILE1,ERR=999)
         
      if (ktape .eq. 2) then
         write (run,902) ronum(krun)
         positn = 80
 121     if (dironame(positn:positn) .EQ. ' ') THEN
            POSITN = POSITN - 1
            GOTO 121
         ENDIF
         fileo2(:positn) = dironame
         positn = positn + 1
         positn2 = positn + 2
         fileo2(positn:positn2) = 'run'
         positn = positn2 + 1
         positn2 = positn2 + 5
         FILEo2(positn:positn2) = run
         positn = positn2 + 1
         positn2 = positn2 + 4
         FILEo2(positn:positn2) = '.mul'
         positn = positn2 + 1
 113     IF(POSITN .LE. 40) THEN
            fileo2(positn:positn) = ' '
            POSITN = POSITN + 1
            GOTO 113
         ENDIF
         call squeze(fileo2,file)
         kskip=0
         call opentape(idev,file,kskip)
         if (idev .lt. 0) go to 990
         endif
c         write (6,*) fileo2,file
         write (6,*) 'SORTING RUN, FILE',rnnum(krun),krun
         IREC=1
 20      icount = 1024
         call readtape(idev,ibuf,icount)
c         write (1,*) icount
c         write (1,1000) ibuf
c 1000    format (i8,10i6/8x,10i6)
c         if (irec .gt. 4) stop
         if (icount .eq. 1024) go to 22
         go to 800
 22      kount = icount/2
         IEV=0
         tmphead = 0
         header = 0
         IF (CONTFLG) GO TO 41
 35      IEV=IEV+1
         IF (IEV .GT. kount)  GO TO 60
 40      IHEAD = IBUF(IEV)
         IF (IHEAD.GE.0) GO TO 35
         JEVTYP = IAND(IHEAD,255)
            numhits= 0
            NNUM = 1
 41         DO 45 K=NNUM,nadc
               IEV=IEV+1
               IF (IEV .GT. kount) THEN
                  CONTFLG = .TRUE.
                  NNUM = K
                  GO TO 60
               END IF
               IC(K) = IBUF(IEV)
               IF(IC(K) .GT. 0 .and. numhits .le. 15) THEN
                  numhits = numhits + 1
                  tmpstore(numhits) = ic(k)
                  adcnum(numhits) = k
                  ADC(K) = ADC(K) + 1
                  HITCNT = HITCNT + 1
               ENDIF
               IF(IC(K) .LT. 0) then
                  GO TO 40
               ENDIF
               if(k .eq. nadc .and. ihead .lt. 0) then
                  header = ior(numhits,32768)
                  if(numhits .eq. 1) then
                     tmphead = (adcnum(1)*16 + header)
                     iflg = 0
                     ihts = 1
                     call store (iflg,ihts,tmphead)
                     iflg = 0
                     ihts = 0
                     call store (iflg,ihts,tmpstore(1))
                     sngl = sngl + 1
                  endif
                  if (numhits .ge. 2) then
                     tmphead = (adcnum(2)*512+adcnum(1)*
     +                    16+header)
                     iflg = 0
                     ihts = 0
                     call store (iflg,numhits,tmphead)
                     call store (iflg,ihts,tmpstore(1))
                     call store (iflg,ihts,tmpstore(2))
                     if (numhits .eq. 2) dbl = dbl + 1
                     if (numhits .eq. 3) trpl = trpl + 1
                     if (numhits .eq. 4) quad = quad + 1
                     if (numhits .ge. 5) mult = mult + 1
                  endif
                  if (numhits .gt. 2) then
                     do 43 i = 3, numhits
                        iflg = 0
                        ihts = 0
                        call store (iflg,ihts,adcnum(i))
                        call store(iflg,ihts,tmpstore(i))
 43                  continue
                  endif
               endif
 45         CONTINUE
            contflg = .false.
            goto 35
         
 60      IF (MOD(IREC,1000) .EQ. 0) WRITE (6,*) IREC,' records sorted.'
         IREC=IREC+1
         IF (IREC .GT. IRMAX .AND. IRMAX .NE. 0) GO TO 800
         GO TO 20
 800     IREC = IREC - 1
         write (6,*) IREC,' RECORDS SORTED'
         iflg = 1
         ihts = 0
         bufr = 0
         call store (iflg,ihts,bufr)
         iflg = -1
         call store (iflg,ihts,bufr)
         krunnum = krunnum + 1
         close(3)
 400  continue
      write(6,*) 'Total # of hits:  ', hitcnt
      do 61 i = 1, nadc
         write(6,700) i, ADC(i)
 700     format('Hits in ADC#',I2,1x,':',2x,I10)
 61   continue
      write(6,701) sngl,dbl,trpl,quad,mult
 701  format('singles:', I10, 3x, 'doubles:', I10, 3x,
     +     'triples:', I10, 3x, 'quadruples:', I10, 3x,
     +     '# more than 4:',I10)
c      call closetape(idev)
      if (knterr .eq. 0) return
      write (6,*) ' ! ! W A R N I N G ! ! ',knterr,
     +     '  tape read error(s)'
      RETURN
 990  write (6,*) ' OPEN ERROR ON ', FILE
      return
 999  write (6, 9030) ios, file1
 9030 format(' OPEN ERROR',I8,'  ON FILE ',A17)
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
      END









