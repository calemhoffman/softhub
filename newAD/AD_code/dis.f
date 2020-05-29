
C C ** Set Plot Speed C
      SUBROUTINE SPEED
      COMMON /ZX/ MAXCHN,NUMRUN,IACC,NTOTAL,IFAST,ISCRN
	character ans
100   write (6,1000) IFAST

	input_size = 1
        call grab ( ans, input_size, input_size )
	jfast = ichar ( ans ) - 48

      IF ( JFAST .LT. 0 .OR. JFAST .GT. 4 ) THEN
        write (6,2000)
        GO TO 100
      END IF
      IFAST = JFAST
      IF ( JFAST .GT. 0 ) ISCRN = 1024/(2**(JFAST-1))
      RETURN
1000  FORMAT (/,/,' **      Present Plot Speed : ',I2,'    **'/,
     +' ------------------------------------------',/,
     +' 0 - -  Slow 		( plot all points)',/,
     +' 1 - -  Slightly Faster  (plot 1024 points)',/,
     +' 2 - -  Faster 		( plot 512 points)',/,
     +' 3 - -  Much Faster 	( plot 256 points)',/,
     +' 4 - -  Very Fast 	( plot 128 points)',/,
     +' ------------------------------------------',/,
     + ' Enter Plot Speed ( 0 - 4 ) : ')
2000  FORMAT (/,'** Invalid Value -- Must be 0 thru 4 **',/)
      END

C
C     DISNSM is used to display single spectra and add up peaks.
C
      SUBROUTINE DISNSM(IDATA)
c     Modified 1/95 by S. Tabor to add -: previous spectrum
c     Modified 4/95 by R. Kaye to add fixed width Gauss fit "u"
c     Modified 12/95 by S. Tabor to add LIPHA out option
c     Modified 6/99  by S. Tabor to redraw screen before Gauss display
      PARAMETER ( MAXSIZ=16534 )
      COMMON /SPC/NUMSPC,LABEL(50),NUMCHN(50),IPOINT(50),
     + MRKL(50),MRKR(50),ISPC
      COMMON /ZX/ MAXCHN,NUMRUN,IACC,NTOTAL,IFAST,ISCRN
      CHARACTER FMT*20, FMT2*20, CHARA, ans,LABEL*6
      CHARACTER TITLE*20, chr*80
      dimension IDATA(MAXSIZ), yback ( maxsiz )
	dimension kspc(2), omax(2), omin(2)
	logical rescale, fixwflg
       include 'keys.inc'

	data double/0/,jpass/1/,rescale/.false./	
        data mrk1st/0/,mrk2st/0/
        data a,b,c/0.,0.,0./
*
*     Inform user of available singles spectra.
*
        lgauss = 0
c    lgauss = 2 indicates redrawing after double gauss fit
      FIRST = 1
   10 IF ( ISPC .EQ. 0 ) THEN
        write (6,1000) NUMRUN
        write (6,1010) ( I , LABEL(I) , NUMCHN(I), I = 1 , NUMSPC )
      ELSE
        GO TO 20                         
c Display spectrum.
      END IF
 1000 FORMAT (/,' *** RUN ',I5,' ***',/,
     +'  SPECTRUM  LABEL   SIZE   SPECTRUM  LABEL   SIZE ',
     +'  SPECTRUM  LABEL   SIZE'/,
     +'  --------  -----   ----   --------  -----   ---- ',
     +'  --------  -----   ----')
 1010 FORMAT (3(4X,I3,4X,A6,2X,I5,1x))
*
*     Get spectrum to be displayed.
*

	write(6,1015) iscrn
1015	format (/,'   Plot: ',i4,' points')

      write (6,1020)
 1020 FORMAT (/,' Enter spectrum # (0 to exit):')

	input_size = 1
c	call grab ( ans, input_size, input_size )
	read (5,*) ispc
c	spc = ichar ( ans )

c      ISPC = SPC - KEY0                  
c Convert to integer
   20 IF( ISPC .EQ. 0 ) THEN
        RETURN
      ELSE IF( ISPC .LT. 1 .OR. ISPC .GT. NUMSPC ) THEN
        ISPC = 0
        GO TO 10
      END IF
	kspc(1) = ispc
*
*
*     Spectrum selected. Go on to next step (whatever that is!!)
*
	IRATE=0
	ITERM=1
	IBF=3
	CALL INITT(IRATE)
	CALL TERM (ITERM,1024)
	CALL SETBUF(IBF)
	CALL NEWPAG
	CALL ANMODE
7	continue

	ispc = kspc(jpass)
c minus 1.
      IRANGE = NUMCHN(ISPC)              
c Range of spectrum.
      ISTRT = 1                          
c Display starts with X=ISTRT
      IEND = IRANGE                      
c Display ends with X=IEND
      MRKLFT = -1                        
c Left marker location (OFF)
      MRKRGT = IEND + 1                  
c Right marker location (OFF)
	MRKL(ISPC)=0
	MRKR(ISPC)=0

	do 919 i = 1,irange
	  yback(i) = 0.
 919    continue
*
*     Must return to this location to replot spectrum for Expand,
*     and other to be indentified when I figure it out.
*
50	ispc = kspc(jpass)
	ibase = ipoint(ispc) - 1
       J1 = IBASE + ISTRT+1               
c X starting location in IDATA
      J2 = IBASE + IEND                  
c X ending location in IDATA
      MAXY = 10.0
      MINY = 10000000

*
*     Define the data window ( X based on channel, Y based on counts).
*     This definition makes it possible to draw on a data defined scale
*     while we let PLOT 10 do the conversion to screen coordinates.
*
      YMAX = MAXY*1.1
      YMIN = 0.0
      if( mrk1st .gt. 0 ) istrt = mrk1st 
      if( mrk2st .gt. 0 ) iend = mrk2st
      XMAX = IEND
      XMIN = ISTRT
	IF ( XMAX-XMIN .LT. 10. ) XMAX = XMIN + 50.

110   continue

   	J1 = IBASE + ISTRT+1               
c new X starting location in IDATA.
	J2 = IBASE + IEND                  
c new X ending location in IDATA.

      IF ( IFAST .EQ. 0 ) THEN
        IDEL = 1
      ELSE
        IWIDE = IEND - ISTRT + 1
        IDEL = IWIDE / ISCRN
        IF ( IDEL .EQ. 0 ) IDEL = 1
      ENDIF

      IF ( IDEL .GT. 1 ) THEN
        IWIDE =  IWIDE - ISCRN*IDEL
        IREM = IWIDE / IDEL 
        IREM = IWIDE - IREM*IDEL
        IEND1 = J2 - IDEL - IREM + 1
        IDEL1 = IDEL - 1
        DO 80 J = J1, IEND1, IDEL
          IEND2 = J + IDEL1
          IDBL = 0
          DO 70 JJ = J , IEND2
            IDBL = IDBL + IDATA(JJ)
70        CONTINUE
          IDBL = IDBL / IDEL
          IF ( IDBL .GT. MAXY ) MAXY = IDBL
80      CONTINUE
        IF ( IREM .NE. 0 ) THEN
          IEND1 = IEND1 + IDEL
          IDBL = 0
          DO 90 JJ = IEND1, J2
            IDBL = IDBL + IDATA(JJ)
90        CONTINUE
          IDBL = IDBL / IREM
          IF ( IDBL .GT. MAXY ) MAXY = IDBL
        ENDIF
      ELSE
        DO 100 J = J1 , J2
          IF ( IDATA(J) .GT. MAXY ) MAXY = IDATA(J)
          IF ( IDATA(J) .LT. MINY ) MINY = IDATA(J)
100     CONTINUE
      ENDIF

      YMAX = MAXY*1.1

	omax(jpass) = ymax
	omin(jpass) = ymin
120   continue
	ispc = kspc(jpass)
	ibase = ipoint(ispc) - 1
	ymax = omax(jpass)
	ymin = omin(jpass)
c---------------uwe's framing routine------------------------------------------

      mode = 0
      nsxtic = 9
      nsytic = 9
      call frame( mode, xmin, xmax, nsxtic, ymin, ymax, nsytic,
     + double,jpass)

c------------------------------------------------------------------------------

	omax(jpass) = ymax
	omin(jpass) = ymin
*
*     Starting with the point ISTRT, IDATA(IBASE+ISTRT) draw lines
*     connecting the data points. (The lines are for visibility).
*
      X = ISTRT
      Y = IDATA(IBASE+ISTRT)
	IF ( Y .LT. 1 ) Y = 1
      CALL POINTA(X,Y)

	x = istrt + .5
	call drawa ( x, y )


      IF ( IFAST .EQ. 0 ) THEN
        IDEL = 1
      ELSE
        IWIDE = IEND - ISTRT + 1
        IDEL = IWIDE / ISCRN
        IF ( IDEL .EQ. 0 ) IDEL = 1
      ENDIF
      IF ( IDEL .GT. 1 ) THEN
        IWIDE =  IWIDE - ISCRN*IDEL
        IREM = IWIDE / IDEL 
        IREM = IWIDE - IREM*IDEL
        IEND1 = IEND - IDEL - IREM 
        IDEL1 = IDEL - 1
        DO 150 I = ISTRT, IEND1, IDEL
          X = I + .5
	  ii = i + idel 
          IEND2 = ii + IDEL1
          IDBL = 0
          DO 140 J = ii, IEND2
            IDBL = IDBL + IDATA(IBASE+J)
140       CONTINUE
          Y = IDBL / IDEL
	  IF ( Y .LT. YMIN ) Y = YMIN
          CALL DRAWA( X , Y )			
c Draw a Vertical Line.
	  x = ii + .5
          CALL DRAWA( X , Y )			
c Draw a Horizontal Line.
150     CONTINUE
        IF ( IREM .NE. 0 ) THEN
          IEND1 = IEND1 + IDEL
          IDBL = 0
          DO 160 I = IEND1, IEND
            IDBL = IDBL + IDATA(IBASE+I)
160       CONTINUE
          Y = IDBL / IREM
	  IF ( Y .LT. YMIN ) Y = YMIN
        ENDIF
	x = iend
        call drawa ( x, y )
      ELSE					
c i.e., idel = 1.
        DO 170 I = ISTRT, IEND - 2
          X = I + .5
	  ii = i + idel 
          Y = IDATA(IBASE+ii)
	  IF ( Y .LT. YMIN ) Y = YMIN
          CALL DRAWA( X , Y )
          X = ii + .5
          CALL DRAWA( X , Y )
170     CONTINUE
	x = iend
	call drawa ( x, y )
      ENDIF

	loc = 690
	if (double .eq. 1) loc = 750
	CALL MOVABS (100,loc)		 
c Display Run #.
	CALL ANMODE
        call anmode
	if (jpass .eq. 1) then
	  write (6, 439 ) label(ispc),ispc, numrun 
	else if (jpass .eq. 2) then
	  write (6, 438) label(ispc),ispc
	end if
439	format (42X,a6,2x,' SPEC', i2, 2x, 'RUN', i5 )
438	format (' Top:  ',a6,2x,' SPEC',i2,11x,'Bottom: ')
      IF( MRKLFT .GE. ISTRT ) THEN           
c Draw left marker if needed.
        X = MRKLFT 
        CALL MOVEA ( X , YMIN )
        CALL DASHA ( X , YMAX , 2 )
      END IF
      IF( MRKRGT .LE. IEND ) THEN        
c Draw right marker if needed.
        X = MRKRGT 
        CALL MOVEA ( X , YMIN )
        CALL DASHA ( X , YMAX , 2 )
      END IF
*
*     Allow user to move cursor (we can't stop him once cursor has been
*     displayed).
*
200   continue

	if (double .eq. 1 .and. jpass .eq. 1) then
	  jpass = 2
	  if (rescale) go to 120
	  go to 50
	endif
	jpass = 1
	rescale = .false.

201     if (double .eq. 1) then
	  call twindo (100,1000,380,730)
	  call dwindo (xmin,xmax,omin(2),omax(2))
	endif

        if (lgauss .eq. 2) go to 790
c    Display results of Double Gauss fit if lgauss = 2


 202    CALL VCURSR( IC, X, Y )            
c Get key pressed and location.
	CALL HOME
	CALL ANMODE
	ks = 1
	if (double .eq. 1 .and. y .gt. 0) then
	  call twindo (100,1000,380,730)
	  ymin = omin(2)
	  ymax = omax(2)
	  ks = 2
	  call dwindo (xmin,xmax,ymin,ymax)
	endif

	if (double .eq. 1 .and. y .lt. 0) then
	  call twindo (100,1000,30,380)
	  ymin = omin(1)
	  ymax = omax(1)
	  ks = 1
	  call dwindo (xmin,xmax,ymin,ymax)
	  yscreen = 380 + 350 * (y - omin(2))/(omax(2) - omin(2) )
	  y = ymin + (yscreen - 30) * (ymax - ymin) / 350
	end if

	ispc=kspc(ks)
	ibase = ipoint(ispc) - 1
	irange = numchn(ispc)

*     Next we have to determine what action to take.
*
 1030 FORMAT(/,'                   DISPLAY MENU',/,
     +' -----------------------------------------------------------',/,
     +' A: add counts    ** MENU **    space: next spectrum        ',/,
     +' B: set background              b: background=counts at curs',/,
     +' C: channel info                c: channel info             ',/,
     +' D: scale spectrum down         U: scale spectrum up        ',/,
     +' E: expand spectrum             e: expand to typed limits   ',/,
     +' F: fill between markers        L: dump LIPHA data          ',/,
     +' G: single gaussian fit         g: double gaussian fit      ',/,
     +' I: linear background           number: display that spect  ',/,
     +' K: input energy calib. coef.s  shift num: keep x limits    ',/,
     +' M: set marker                  m: enter marker             ',/,
     +' N: redraw screen               O: reset display            ',/,
     +' P: project twod                Q: quit display of spectrum ',/,
     +' T: turn ON double display      t: turn OFF double display  ',/,
     +' U: scale spectrum up           u: Gauss fit with fixed FWHM',/,
     +' W: Write title into screen	                           ',/,
     +' X: exit display mode           x: exit display mode        ',/,
     +' Y: enter Ymax                  y: enter Ymin               ',/,
     +' Z: reset background to zero    -: previous spectrum        ')
1031   format(
     +' <: slide display left          >: slide display right      ',/,
     +' {: double slide left           {: double slide right       ',/,
     +' ,: half slide left             .: half slide right         ',/,
     +'           Press "H" to resume display.')
*
      IF( IC .EQ. HKEY ) THEN            
c Display menu with H key.
	CALL VTMODE
	CALL VTMODE
	write (6,1030)
	write (6,1031)
	input_size = 1
   60   call grab ( ans, input_size, input_size )
	ic = ichar ( ans )

	GO TO 50
C
      ELSE IF(IC .GE. 49 .AND. IC .LE. 57) THEN
	ISPC = IC - 48
	GO TO 20
C
      ELSE IF( IC .EQ. KKEY ) THEN       
c Input energy calib. coefficients.
	call ANMODE
	write (6,49)
49      FORMAT (' INPUT COEF''S OF LIN. OR QUADR. FIT: A,B,C (in keV)')
        read (5,*) A,B,C
      ELSE IF( IC .EQ. mmkey ) THEN      
c Enter marker with m key.
	CALL ANMODE
5385	write (6,*) 'ENTER MARKER:  '
	mrkrgt = mrklft
	READ (5,*,ERR=5385) MRKlft
	IF ( MRKLFT .LT. 0 ) MRKLFT = 0 
	IF ( MRKLFT .GT. IRANGE ) MRKLFT = IRANGE
	GOTO 5390
      ELSE IF( IC .EQ. MKEY .or. ic .eq. tildkey) THEN       
c Set marker with M key.
	mrkrgt = mrklft
        MRKLFT = nint ( x )
5390    X = max ( MRKLFT+0., 0. ) 
	IF (( X .LT. 0. ) .OR. ( X .GT. XMAX )) GOTO 200

	if (double .eq. 1)  call twindo (100,1000,30,730)

        CALL MOVEA ( X , YMIN )
        CALL DASHA ( X , YMAX , 1 )
	call movea ( x, y )
	mrk1 = min(mrklft,mrkrgt)
	mrk2 = max(mrklft,mrkrgt)
      ELSE IF( IC .EQ. eekey ) THEN	
c Input limits for spectrum expansion with e key.
5372	write (6,*) 'Type left, right limits for expansion       '
	read (5,*) istrt,iend
	if (istrt .lt. 0) go to 200
	if (iend .gt. irange) go to 5372
	go to 50
      ELSE IF( IC .EQ. EKEY .or. ic .eq. bslkey) THEN	
c Expand spectrum with E key.
        if ( mrk1 .lt. 0 ) goto 200
	if ( mrk2 .gt. irange ) goto 200
        ISTRT = min (mrklft,mrkrgt)     
c Set left edge to left marker
        IEND = max (mrklft,mrkrgt)	
c Similar for right edge.
        GO TO 50			
c Go back and redefine plot.
      ELSE IF( IC .EQ. FKEY ) THEN      
c Use F key for fill.
        IFIL1 = ISTRT
        IF ( MRKLFT .GT. 0 ) IFIL1 = MRK1
        IFIL2 = IEND
        IF ( MRKRGT .LE. IEND ) IFIL2 = MRK2
        DO 610 I = IFIL1 , IFIL2
          X = I
          Y = IDATA(IBASE+I)
          CALL MOVEA ( X , YMIN )
          CALL DASHA ( X , Y , 1 )
610     CONTINUE
      ELSE IF( IC .EQ. 123) THEN	
c SLIDE 2 WIDTHS TO LEFT
	MOVW=(IEND-ISTRT+1) * 2		
c ASCII(123) = {
	GO TO 6060
      ELSE IF( IC .EQ. 60) THEN		
c SLIDE 1 WIDTH TO LEFT
	MOVW=(IEND-ISTRT+1)		
c ASCII(60) = <
	GO TO 6060
      ELSE IF( IC .EQ. 44) THEN		
c SLIDE 1/2 WIDTH TO LEFT
	MOVW=(IEND-ISTRT+1)/2		
c ASCII(44) = ,
6060	ISTRT1=ISTRT-1
	MOVW=MIN(MOVW,ISTRT1)
	ISTRT=ISTRT-MOVW
	IEND=IEND-MOVW
	GO TO 50
      ELSE IF( IC .EQ. 125) THEN	 
c SLIDE 2 WIDTHS TO RIGHT
	MOVW=(IEND-ISTRT+1) * 2		 
c ASCII(125) = }
	goto 6262 
      ELSE IF( IC .EQ. 62) THEN		 
c SLIDE 1 WIDTH TO RIGHT
	MOVW=(IEND-ISTRT+1)		 
c ASCII(62) = >
	goto 6262 
      ELSE IF( IC .EQ. 46) THEN		 
c SLIDE 1/2 WIDTH TO RIGHT
	MOVW=(IEND-ISTRT+1)/2		 
c ASCII(46) = .
6262	IEND1=IRANGE-IEND
	MOVW=MIN(MOVW,IEND1)
	IEND=IEND+MOVW
	ISTRT=ISTRT+MOVW
	GO TO 50
      ELSE IF( IC .EQ. UKEY ) THEN       
c Scale data up with U key.
	omax(ks) = omax(ks)/2.0
	rescale = .true.
        GO TO 120
      ELSE IF( IC .EQ. DKEY ) THEN       
c Scale data down with D key.
	omax(ks) = omax(ks) * 2
	rescale = .true.
        GO TO 120
c Set flat background to counts/chan at cursor channel
	else if (ic .eq. bbkey) then
	ix = x + .5
	ycount = idata(ibase+ix)
	do 306 i = 1, irange
306	yback(i)=ycount
	call movea(xmin,ycount)
	call dasha(xmax,ycount,3)
	go to 201
      ELSE IF( IC .EQ. BKEY ) THEN       
c Set background with B key.
        if ( mrk1 .lt. 0 ) goto 200
	if ( mrk2 .gt. iend ) goto 200
        YCOUNT  = 0.0
	DO 310 I = min(mrklft,mrkrgt), max(mrklft,mrkrgt)
          YCOUNT = YCOUNT + IDATA(IBASE+I)
310	CONTINUE
	do 305 i=1,irange
          yback(i) =  YCOUNT / ( (mrk2 - mrk1) + 1 )
305	continue
        call movea ( xmin , yback(mrk1) )
        call dasha ( xmax , yback(mrk2), 3 )
	call movea ( x, y )
	MRKL(ISPC)=MIN(MRKLFT,MRKRGT)
	MRKR(ISPC)=MAX(MRKLFT,MRKRGT)
      ELSE IF( IC .EQ. IKEY ) THEN       
c Get linear background with I key.
	x1 = mrk1
	x2 = mrk2
        y1 = idata(ibase + mrk1)
        y2 = idata(ibase + mrk2)		
c mrk1 < mrk2 (always!)
	bb = (y2 - y1) / AMAX1 ( (x2 - x1), 1. )
	ba = y1 - bb * x1
	do 405 i=1,irange
	  yback(i) = ba + bb * i
405	continue
        write (6,*) "Intercept, slope = ",ba,bb
	call movea ( x1, yback(mrk1) )
        call dasha ( x2, yback(mrk2) , 3 )
c	if ( iback .eq. 1 ) then
c	  call movea ( x1, yback(mrk1) )
c          call dasha ( x2, yback(mrk2) , 3 )
c	end if
      ELSE IF( IC .EQ. AKEY .or. ic .eq. barkey) THEN       
c Add up counts with the A key.
        if ( mrk1 .lt. 1 ) go to 200
	XCENT2 = XCENTR
	ygross = 0.0
        YCOUNT = 0.0
        XCOUNT = 0.0
        X2CNT = 0.0
        DO 410 I = min(mrklft,mrkrgt), max(mrklft,mrkrgt)
          X = I
          Y = IDATA(IBASE+I) 
	  ygross = ygross + y
          Y = IDATA(IBASE+I) - YBACK(i)
          YCOUNT = YCOUNT + Y
          XCOUNT = XCOUNT + X*Y
          X2CNT = X2CNT + X*X*Y
410     CONTINUE
        XCENTR = XCOUNT / YCOUNT
        FWHM = 2.3548 * SQRT (max( X2CNT/YCOUNT - XCENTR**2, 0.))
        totbck = ygross - ycount 
	EN = XCENTR
        IF(B.NE.0.) THEN
          ECENTR=A+B*XCENTR+C*XCENTR*XCENTR
          FWHME=FWHM*B
	  EN = ECENTR
        END IF
	IYGROS = NINT ( YGROSS )
	ITOTBC = NINT ( TOTBCK )
	IYCOUN = NINT ( YCOUNT )
	CALL LENGTH ( FWHM,   L1 )
	CALL LENGTH ( YGROSS, L3 )
	CALL LENGTH ( TOTBCK, L4 )
	CALL LENGTH ( YCOUNT, L5 )
	nblank = max ( 60-4-4-6-(L1+2)-L3-L4-L5-20, 1 )
        write (6, 450) MRK1, MRK2, XCENTR, FWHM, 
     1		  	   IYGROS, ITOTBC, IYCOUN
  450   FORMAT(' (',I4,'/',I4,'): CENT=',F6.1,' FWHM=', F6.1,
     1   ' SUM=(', I9,'-', I8,')=', I9,
     2	  13X)
	IF (B.NE.0.) THEN
	  CALL MOVABS ( 1, 760 )
	  CALL LENGTH ( FWHME, L4 )
  	  write (6, 454 ) ECENTR, FWHME
	END IF
  454   FORMAT (19X, F6.1, ' keV', 2X, F6.1, ' keV')
	   call movea ( xcentr, y )
      ELSE IF( IC .EQ. CKEY .or. ic .eq. cckey) THEN       
c Use C key to get channel info
        IX = X + .5
        IY = IDATA(IBASE+IX)
         IF(B.NE.0.) THEN
           ENERG=A+B*IX+C*IX*IX
           write (6, 511) IX, IY, ENERG
511        format ('  CHANNEL: ',I4,'  COUNTS: ',I9,'  ENERGY: ',F6.1,
     +		   ' KEV                         ')
         ELSE   
           write (6, 510 ) IX , IY
510        format ('   CHANNEL: ',I4,'   COUNT: ',I9, 36X )
         END IF
	 call movea ( x, y )
C  L key to put out displayed data for LIPHA plot
         ELSE IF (IC .EQ. LKEY) THEN
            call anmode
            write (6,*) 'Type factor to multiply counts by'
            read (5,*) factor
            npts = iend - istrt
            nsets = npts/250 + 1
            ji = istrt
            do 534 j = 1, nsets
            jf = ji + 249
               if (jf .gt. iend) jf = iend
               npts = jf - ji + 1
               write (4,*) npts, ' 2 1 0 2'
               do 533 ix = ji, jf
                  iy = factor * idata(ibase+ix)
                  if (b .eq. 0) then
                     write (4,*) ix, iy
                  else
                     energ = a + b*ix + c*ix*ix
                     write (4,*) energ, iy
                  end if
 533              continue
               ji = jf + 1
 534           continue
      ELSE IF( IC .EQ. NKEY ) THEN	
c Use N key to redraw screen.
        GO TO 50
      ELSE IF( IC .EQ. OKEY ) THEN	
c O key resets to start display
        ISTRT = 1			
c Reset spectrum start location
        IEND = IRANGE			
c Reset spectrum end location.
        MRKLFT = -1			
c Put left marker offscreen
        MRKRGT = IEND + 1		
c Put right marker off screen.
	MRKL(ISPC)=0
	MRKR(ISPC)=0
	do 205 i=1,irange		
c Set background to zero.
          yback(i) =  YCOUNT / ( (mrk2 - mrk1) + 1 )
205	continue
        GO TO 50			

      ELSE IF( IC .EQ. PPKEY .or. ic .eq. pkey ) THEN      
cPROJECT FROM TWOD ARRAY
	ISTRT = MIN(MRKLFT,MRKRGT)
	IF (ISTRT .LT. 1) ISTRT = 1
	IEND = MAX(MRKLFT,MRKRGT)
	IF (IEND .GT. IRANGE) IEND = IRANGE
	CALL PROJ (IDATA,ISTRT,IEND)
	GO TO 20

      ELSE IF( IC .EQ. WKEY ) THEN	
c Use W key to write into screen.
	CALL MOVEA (X,Y)
	CALL ANMODE
	READ (5,521) CHARA
521	FORMAT (A80)
	CALL MOVEA (X,Y)
      ELSE IF ( IC .EQ. YKEY ) THEN	
c Use Y key to set Ymax.
	write (6,578)
578	format (' Enter new Ymax:                                   ')
	rescale = .true.
	READ (5,*) omax(ks)
	GOTO 120	
      ELSE IF ( IC .EQ. yykey ) THEN	 
c Use y key to set Ymin.
	write (6,579)
579	format (' Enter new Ymin:                                   ')
	READ (5,*) omin(ks)
	rescale = .true.
	GOTO 120	
      ELSE IF( IC .EQ. ZKEY ) THEN       
c Reset background to zero.
	MRKL(ISPC)=0
	MRKR(ISPC)=0
        MRKLFT = -1
        MRKRGT = IEND + 1
	do 605 i=1,irange
	  yback(i) = 0.0
605	continue
        GO TO 120
      ELSE IF( IC .EQ. SPKEY) THEN       
c Use space bar for next spectrum.
	  ISPC = ISPC + 1
	  IF ( ISPC .GT. NUMSPC ) ISPC = 1
          kspc(jpass) = ispc
	  GO TO 50
      ELSE IF( IC .EQ. minkey) THEN       
c Use backspace for previous spectrum.
	  ISPC = ISPC - 1
	  IF ( ISPC .lT. 1 ) ISPC = numspc
          kspc(jpass) = ispc
	  GO TO 50
      ELSE IF( IC .EQ. QKEY ) THEN       
c Use Q key for quit option.
	ISPC = 0
	call erase
	call vtmode
	GO TO 10
      ELSE IF ( IC .EQ. XKEY .OR. IC .EQ. -1) THEN
c	call erase
	CALL VTMODE
        call vtmode
	double = 0
        RETURN
      else if (ic .eq. xxkey) then
c	call erase
	call vtmode
        call vtmode
	return
c 
      else if (ic .eq. uukey) then
         write (6,703) 
 703     format (' Type fixed FWHM (in channels) for Gaussian fit 
     +          ')
         read (5,*) FWHM
         fixwflg = .true.
         goto 704
C  Single Gaussian fit = G
      ELSE IF ( IC .EQ. GKEY) THEN
           fixwflg = .false.
 704       IF (MRK1 .LT. 1) GOTO 200
           MRLFT=MIN(MRKLFT,MRKRGT)
           MRRGT=MAX(MRKLFT,MRKRGT)
           IF (MRRGT .EQ. MRLFT) GOTO 200
           YCOUNT=0.0
           XCOUNT=0.0
           X2CNT=0.0
           DO 700 I=MRLFT,MRRGT
               X=I
               Y=IDATA(IBASE+I)-YBACK(I)
               YCOUNT=YCOUNT+Y
               XCOUNT=XCOUNT+X*Y
               X2CNT=X2CNT+X*X*Y
700        CONTINUE
           if (ycount .eq. 0) go to 200
           XCENTR=XCOUNT/YCOUNT
           if (.not. fixwflg)
     +     FWHM=2.3548*SQRT(MAX(X2CNT/YCOUNT-XCENTR**2,0.))
           if (fwhm .le. 0) fwhm = 1.1774*(mrrgt-mrlft+1)
           CALL GAUSS(XCENTR,W,AA,FWHM,IBASE,MRLFT,MRRGT, CHISQ,
     1 IDATA,YBACK,fixwflg)
           if (chisq .eq. -1) then
               write(6,701)
               goto 200
           end if
           AREA=AA*W*1.7725
           FWHM=1.665*W
           FAC=MRRGT-MRLFT-2
           IF (FAC .GT. 0) THEN
                CHISQ=CHISQ/FAC
           END IF
           write (6,*) ' '
           WRITE(6,750)MRK1,MRK2,XCENTR,FWHM,AREA, CHISQ
750        FORMAT(' (',I4,'/',I4,'): CENT=',F6.1,' FWHM=',F6.1,
     1 ' AREA=',F9.1,' CHISQ=',F6.1,11X)
           IF (B .NE. 0) THEN
               ECENTR=A+B*XCENTR+C*XCENTR**2
               FWHME=B*FWHM
               CALL MOVABS(1,760)
               WRITE(6,775)ECENTR,FWHME
           END IF
775        FORMAT(19X,F6.1,' keV',F6.1,' keV')
           XMIN1=XCENTR-2.628*W
           XMIN1=AMAX1(XMIN1,FLOAT(ISTRT))
           XMAX1=XCENTR+2.628*W
           XMAX1=AMIN1(XMAX1,FLOAT(IEND))
           DELX=(IEND-ISTRT)/500.
           DELX=AMIN1(DELX,1.)
           LAST=(XMAX1-XMIN1)/DELX+1
           X=XMIN1
           J=X
           Y=YBACK(J)
           CALL MOVEA(X,Y)
           DO 800 I=1,LAST
               X=XMIN1+DELX*FLOAT(I-1)
               J=X
               Y=AA*EXP(-((X-XCENTR)/W)**2)+YBACK(J)
               CALL DRAWA(X,Y)
800        CONTINUE
C Double Gaussian fit = g
      ELSE IF (IC .EQ. GGKEY) THEN
           IF (MRK1 .LT. 1) GOTO 200
           MRLFT=MIN(MRKLFT,MRKRGT)
           MRRGT=MAX(MRKLFT,MRKRGT)
           IF (MRRGT .EQ. MRLFT) GOTO 200
           CALL ERASER
           WRITE(6,850)
850        FORMAT('  PLACE MARKER OVER EXPECTED POSITION FOR CENTROID',
     1 ' 1 AND PRESS ANY KEY')
           CALL VCURSR(IC,X,Y)
           XCENTR1=X
           CALL ERASER
           WRITE(6,900)
900        FORMAT('  PLACE MARKER OVER EXPECTED POSITION FOR CENTROID',
     1' 2 AND PRESS ANY KEY')
           CALL VCURSR(IC,X,Y)
           IF ( X .EQ. XCENTR1) THEN
                 CALL ERASER
                 WRITE (6,902)
                 CALL VCURSR(IC,X,Y)
           END IF
902        FORMAT(' CENTROIDS 1 AND 2 HAVE SAME MARKER POSITION! TRY',
     1' AGAIN')
           XCENTR2=X
           CALL ERASER
           WRITE(6,905)XCENTR1
           READ(5,*)FWHM1
           IF (FWHM1 .LE. 0) THEN
               CALL ERASER
               WRITE(6,*)'Double Gauss Abort!'
               GOTO 200
           END IF
           CALL ERASER
           write(6,906)
           WRITE(6,905)XCENTR2
           READ(5,*)FWHM2
           IF (FWHM2 .LE. 0) THEN
               CALL ERASER
               WRITE(6,*)'Double Gauss Abort!'
               GOTO 200
           END IF
905        FORMAT(' Input FWHM in CHANNELS for',1X,F6.1,1X,' CENTROID',
     1 ' (Use negative to ABORT) ',$)
906        format(' NOTE: If the second FWHM is same as first',
     1 ' FWHM,',/,' then the widths will be varied together,',
     2 ' NOT INDEPENDENTLY !')
           CALL ERASER
           write (6,*)'   Fitting ...'
           CALL GAUS2(FWHM1,FWHM2,IBASE,MRLFT,MRRGT,IDATA,
     1 YBACK,XCENTR1,XCENTR2,CHISQ,A1,A2,W1,W2)
           lgauss = 2
           go to 120
c    return to redraw screen before displaying results at statement 790


3200	else if (ic .eq. tkey) then
	double = 1
	write (6,3201)
3201	format ('     Enter Bottom & Top spectrum numbers:  ')
	read (5,*) kspc(1),kspc(2)
	goto 50
3300	else if (ic .eq. ttkey) then
           write (6,3202)
 3202      format ('    Type spectrum number: ')
           read (5,*) kspc(1)
	   double = 0
	goto 50
      END IF

	lc = 0
	if (ic .eq. 33) lc = 1
	if (ic .eq. 64) lc = 2
	if (ic .eq. 35) lc = 3
	if (ic .eq. 36) lc = 4
	if (ic .eq. 37) lc = 5
	if (ic .eq. 94) lc = 6
	if (ic .eq. 38) lc = 7
	if (ic .eq. 42) lc = 8
	if (ic .eq. 40) lc = 9
	IF (LC .GT. 0 .AND. LC .LE. NUMSPC) THEN
	kspc(jpass) = LC
	  GOTO 50
	ENDIF

	CALL ANMODE
      GO TO 201       
c Get the next key.

c  Return here to display Double Gauss fit results
 790       lgauss = 0
           if (chisq .eq. -1) then
                 write (6,701)
                 goto 200
           end if
701        format('   FATAL ERROR DURING FIT!  ABORT FIT!')
           AREA1=A1*W1*1.7725
           AREA2=A2*W2*1.7725
           FWHM1=1.665*W1
           FWHM2=1.665*W2
           FAC=MRRGT-MRLFT-3
           IF (FAC .GT. 0) THEN
               CHISQ=CHISQ/FAC
           END IF
           CALL ERASER
           WRITE(6,915)MRK1,MRK2,XCENTR1,FWHM1,
     1 AREA1,CHISQ,XCENTR2,FWHM2,AREA2
915        FORMAT(' (',I4,'/',I4,'): CENT1=',F6.1,7x,
     1 ' FWHM1=',F6.1,' AREA1=',F9.1,/,' CHISQ=',F6.1,' CENT2=',F6.1,
     2 7x,' FWHM2=',F6.1,' AREA2=',F9.1)
           IF (B .NE. 0) THEN
                ECENTR1=A+B*XCENTR1+C*XCENTR1**2
                ECENTR2=A+B*XCENTR2+C*XCENTR2**2
                FWHME1=B*FWHM1
                FWHME2=B*FWHM2
                CALL MOVABS(1,75)
                WRITE(6,950)ECENTR1,FWHME1
                WRITE(6,951)ECENTR2,FWHME2
           END IF
950        FORMAT(12X,'CENTR1= ',F6.1,' keV',2X,'FWHME1= ',F6.1,
     1 ' keV') 
951        FORMAT(12X,'CENTR2= ',F6.1,' keV',2X,'FWHME2= ',F6.1,
     1 ' keV')
           XMIN1=XCENTR1-2.628*W1
           XMIN2=XCENTR2-2.628*W2
           XMAX1=XCENTR1+2.628*W1
           XMAX2=XCENTR2+2.628*W2
           XMIN1=AMIN1(XMIN1,XMIN2)
           XMIN1=AMAX1(XMIN1,FLOAT(ISTRT))
           XMAX1=AMAX1(XMAX1,XMAX2)
           XMAX1=AMIN1(XMAX1,FLOAT(IEND))
           DELX=(IEND-ISTRT)/500.
           DELX=AMIN1(DELX,1.0)
           LAST=(XMAX1-XMIN1)/DELX+1
           X=XMIN1
           J=X
           Y=YBACK(J)
           CALL MOVEA(X,Y)
           DO 3000 I=1,LAST
              X=XMIN1+DELX*FLOAT(I-1)
              J=X
              Y=A1*EXP(-((X-XCENTR1)/W1)**2)+A2*EXP(-((X-XCENTR2)/W2)
     1 **2)+YBACK(J)
              CALL DRAWA (X,Y)
3000       CONTINUE
           XMIN1=XCENTR1-2.628*W1
           XMAX1=XCENTR1+2.628*W1
           XMIN1=AMAX1(XMIN1,FLOAT(ISTRT))
           XMAX1=AMIN1(XMAX1,FLOAT(IEND))
           LAST=(XMAX1-XMIN1)/DELX+1
           X=XMIN1
           J=X
           Y=YBACK(J)
           CALL MOVEA(X,Y)
           DO 3050 I=1,LAST
               X=XMIN1+DELX*FLOAT(I-1)
               J=X
               Y=A1*EXP(-((X-XCENTR1)/W1)**2)+YBACK(J)
               CALL DRAWA (X,Y)
3050       CONTINUE
           XMIN2=XCENTR2-2.628*W2
           XMAX2=XCENTR2+2.628*W2
           XMIN2=AMAX1(XMIN2,FLOAT(ISTRT))
           XMAX2=AMIN1(XMAX2,FLOAT(IEND))
           LAST=(XMAX2-XMIN2)/DELX+1
           X=XMIN2
           J=X
           Y=YBACK(J)
           CALL MOVEA(X,Y)
           DO 3100 I=1,LAST
                X=XMIN2+DELX*FLOAT(I-1)
                J=X
                Y=A2*EXP(-((X-XCENTR2)/W2)**2)+YBACK(J)
                CALL DRAWA(X,Y)
3100       CONTINUE   
           go to 202
c   Go back and get Vcursor
c
      END
C
C
	SUBROUTINE LENGTH ( X, ILEN )
C  Calculates the length of the nearest integer part of a real number
	ILEN = INT ( ALOG10(abs(X)+.5) ) + 1 
	IF ( ILEN .LT. 1 ) ILEN = 1
	RETURN
	END
C
C ERASER ROUTINE
        SUBROUTINE ERASER
        CALL HOME
        CALL ANMODE
        WRITE(6,*)'                                                  ',
     1 '                    '
        WRITE(6,*)'                                                ',
     1 '                    '
        WRITE(6,*)'                                                ',
     1 '                    '
        CALL HOME
        CALL ANMODE
        RETURN
        END
