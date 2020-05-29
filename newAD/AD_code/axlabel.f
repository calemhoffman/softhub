*******************************************************************************
	subroutine loglabel ( jx1abs, jy1abs, jx2abs, jy2abs,
     .			s1, s2 )
*******************************************************************************
C
C	Subroutine to label a vertical LOG axis which has been drawn by another
C	program, for example by subroutine LOGAXIS.  The labels are drawn
C	to one side or the other of the axis, one for either end, and they are
C	arranged such that the numbers do not project beyond the extremities of
C	the axis.  The arguments are:
C
C	  JX1ABS,JY1ABS	screen coordinates for the lower corner
C	  JX2ABS,JY2ABS	screen coordinates for the upper corner
C	  S1		starting value of the variable plotted on the axis
C	  S2		final value of the variable plotted on the axis
C	  KARSIZ	character size as in PLOT10; plus or minus 1,2,3,4;
C			the absolute value is used, but positive/negative
C			signs signal that the labels shall be plotted,
C			respectively, left/right of the axis
C	  NSKIPRIGHT	If the format is given as F6.0, you won't want to plot
C			the decimal point.  This argument says how many of the
C			right-most characters NOT to plot out; thus for format
C			= (F10.0), you would set NSKIPRIGHT = 1.
C	  FREEROOM	I recommend 1 for a vertical plot.  This is the
C			distance between the axis and the digits of the label,
C			and might have to be somewhat bigger if the tic marks
C			would collide with the digits.
C					
C					U. Huettmeier
C					Tallahassee
C					6/26/86
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

	implicit double precision (a-h, o-z)
	character buffer*20, fmt*6	
c	 for output of the labels
	dimension ibuf(20)
	logical totheleft	
c	 TRUE means left of the axis, FALSE right

        logical hd_copy
c        common /printer/ hd_copy          !glg

c------	Define the character size.  How big is one? ---------------------------

	nskipright = 0
	karsiz = 1
	karsabs = abs(karsiz)
	totheleft = karsiz.ge.0
	call chrsiz ( karsabs )
	call csize ( isizehoriz, isizevert )

c------	Determine Fortran format of labels ------------------------------------

	fmt = '(e6.1)'
	freeroom = .2

c------	THE LEFT EDGE.  Encode s1, and find out how big its image will be -----

	if ( s1 .lt. 1e-10 ) s1 = 1		
c	 if's to prevent
	if ( s2 .lt. 1e-10 ) s1 = 1		
c	 screw ups.
	s1 = min ( s1, s2 )				
c	 just to make sure!
	s2 = max ( s1, s2 )

	sadd = max ( 0, -int ( log10 (s1)))

	nsteps = int(log10(s2)) - int(log10(s1)) + 1
	if ( nsteps .lt. 1 ) return		
c	 **** add 1 label print ****

	do 50 j=1,nsteps
	  s = 10 ** (float(int(log10(s1)))) * 10 ** (float(j-1))
	  if ( j .eq. 1) si = s
	  if ( s .le. 0. ) s = 1e-3
	  write ( buffer, fmt ) s
	  do 10 i = 1, 20		
c	 convert to integer
 10	  ibuf(i) = ichar (buffer(i:i))

	  do 20 i = 1, 20
	  i1 = i
	  if ( buffer(i:i).ne.' ' ) go to 21
 20	  continue
 21	  continue

	  do 30 i = i1+1, 20
	  i2 = i - 1
	  if ( buffer(i:i).eq.' ' ) go to 31
 30	  continue
 31	  continue

	  length = i2 - i1 + 1 - nskipright+1	
c	 How many characters total?

C	Position PLOT10 at the start, then plot the left-edge value.

	  jskipx = -(length+1.00001*freeroom)*isizehoriz 
c	 - 10. for PC.
	  if ( .not.totheleft ) jskipx = -jskipx
	  jx = jx1abs + jskipx
	  if ( jx .lt. 1) jx = 1
 	  jy = abs(jy2abs-jy1abs) * (max (log10(s)+sadd, 0.d0)
     1	  + log10(si/s1)) / log10(s2/s1) + jy1abs -10. 

	  call movabs ( jx, jy )
	  call anstr ( length, ibuf(i1) )
c          if( hd_copy )then
c              call set_ptr( jx, jy )
c              call put_string( length, ibuf(i1) )    !glg
c          endif

50	continue

200	return
	end
*******************************************************************************
	subroutine verlabel ( jx1abs, jy1abs, jx2abs, jy2abs, ybtic,
     &				s1, s2, m1, m2 )
*******************************************************************************
c	Subroutine to label a vertical axis which has been drawn by another
c	program, for example by subroutine LINAXIS.  The labels are drawn
c	to one side or the other of the axis, one for either end, and they are
c	arranged such that the numbers do not project beyond the extremities of
c	the axis.  The arguments are:
c
c	  JX1ABS,JY1ABS	screen coordinates for the lower corner
c	  JX2ABS,JY2ABS	screen coordinates for the upper corner
c	  S1		starting value of the variable plotted on the axis
c	  S2		final value of the variable plotted on the axis
c	  KARSIZ	character size as in PLOT10; plus or minus 1,2,3,4;
c			the absolute value is used, but positive/negative
c			signs signal that the labels shall be plotted,
c			respectively, left/right of the axis
c	  FMT		a CHARACTER*6 variable containing the format
c			specifier that will be used for encoding the numbers;
c			THIS WILL HAVE TO BE ENCLOSED IN PARENTHESES.
c	  NSKIPRIGHT	If the format is given as F6.0, you won't want to plot
c			the decimal point.  This argument says how many of the
c			right-most characters NOT to plot out; thus for format
c			= (F10.0), you would set NSKIPRIGHT = 1.
c	  FREEROOM	I recommend 1 for a vertical plot.  This is the
c			distance between the axis and the digits of the label,
c			and might have to be somewhat bigger if the tic marks
c			would collide with the digits.
c	  YBTIC		DISTANCE BETWEEN BIG TICMARKS
c			Jims original program was modified to plot labels
c			left to every big ticmark. JX2ABS and JY2ABS are
c			have now become obsolete.
c					
c					U. Huettmeier, J. Hamill
c					Tallahassee
c					3/14/85
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	implicit double precision (a-h, o-z)
	character buffer*20, fmt*6	
c	 for output of the labels
	dimension ibuf(20)
	logical totheleft	
c	 TRUE means left of the axis, FALSE right

        logical hd_copy
c        common /printer/ hd_copy     !glg

c------	Define the character size.  How big is one? ---------------------------

	karsabs = abs(karsiz)
	totheleft = karsiz.ge.0
	call chrsiz ( karsabs )
	call csize ( isizehoriz, isizevert )

c------	Determine Fortran format of labels ------------------------------------

	call lablfmt ( fmt, s1, s2 )

	nskipright = 0
	freeroom = 0.2
	if ( fmt(5:5) .eq. '0' ) then
	  nskipright = 1
	  freeroom = 1.
	end if	

c------	THE LEFT EDGE.  Encode s1, and find out how big its image will be -----

	s1 = min ( s1, s2 )				
c	 just to make sure!
	s2 = max ( s1, s2 )

	if ( s1 .gt. 0 ) then
	  s = ybtic * int(( s1+ybtic-ybtic*1d-7 ) / ybtic )
	else
	  s = ybtic * int( s1 / ybtic )
	end if
	si = s
	ntic = int(( s2-s1 ) / ybtic ) + 1	
c	 number of big tics.

	do 929 n = 1,ntic
	  write ( buffer, fmt ) s
	  do 10 i = 1, 20		
c	 convert to integer
 10	  ibuf(i) = ichar (buffer(i:i))

	  do 20 i = 1, 20
	  i1 = i
	  if ( buffer(i:i).ne.' ' ) go to 21
 20	  continue
 21	  continue

	  do 30 i = i1+1, 20
	  i2 = i - 1
	  if ( buffer(i:i).eq.' ' ) go to 31
 30	  continue
 31	  continue

	  length = i2 - i1 + 1 - nskipright	
c	 How many characters total?

c------	Position PLOT10 at the start, then plot the left-edge value -----------

	  jskipx = -(length+1.00001*freeroom)*isizehoriz
	  if ( .not.totheleft ) jskipx = -jskipx
	  jskipy = ((si - s1) + ((n-1)*ybtic)) 
     1		/ (s2-s1) * (jy2abs - jy1abs - m1 - m2)

	  jx = jx1abs + jskipx
	  if ( jx .lt. 1 ) jx = 1
 	  jy = jy1abs + m1 + jskipy - 10	
c	 -2 for PC Emulator.

	  call movabs ( jx, jy )
	  call anstr ( length, ibuf(i1) )
c          if( hd_copy )then
c              call set_ptr( jx, jy )
c              call put_string( length, ibuf(i1) )    !glg
c          endif

	s = s + ybtic

929	continue

200	return
	end
*******************************************************************************
	subroutine horlabel ( jx1abs, jy1abs, jx2abs, jy2abs, xbtic,
     &				s1, s2, m1, m2 )
*******************************************************************************
c	Subroutine to label a horizontal axis which has been drawn by another
c	program, for example by my subroutine LINAXIS.  The labels are drawn
c	above or below the axis, one for either end, and they are arranged
c	such that the numbers do not project beyond the extremities of the
c	axis.  The arguments are:
c
c	  JX1ABS,JY1ABS	screen coordinates for the left corner
c	  JX2ABS,JY2ABS	screen coordinates for the right corner
c	  S1		starting value of the variable plotted on the axis
c	  S2		final value of the variable plotted on the axis
c	  KARSIZ	character size as in PLOT10; plus or minus 1,2,3,4;
c			the absolute value is used, but positive/negative
c			signs signal that the labels shall be plotted,
c			respectively, below/above the axis
c	  FMT		a CHARACTER*6 variable containing the format
c			specifier that will be used for encoding the numbers;
c			THIS WILL HAVE TO BE ENCLOSED IN PARENTHESES.
c	  NSKIPRIGHT	If the format is given as F6.0, you won't want to plot
c			the decimal point.  This argument says how many of the
c			right-most characters NOT to plot out; thus for format
c			= (F10.0), you would set NSKIPRIGHT = 1.
c	  FREEROOM	I recommend 2 for a horizontal plot.  This is the
c			distance between the axis and the digits of the label,
c			and might have to be somewhat bigger if the tic marks
c			would collide with the digits.
c	  XBTIC		DISTANCE BETWEEN BIG TICMARKS
c			Jims original program was modified to plot labels
c			below to every big ticmark. JX2ABS and JY2ABS are
c			have now become obsolete.
c	  AXFACTOR	= x, gives proper labeling for x keV/Ch spectra.
c
c					U. Huettmeier, J. Hamill
c					Tallahassee
c					3/14/85
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	implicit double precision (a-h, o-z)
	character buffer*20, fmt*6	
c	 For output of the labels.
	dimension ibuf(20)
	logical below		
c	 TRUE means below the axis, FALSE above

        logical hd_copy
c        common /printer/ hd_copy        !glg

c------	Define the character size.  How big is one? ---------------------------

	karsiz = 1
	karsabs = abs(karsiz)
	below = karsiz.ge.0
	call chrsiz ( karsabs )
	call csize ( isizehoriz, isizevert )

c------	Determine Fortran format of labels ------------------------------------

	call lablfmt ( fmt, s1, s2 )

	if ( fmt(5:5) .eq. '0' ) nskipright = 1
	freeroom = 2.
	axfactor = 1.

c------	THE LEFT EDGE.  Encode s1, and find out how big its image will be -----

	s1 = min ( s1, s2 )			
c	 just to make sure!
	s2 = max ( s1, s2 )

	if ( s1 .gt. 0 ) then
	  s = xbtic * int(( s1+xbtic-xbtic*1d-7 ) / xbtic )
	else
	  s = xbtic * int( s1 / xbtic )
	end if
	si = s
	ntic = int(( s2-s1+1d-7 ) / xbtic ) + 1	
c	 number of big tics.

	do 929 n = 1,ntic
	  ss = s
	  if (axfactor .ne. 0) ss = s * axfactor
	  write ( buffer, fmt) ss
	  do 10 i = 1, 20		
c	 convert to integer
 10	  ibuf(i) = ichar(buffer(i:i))

	  do 20 i = 1, 20
	  i1 = i
	  if ( buffer(i:i).ne.' ' ) go to 21
 20	  continue
 21	  continue

 	  do 30 i = i1+1, 20
	  i2 = i - 1
	  if ( buffer(i:i).eq.' ' ) go to 31
 30	  continue
 31	  continue

	  length = i2 - i1 + 1 - nskipright	
c	 How many characters total?

c------	Position PLOT10 at the start, then plot the left-edge value -----------

	  jskipx =  ((si - s1) + ((n-1)*xbtic)) / (s2-s1) 
     1	* abs(jx2abs-m2 - (jx1abs+m1)) - length/2.*isizehoriz
	  jskipy = 1.00001*freeroom*isizevert 
	  if ( below ) jskipy = -jskipy

	  jx = jx1abs + m1 + jskipx
	  if(jx+length*isizehoriz .gt. 1020) jx = 1020-length*isizehoriz
	  if (jx .lt. 1) jx = 1
	  jy = jy1abs + jskipy - 2		
c	 + 18 for PC Emulator.

	  call movabs ( jx, jy )
	  call anstr ( length, ibuf(i1) )
c          if( hd_copy )then
c              call set_ptr( jx, jy + 10 )
c              call put_string( length, ibuf(i1) )    !glg
c          endif
  
	s = s + xbtic
	if (( s-s2 ) .gt. 1d-7) goto 200

929	continue


200	return
	end

*******************************************************************************
	subroutine lablfmt ( fmt, s1, s2 )
*******************************************************************************

	implicit double precision ( a-h, o-z )
	character fmt*6

c------	Determine Fortran format of labels of vertical axis -------------------

	k = 0
	fmt = '(    )'
	ds = abs ( s2 - s1 )
	ds = ds - ds*1d-7
	s = max ( abs (s1), abs (s2) )
	num = int( log10( s )) + 2
	num = max ( num, 1 )
	if ( min(s1,s2) .lt. 0. ) num = num + 1	
c	 format+1 if label neg. 
	if ( s .ge. 10**6 ) then		
c	 exp. format.
 	  fmt = '(e6.1)'
	else 
	  if ( ds .gt. 1 ) then			
c	 integer format.
	    k = 0
	  else if ( ds. le. 1 ) then
	    k = abs( int( log10( ds ))) + 1 
	  end if
	  fmt(1:2) = '(f'
	  fmt(3:3) = char( num + 48 + k )
	  fmt(4:4) = '.'
	  fmt(5:5) = char( k + 48 )
	  fmt(6:6) = ')'
	end if

	return
	end

