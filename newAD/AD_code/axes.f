C
C	Subroutine to plot a logarithmic axis.  PLOT10 subroutines are used,
C	and it is assumed that the graphics have been initialized by the
C	time that this subroutine is called.
C
C					Jim Hamill
C					January, 1985
C					Tallahassee, Florida
C
c	Debugged and modified,
c					Uwe Huettmeier
c					June, 1986
c					Tallahassee, Fla
c
	SUBROUTINE LOGAXIS ( S1, S2,	
c	 the two ends of the axis
     &		TICLENGTH,		
c	 ABSOLUTE size of a tic
     &		JX1ABS, JY1ABS,		
c	 ABSOLUTE starting and ...
     &		JX2ABS, JY2ABS )	
c	  ... finishing coordinates

	implicit double precision (a-h, o-z)
	logical flag9

	kfix(xx) = ifix(1.00001*real(xx)+20.) - 20

C	(Numbers along the axis are of the form jx10**p.  First j1, p1,
C	and j2, p2 must be computed.  The user had better have specified
C	these such that j1 and j2 are whole numbers, or pretty nearly,
C	since otherwise the axis labeling may not correspond to the values
C	subsequently plotted.)

	if ( abs(s1) .lt. 1e-10 ) then
	  jp1 = 0
	  j1 = 1
	  s1a = 0
	  goto 20
	end if

	jp1 = ifix(log10(1.00001e10*ABS(real(s1)))) - 10
	j1 = kfix(ABS(s1)/10.**jp1)
	s1a = jp1 + log10(float(j1))	
c	 common log of the starting value
20	jp2 = log10(1.00001e10*ABS(s2)) - 10
	j2 = kfix(ABS(s2)/10.**jp2)
	s2a = jp2 + log10(float(j2))

C	The orientation of the axis and of tic marks.

c	JXDELTA = (JX2ABS - mr) - (JX1ABS + ml)
c	JYDELTA = (JY2ABS - mt) - (JY1ABS + mb)

	jxdelta = jx2abs - jx1abs
	jydelta = jy2abs - jy1abs
	z = float(jxdelta)**2 + float(jydelta)**2
	z = sqrt(z)
	sine = jydelta/z
	cosine = jxdelta/z

	jdxtic = ticlength*sine		
c	! relative x and y tic displacements
	jdytic = -ticlength*cosine

	jx = jx1abs
	jy = jy1abs
	call movabs ( jx, jy )		
c	 pen at lower end of the axis
	jscale = 1
	s = jp1 + log10(float(j1))	
c	 common log of j1x10**p1
	if ( j1.eq.1 .or. j1.eq.10 ) jscale = 3
	if ( j1.eq.2 .or. j1.eq.5 ) jscale = 2
	call drwrel ( jscale*jdxtic, jscale*jdytic )	
c	 tic
c	
	call drwabs ( jx, jy )				
c	 back to the axis

C	Loop through the remaining possible values jx10**p.

	flag9 = j1.eq.9
	if ( .not.flag9 ) jp1a = jp1
	if ( flag9 ) jp1a = jp1 + 1

	do 50 jp = jp1a, jp2
	if ( jp.eq.jp1a ) ja = j1 + 1
	if ( jp.ne.jp1a .or. flag9 ) ja = 1
	if ( jp.eq.jp2 ) jb = j2
	if ( jp.ne.jp2 ) jb = 9
	flag9 = .false.
	do 50 j = ja, jb
	s = jp + log10(float(j))	
c	 common log of jx10**p
	jx = jx1abs + (s-s1a)/(s2a-s1a)*jxdelta
	jy = jy1abs + (s-s1a)/(s2a-s1a)*jydelta
	call drwabs ( jx, jy )		
c	 move the pen one notch up the axis
	jscale = 1
	if ( j.eq.1 .or. j.eq.10 ) jscale = 3
	if ( j.eq.2 .or. j.eq.5 ) jscale = 2
	call drwrel ( jscale*jdxtic, jscale*jdytic )	
c	 draw the tic mark
 50	call drwabs ( jx, jy )				
c	 back to the axis

	return
	end

********************************************************************************
********************************************************************************

C	Subroutine to plot a linear axis.  PLOT10 subroutines are used,
C	and it is assumed that the graphics have been initialized by the
C	time that this subroutine is called.
C
C					Uwe Huettmeier, Jim Hamill
C					May, 1985
C					Tallahassee, Florida
C
C
	SUBROUTINE LINAXIS ( S1, S2,	
c	 the two ends of the axis
     &		DS, DSDBLTIC,		
c	 distance between small, big tics
     &		JX1ABS, JY1ABS,		
c	 ABSOLUTE starting and ...
     &		JX2ABS, JY2ABS,		
c	  ... finishing coordinates
     &		m1, m2, ticlength )		
c	 Margin left/right or bottom/top  and ticlength
					
c	   in absolute coordinates!
	implicit double precision (a-h, o-z)

	xmod(xx,yy) = abs(xx) - abs(yy)*int(abs(xx/yy))


c------	Find out whether m1,m2 is left/right or bottom/top margin -------------

	if ( jy1abs .eq. jy2abs ) then		
c	 x-axis.
	  ml = m1			
	  mr = m2
	  mb = 0
	  mt = 0
	else if ( jx1abs .eq. jx2abs ) then	
c	 y-axis.
	  ml = 0
	  mr = 0
	  mb = m1
	  mt = m2
	end if 

	stotal = abs (s2 - s1)			
c	 Total length of axis.
	isign = (s2 - s1)/stotal		
c	 ds < 0: draw axes reverse.
	s0 = ds*int(s1/ds)
	si = s1
	if ( abs( s0-s1 ) .lt. 1d-7 ) then
	  if ( s1 .lt. 0. ) s0 = s0 - ds
	end if
	nsteps = int (S2/ds) - nint ((S1+.499*ds)/ds) + 1

	JXDELTA = (JX2ABS - mr) - (JX1ABS + ml)
	JYDELTA = (JY2ABS - mt) - (JY1ABS + mb)
	Z = FLOAT(JXDELTA)**2 + FLOAT(JYDELTA)**2
	Z = SQRT(Z)				
	SINE = JYDELTA/Z			
c	 angle of tics.
	COSINE = JXDELTA/Z

	JDXTIC = TICLENGTH*SINE		
c	 relative x,y tic displacements
	JDYTIC = -TICLENGTH*COSINE

	call movabs ( jx1abs, jy1abs )		
c	 pen at lower end of the axis.
	jx1 = jx1abs + ml
	jy1 = jy1abs + mb
	jx2 = jx2abs - mr
	jy2 = jy2abs - mt
	call drwabs ( jx1, jy1 )		
c	 draw left/bottom margin.

	JSCALE = 1
	S = Si
	if (( si-s0 ) .lt. 1d-7 ) then
	  JX = abs(S-S0)/(S2-S1)*JXDELTA
	  JY = abs(S-S0)/(S2-S1)*JYDELTA
	  CALL DRWrel ( JX, JY )			
c	 draw to first tic.
	end if

	IF ( XMOD(1.000001*S,DSDBLTIC) .LT. 0.01*DSDBLTIC ) 
     &		JSCALE = 2
	CALL DRWREL ( JSCALE*JDXTIC, JSCALE*JDYTIC )	
c	 draw the tic mark.
	CALL DRWABS ( JX1, JY1 )			
c	 back to the axis.

	call anmode

	DO 50 I = 1, NSTEPS 
	S = S0 + I*DS*isign
	if ( ds .gt. 0 ) then
	  if ((s - 1d-7) .gt. s2 ) goto 90	
c	 this makes NSTEPS obsolete.
	else
	  if ((s + 1d-7) .lt. s2 ) goto 90	
c	 this makes NSTEPS obsolete.
	end if
	slabel = s 				
c	 actual coordinates of label.
	smove = s - si

	JX = JX1 + smove/stotal * JXDELTA
	JY = JY1 + smove/stotal * JYDELTA
	CALL DRWABS ( JX, JY )		
c	 move the pen one notch up the axis
	JSCALE = 1
	IF ( XMOD(1.000001d0*Slabel, DSDBLTIC) .LT. 0.01d0*DSDBLTIC ) 
     & 		JSCALE = 2	
	CALL DRWREL ( JSCALE*JDXTIC, JSCALE*JDYTIC )	
c	 draw the tic mark
50	CALL DRWABS ( JX, JY )				
c	 back to the axis
90	call drwabs ( jx2abs, jy2abs )		
c	 draw right/top margin.
	RETURN
	END
