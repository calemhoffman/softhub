
	subroutine frame (mode, xmi, xma, nsxtic, ymi, yma, nsytic,
     + double, jpass)

c	Draws a coordinate frame on a Visual 550 or IBM-PC terminal,
c	using Plot10 subroutines.
c
c	Parameters:
c		mode = 0	
c	 linear frame.
c		       1	
c	 linear frame with left & right margin.
c		       10	
c	 logarithmic y-axis, linear x-axis.
c				
c	 ( Note: need neg. nsytic 
c	
c	!!!!!!! )
c		       11	
c	 log frame with left & right margin.
c				  ( this last option is not yet implemented!)
c
c		nsxtic:	Number of small tic marks between two big tic marks
c			on x-axis.
c		nsytic:	Number of small tic marks between two big tic marks
c			on y-axis.
c
c	Subroutines:
c		linaxis: Draws a linear axis with tic marks.
c		logaxis: Draws a logarithmic axis with tic marks.
c		horlabel: Puts labels on horizontal axis.
c		verlabel: Puts labels on vertical axis.
c		loglabel: Puts labels on vertical logarithmic axis.
c
c	Plot10 subroutines:
c		erase: To erase graphics screen.
c		movabs: To move to lower left corner of frame.
c
c					written by: Uwe Huettmeier, Oct-1986

	implicit double precision ( a-h, o-z )
	real xmi, xma, ymi, yma, double

        common /printer/ hd_copy

	xmin = dble( xmi )
	xmax = dble( xma )
	ymin = dble( ymi )
	ymax = dble( yma )

	if ( mode .eq. 1 ) then
	  ml = 50		
c	 left margin.
	  mr = 20		
c	 right margin.
	  mb = 15		
c	 bottom margin.
	  mt = 15		
c	 top margin.
	else
	  ml = 0		
c	 left margin.
	  mr = 0		
c	 right margin.
	  mb = 0		
c	 bottom margin.
	  mt = 0		
c	 top margin.
	end if

	ix1 = 100 - ml		
c	 absolute screen coordinates of frame.
	ix2 = 1000 + mr
	if (double .eq. 0) then
	  iy1 = 60 - mb
	  iy2 = 660 + mt
	else if (double .eq. 1 .and. jpass .eq. 1) then
	  iy1 = 30 - mb
	  iy2 = 380
	else if (double .eq. 1 .and. jpass .eq. 2) then
	  iy1 = 380
	  iy2 = 730 + mt
	end if

	tic = 5.d0		
c	 length of small ticmarks in abs. coord.

c------	Determine distance between big and small ticmarks in rel. coord.--------

	dx = abs ( xmin - xmax )
	dx = dx - dx*1d-7
	kx = int( log10( dx ))
	if (dx .lt. 1) kx = kx - 1

	xbtic = 10d0**( dble( kx )) 
	if (nsxtic .le. 2) then
	  if ( dx/xbtic .le. 2d0 ) xbtic = xbtic/10.
	end if
	xstic = xbtic / (nsxtic+1)
	call adjlin (xmin, xmax, xstic)

	if ( nsytic .ge. 0 ) then		
c	 omit the following stuff
						
c	      if y-axis is log.
	dy = abs ( ymin - ymax )
	dy = dy - dy*1d-7
	ky = int( log10( dy ))
	if (dy .lt. 1) ky = ky - 1

	ybtic = 10d0**( dble( ky )) 
	if (nsytic .le. 2) then
	  if ( dy/ybtic .le. 2d0 ) ybtic = ybtic/10.
	end if
	ystic = ybtic / (nsytic+1)
	call adjlin (ymin, ymax, ystic )
	end if

	xmi = real( xmin )
	xma = real( xmax )
	ymi = real( ymin )
	yma = real( ymax )

c------	Visual 550 terminal setup ---------------------------------------------

	irate = 0
	iterm = 1
	ibf = 3
	if (jpass .ne. 2) then
	  call initt( irate )
	  call term ( iterm , 1024 )
	  call setbuf ( ibf )

c-----	Start graphics --------------------------------------------------------
	
	call newpag
	end if

	call anmode
	call twindo ( ix1, ix2, iy1, iy2 )	
c	 Define screen display pos.
	call dwindo ( xmi , xma, ymi , yma )
	if (jpass .ne. 2) call erase				
c	 Erase the old screen
	call movabs ( ix1, iy1 )         	
c	 Start new one in lower-left.

	tic = -5
	call linaxis (xmin,xmax,xstic,xbtic,ix1,iy1,ix2,iy1,ml,mr,tic)  
	if (jpass .ne. 2) then
	call horlabel (ix1, iy1, ix2, iy1, xbtic, xmin, xmax, ml, mr) 
	end if
	tic = 5
	call linaxis (xmin,xmax,xstic,xbtic,ix1,iy2,ix2,iy2,ml,mr,tic)
	if (mode .lt. 10 ) then
	 call linaxis (ymin,ymax,ystic,ybtic,ix1,iy1,ix1,iy2,mb,mt,tic)
	 call verlabel (ix1, iy1, ix1, iy2, ybtic, ymin, ymax, mb,mt)
	 tic = -5 
	 call linaxis (ymin,ymax,ystic,ybtic,ix2,iy1,ix2,iy2,mb,mt,tic)
	else if (mode .ge. 10 ) then
	  call logaxis ( ymin, ymax, -tic, ix2, iy1, ix2, iy2)
	  call logaxis ( ymin, ymax, tic, ix1, iy1, ix1, iy2)
	  call loglabel (ix1, iy1, ix1, iy2, ymin, ymax) 
	end if 

	call anmode
	call baksp
	call home
c	 Define screen display pos.
	if (double .eq. 0) then
	  call twindo (100,1000,60,660)
	else if (double .eq. 1 .and. jpass .eq. 1) then
	  call twindo (100,1000,30,380)
	else if (double .eq. 1 .and. jpass .eq. 2) then
	  call twindo (100,1000,380,730)      
	endif
	return
	end

