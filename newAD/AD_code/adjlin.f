C
C	Adjust the min and max values of a variable, so that the corresponding
C	axis starts and ends on a tic mark.
C
	SUBROUTINE ADJLIN ( XMIN, XMAX, TIC )

	implicit double precision ( a-h, o-z )

c	type *,' Tic = ', tic
c	type *,' Enter new value for tic:'
c	accept *, tic

	JFRAC = XMIN/TIC			
c	 lower limit: round down
	if ( xmin .lt. 0d0 ) then
	  if ((xmin/tic - dble(jfrac)) .gt. 1d-7 ) jfrac = jfrac - 1
	end if
	XMIN = TIC*JFRAC

	JFRAC = 0.999*XMAX/TIC			
c	 upper limit: round up
	if ( xmax .lt. 0. ) jfrac = jfrac - 1
c	  JPWR2 = 1.0001*LOG(XMAX)/LOG(2.)! 1024 etc. need special handling
c	  IF (MOD(1.0001*real(XMAX),2.**JPWR2).LT.1 ) JFRAC = JFRAC - 1
	XMAX = TIC*(JFRAC+1)
	RETURN
	END
