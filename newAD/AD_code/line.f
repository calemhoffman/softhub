	subroutine line (x,y,er, numb, xmin, xmax, ymin, ymax, kind, lin)
C
C
C	subroutine LINE to plot the points
C	IX,IY=screen coordinates of the begining of the axis.
C	LX,LY=length of x and y axises in screen units.
C	X,Y=one dimensional arrays containing the x and y values.
C	ER=one dimensional array containing the errors.
C	numb=number of data points
C	XMIN,XMAX=minimum and maximun x-value
C	YMIN,YMAX=minimum and maximum y-value
C	KIND<0 for point to point plot with error bars
C	    =0 solid line plot
C	    =1 dotted line plot
C	    =2 dot - dash line
C	    =3 dashed line
C
C
C
	CHARACTER*1 STAR
	dimension istar(1), icx(1)

	DIMENSION X(181),Y(181),ER(181),ER1(181),ER2(181) 
	DATA STAR /'*'/, istar /42/, icx /120/	! 42 = ascii(*), 120=ascii(x)

	include 'ad.inc'

	ixmin = 100
	ixmax = 1000
	iymin = 60
	iymax = 660
	call twindo (ixmin, ixmax, iymin, iymax)

	if ( ymin .ge. ymax ) goto 220

	IF(LIN.EQ.0)THEN
	  CALL DWINDO(XMIN,XMAX,YMIN,YMAX)
	ELSE
	  if ( ymin .le. 1e-11 ) goto 200	! works from 1e-10
	  if ( ymax .ge. 1e+11 ) goto 200	! 	  to 1e+10  !!!!
	  ymin = ymin + 1e-5 * ymin		! works down to ymin = 1e-10 !
	  ymax = ymax - 1e-5 * ymax		! works up to ymax = 1e10    !
	  almax=alog10(ymax)
	  almin=alog10(ymin)
	  if(almin.lt.0.)imi=int(almin)-1
	  if(almin.ge.0.)imi=int(almin)
	  if(almax.ge.0.)ima=int(almax)+1
	  if(almax.lt.0.)ima=int(almax)
	  yimi=imi
	  yima=ima
c	call movabs(750,0)
c	call anmode
c	write (5,*) ymin,ymax, yimi, yima
c	accept *, inum
	  call dwindo(xmin,xmax,yimi,yima)
*	  call dwindo(xmin,xmax,ymin,ymax)
	END IF
*	CALL MOVABS (ixmin, iymin)
	DO 100 I=1,numb
	  IF(LIN.EQ.0)THEN
	    YY=Y(I)
	    ER1(I)=Y(I)+ER(I)
	    ER2(I)=Y(I)-ER(I)
	    IF(ER1(I).GT.YMAX)ER1(I)=YMAX
	    IF(ER2(I).LT.YMIN)ER2(I)=YMIN
	  ELSE
   	    ER1(I) = LOG10 ( max (abs(Y(I)+ER(I)), 1e-10) )
	    IF((Y(I)-ER(I)).LT.10.**(-8))THEN
	      ER2(I)=YIMI
	    ELSE
	      ER2(I) = LOG10 ( max( abs(Y(I)-ER(I)), 1e-10 ) )
	    END IF
	    YY = LOG10 (max ( abs(Y(I)), 1e-10 ) )
	    IF(ER1(I).GT.YIMA)ER1(I)=YIMA
	    IF(ER2(I).LT.YIMI)ER2(I)=YIMI
	  END IF
	IF(KIND.GE.0)GO TO 10

c  POINT TO POINT PLOT

	CALL MOVEA(X(I),YY)
*	if (jterm .eq. 0) call movrel(-2,7)		! terminal screen (*)
*	if (jterm .eq. 0) call movrel(-1,7)		! screen dump (*)
*	if (jterm .eq. 0) call movrel(-5,-5)		! terminal screen (x)
	if (jterm .eq. 0) call movrel(-5,-2)		! screen dump (x)
	if (jterm .eq. 1) call movrel(-5,-11)
	call anstr ( 1, icx(1) )
*	call anstr ( 1, istar(1) )
*	write (5,101) star
*101	FORMAT(1H+,$,A1)
	CALL MOVEA(X(I),YY)
c	if (x(i) .eq. 30) call movrel(1,0)
c	if (x(i) .eq. 60) call movrel(1,0)
	CALL DRAWA(X(I),ER1(I))
c	ix = int (3/4*(er1(i) - yy))
	CALL DRWREL(-5,0)
	CALL DRWREL(10,0)
	CALL MOVEA(X(I),YY)
c	if (x(i) .eq. 30) call movrel(1,0)
c	if (x(i) .eq. 60) call movrel(1,0)
	CALL DRAWA(X(I),ER2(I))
	CALL DRWREL(-5,0)
	CALL DRWREL(10,0)
	CALL MOVEA(X(I),YY)
	GO TO 100
10	KIND=IABS(KIND)
c	call movabs (750,0)
c	call anmode
c	type *,' yy = ', yy
c	accept *, inum
	IF(KIND .ge. 0)THEN
	  IF(I.EQ.1)CALL MOVEA(X(I),YY)
	  CALL DASHA(X(I),YY,KIND)
	END IF
100	CONTINUE
	goto 300

200	call home
	call anmode
	write (5,*) ' ymin or ymax beyond lower limit ( 1e-/+10 ) !! '
	goto 300
220	call anmode
	write (5,*) ' ymin .lt. ymax !! '

300	RETURN
	END

