
	Subroutine PLOTCHI 

c	Subroutine PLOTCHI to plot data generated by the Angular 
c	Distribution Program AD.

	include 'ad.inc'

	dimension xx(181), yy(181), er(181), kin(6)
	dimension imin(6), chisqmin(10), ibuf(20), chi(10,181)
	character buffer*5, FILE*20
        integer*4 kpn
        integer*2 idim,ltype,ptype,stype

	include 'keys.inc'

	data chisqmin /100.,100.,100.,100.,100.,100.,100.,100.,100.,100./

	fac = .017453293

c----------------------------------------------------------------------

	  xmin = -90				! initialization.
	  xmax = 90
	  ip = 181				! number of points.
	  lin = 1				! log plot.

c----------------------------------------------------------------------

	  if ( lmax .eq. 2 ) yconf = 10.827	! 0.1% confidence limit
	  if ( lmax .eq. 3 ) yconf = 6.908	!      for chisqr fit.
	  if ( lmax .eq. 4 ) yconf = 5.423	! { from Bevington.} 
	  if ( lmax .eq. 5 ) yconf = 4.616	! degrees of freedom =
	  if ( lmax .eq. 6 ) yconf = 4.103	! (# of data points - 1)
          if ( lmax .eq. 7 ) yconf = 3.743
          if ( lmax .eq. 8 ) yconf = 3.475      ! 9 values covering up
          if ( lmax .eq. 9 ) yconf = 3.266      ! to 10 angles
          if ( lmax .eq. 10 ) yconf = 3.097

c----------------------------------------------------------------------

c
c		Calculate Chisq as a function of Delta.
c
	np=lmax-1
	do k = 1, num
        chisqmin(k) = 100
	do 120 i=1,ip
	  iang = i - 91
	  theta = float(iang)
	  rad=fac*theta
	  del=tan(rad)
	  delsq=del**2
          den=1+delsq
	  r2=rk0(k,2)+2*rk1(k,2)*del+rk2(k,2)*delsq
	  a2=bk1(k,2)*r2*qd2/den
	  r4=rk0(k,4)+2*rk1(k,4)*del+rk2(k,4)*delsq
	  a4=bk1(k,4)*r4*qd4/den
c
c		normalize theoretical yields
c
c	  call legnorm (thet, yexp, a2, a4, yth, ynorm)
c       1-7-2000:  I have changed this call to conform with my new legnorm
c       The other parameters are in the 'ad.inc' common blocks.
	  call legnorm (a2, a4, yth, ynorm)

	  chisq=0
	  do 121 l=1,lmax
		dif = yth(l) - yexp(l)
		anum=dif**2
		eew=yerr(l)
		ersq=eew**2
		deno=np*ersq
		chi1=anum/deno
		chisq=chisq+chi1
 121	  continue

	  chi(k,i) = chisq
	  atwo(k,i) = a2
	  afour(k,i) = a4
	  if (abs(iang) .lt. 60) then
	    if (chisq .lt. chisqmin(k)) then
	  	chisqmin(k) = chisq
	    	imin(k) = i
		deltamin(k) = theta
	    end if
	  end if
 120	continue
	end do

c-------------------------------------------------------------------------------

	  ymax = -1e+10
	  ymin = 1e+10
	  do 11 k=1,num
	    do 11 i=1,ip
	      if(chi(k,i).gt.ymax) ymax = chi(k,i)
	      if(chi(k,i).lt.ymin) ymin = chi(k,i)
	      if ( ymin .gt. yconf ) ymin = yconf - 1.! Display conf.limit
	      if ( ymax .lt. yconf ) ymax = yconf + 1.! Display conf.limit
 11	  continue
	  if ( ymin .ge. 1. ) then
	    ymin = 10 ** ( int (log10(ymin)) )		! for log plot.
	  else if ( ymin .lt. 1 .and. ymin .gt. 0 ) then
	    ymin = 10 ** ( int (log10(ymin)) - 1.)	! for log plot.
	  end if
	  if ( ymax .ge. 1. ) then
	    ymax = 10 ** ( int (log10(ymax)) + 1 )	! for log plot.
	  else
	    ymax = 10 ** ( int (log10(ymax)) )		! for log plot.
	  end if

c-------------------------------------------------------------------------------

	mode = 10			! for semi-logarithmic plot.
	nsxtic = 0
	nsytic = -1
	double = 0.
	jpass = 1
 300	call frame (mode, xmin, xmax, nsxtic, ymin, ymax, nsytic,
     +  double, jpass)	

c-------------------------------------------------------------------------------
C        OPEN(2, FILE= FILE, FOR= 'FORMATTED', STATUS= 'UNKNOWN')

	do j = 1,2			! plot confidence limit.
	  yy(j) = yconf
	  er(j) = 0.
	end do
	  itwo = 2
	  kind = 2
	  xx(1) = -90.
	  xx(2) = 90.
	call line (xx, yy, er, itwo, xmin, xmax, YMIN, YMAX, kind, lin)

	do 30 k=1,num			! plot chisq.
	do 28 i=1,ip
	  xx(i) = -90. + (i-1.)
	  yy(i) = chi(k,i)
	  er(i) = 0.
 28	continue
	kind = k - 1
	call line (xx, yy, er, ip, xmin, xmax, YMIN, YMAX, kind, lin)
 30	continue

c-------------------------------------------------------------------------------

	call movabs ( 0, 0 )
 50	call scursr ( ic, ix, iy )
	call movabs ( ix, 0 )

	if ( ic .ge. 97 ) ic = ic - 32		! Uppercase = Lowercase.
	if ( ic .eq. pkey ) then
	  call hdcopy
	else if ( ic .eq. 32 ) then
	call movabs (0, 750)
	call anmode
	write ( 6, 1110 ) ( nint( deltamin(k)), k = 1, num)
 1110	format (' min.Delta:  ', 9(i3, 5x))
	call movabs (0, 740)
	call anmode
	write ( 6, 1120) (atwo(k,imin(k)), k = 1, num)
 1120	format ('   A2:     ', 9(f6.3, 2x))
	call movabs (0, 720)
	call anmode
	write ( 6, 1130) (afour(k,imin(k)), k = 1, num)
 1130	format ('   A4:     ', 9(f6.3, 2x))
	  length = 12				! How many characters total?
	  buffer = '-->'
	  do 113 i = 1, 3			! convert to integer
 113	  ibuf(4+i) = ichar(buffer(i:i))
	do k=1,num
	  call movabs (125, 620-20*(k-1))
	  call dshrel (50, 0, k-1)
	  call movabs (185, 608-20*(k-1))
	  write (buffer,'(f4.1)') aj1(k)
	  do 114 i = 1, 4			! convert to integer
 114	  ibuf(i) = ichar(buffer(i:i))
	  write (buffer,'(f4.1)') aj2(k)
	  do 115 i = 1, 4			! convert to integer
 115	  ibuf(i+7) = ichar(buffer(i:i))
	  call anstr ( length, ibuf(1) )
	end do
	else if ( ic .eq. hkey ) then
	  call anmode
	  if (jterm .eq. 0) call vtmode
	  if (jterm .eq. 1) call onlyal
	  write (6,488)
 488	  format ('     Menu					',/,
     +	   	   '   ------					',/,
     +	' d: get delta						',/,
     +  ' f: file delta, a2, a4                                 ',/,
     +	' h: help						',/,
     +  ' l: write output file for lipha                        ',/,
     +	' n: new screen						',/,
     +	' p: print hardcopy					',/,
     +	' s: stop						',/,
     +	' t: type to output file				',/,
     +	' x: exit						',/,
     +	' To continue type any number				',/)
 
	  read(5,*) inum
	  goto 300
	else if ( ic .eq. dkey ) then
	  ksy = 750
	  do k = 1,num
	  if ( ix .lt. 100 .or. ix .gt. 1000 ) goto 50
	  xi = (xmax - xmin)/900 * (ix - 100) + xmin
	  jx = nint(xi)
	  Deltamin(k) = xi
	  i = nint ( xi ) + 91
	  yi = chi(k,i)			! Only if idel = 1.
	call movabs (400, ksy)
	call anmode
          write ( 6, 1210 ) jx, yi,atwo(k,i),afour(k,i)
 1210	format (' ', i3, e9.2,2f7.3)
	ksy = ksy - 25
c	call movabs (800, 740)
c	call anmode
c	write ( 6, 1220) atwo(k,i)
 1220	format (' ', f6.3)
c	call movabs (800, 720)
c	call anmode
c	write ( 6, 1230) afour(k,i)
 1230	format (' ', f6.3)
c	  call movabs (0, 750)
c	  call anmode
c 510       format (' k = ',i,' Delta = ',f5.1,' Chisqr = ',f6.1)
	  call movabs ( ix, 60 )
	  call dshabs ( ix, 660, 1 )
	  call anmode
	  end do
	else if ( ic .eq. nkey ) then
	  goto 300
	else if ( ic .eq. skey ) then
	  if (jterm .eq. 0) call vtmode
	  if (jterm .eq. 1) call onlyal
	  return				! return to program AD.
	else if ( ic .eq. tkey ) then
	  open (4, file = 'ad.dat', status='unknown', form='formatted')
	  do k=1,num
	    WRITE (4,1000) AJ1,AJ2,SIGMA
 1000	    FORMAT (' J1=',F4.1,2X,'J2=',F4.1,2X,'SIGMA=',F10.5)
	    WRITE (4,1001)
 1001	    FORMAT (' THETA',10X,'INTENSITY',10X,'ERROR')
	    WRITE (4,1002)
 1002	    FORMAT (' =====',10X,'=========',10X,'=====')
            WRITE (4,1003) (THET(L),YEXP(L),YERR(L),L=1,LMAX)
 1003	    FORMAT(F6.1,9X,F10.4,5X,F10.4)
            WRITE (4,1004) IDEL
 1004	    FORMAT(' STEP SIZE IN ARCTAN(DELTA)=',I3,' DEGREES')
	    WRITE (4,1005)
 1005	    FORMAT( ' ARCTAN(DELTA)',10X,'CHI SQUARE',16X,'A2',18X,'A4')
	    WRITE (4,1006)
 1006	    FORMAT(' ',13('='), 6X, 13('='), 13X, 7('='), 14X, 7('=') )
	    do i = 1, ip
		delta = -90. + (i-1.)
		write (4,1007) delta, chi(k,i)   !, atwo(k,i), afour(k,i)
	    end do
 1007	    format (f5.1, 16x, f12.5)    !, 2(10x, f10.5))    !carl
	    write (4,1008)
 1008	    format ('1')
	  end do
	  close (4)
	else if ( ic .eq. xkey ) then
	  if (jterm .eq. 0) call vtmode
	  if (jterm .eq. 1) call onlyal
	  return				! return to program AD.
	else if ( ic .eq. 49 ) then
	  if (jterm .eq. 0) call vtmode
	  if (jterm .eq. 1) call onlyal
	  return				! return to program AD.
c    prepare to write file to be used by lipha
        else if ( ic .eq. lkey ) then
          open (unit = 3, status = 'new', file='fort.3')
c    write info on solid line curve
          kpn = 181
          idim = 2
          ltype = 1
          ptype = 0
          stype = 3
          write (3,601) kpn,idim,ltype,ptype,stype
601       format (1x,i4,2x,i2,2x,i2,2x,i2,2x,i2)
          do 603 i = 1,181
            write (3,602) xx(i),chi(1,i)
602         format (1x,f8.2,2x,f8.3)    
603       continue
c    write info on mid-dashed line curve (if necc.)
          if ( num .ge. 2 ) then
            ltype = 4
            write (3,604) kpn,idim,ltype,ptype,stype
604         format (1x,i4,2x,i2,2x,i2,2x,i2,2x,i2)
            do 606 i = 1,181
              write (3,605) xx(i),chi(2,i)
605           format (1x,f8.2,2x,f8.3)
606         continue
          endif
c    write info on short-dashed line curve (if necc.)
          if ( num .ge. 3 ) then
            ltype = 2
            write (3,607) kpn,idim,ltype,ptype,stype
607         format (1x,i4,2x,i2,2x,i2,2x,i2,2x,i2)
            do 609 i = 1,181
              write (3,608) xx(i),chi(3,i)
608           format (1x,f8.2,2x,f8.3)
609         continue
          endif
c    write info on long-dashed line curve (if necc.)
          if ( num .ge. 4 ) then
            ltype = 6
            write (3,610) kpn,idim,ltype,ptype,stype
610         format (1x,i4,2x,i2,2x,i2,2x,i2,2x,i2)
            do 612 i = 1,181
              write (3,611) xx(i),chi(4,i)
611           format (1x,f8.2,2x,f8.3)
612         continue
          endif
c    draw line for .1% confidence limit
         ltype = 3
         stype = 1
         write (3,613) kpn,idim,ltype,ptype,stype
613      format (1x,i4,2x,i2,2x,i2,2x,i2,2x,i2)
         do 615 i = 1,181
           write (3,614) xx(i),yconf
614        format (1x,f8.2,2x,f8.3)
615      continue
         close (3)
c    let user know when done
         write (6,*) '   Output written to file FOR004.DAT'          
c    prepare to write file of delta, a2, a4 for polarization pgm
        else if ( ic .eq. fkey ) then
          open (unit = 3, status = 'new', file='fort.4')
          do 701 i = 1,181
            iangs = i - 91
            thets = fac * float(iangs)
            write (3,702) thets, atwo(1,i), afour(1,i)
702         format (1x,3(f10.5,','))    
701       continue
        close(3)
         write (6,*) '   Output written to file FOR003.DAT'          
	else
	  goto 50
	end if

	goto 50

	end
