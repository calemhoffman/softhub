
	Subroutine legfits

c	Subroutine legfit to fit data to an even Legendre Polynomial.
c	To be called from Angular Distribution Program AD.

	include 'ad.inc'

	dimension x(100), y(100), er(100), kin(6)
	dimension angle(10), yield(10), sigmay(10), ftest(10), 
     +		  yfit(10), a(10), sigmaa(10), b(10), sigmab(10)	

	do i=1, lmax
	  angle(i) = thet(i)
	  yield(i) = yexp(i)
	  sigmay(i) = yerr(i)
	end do
	norder = 4				! fit up to P(4).
	neven = 1				! fit only even P's.
	modus = +1				! instrumental weights.
	call legfitc ( angle, yield, sigmay, lmax, norder, neven, 
     	1	modus, ftest, yfit, a, sigmaa, b, sigmab, chisqr )

!	a(1) = b(1)
	a(2) = 0.
	a(3) = b(3)
	a(4) = 0.
	a(5) = b(5)
	
	write (6,*) '  Fit Results:'
	write (6,*) ' --------------'
	write (6,700) a(1), sigmaa(1)
	write (6,710) a(3), sigmab(3)
	write (6,720) a(5), sigmab(5)
	write (6,*) ' Chisqr = ', chisqr
700	format (' A0 = ', g12.6,' sigma = ', g12.5)
710	format (' A2 = ', f6.3,' sigma = ', f6.3 ) 
720	format (' A4 = ', f6.3,' sigma = ', f6.3 ) 
*	write (6,*) ' Type any number to continue...'
*	accept *, inum
	  
	return

	end
