C
c	Program Legfit
c
c	purpose:
c	  make a least squares fit to data with a Legendre polynomial
c	  	Y = A(1) + A(2)*X + A(3)*(3X**2-1)/2 + ...
c		  = A(1) * (1. + B(2)*X + B(3)*(3X**2-1)/2 + ...)
c
c	subroutines:
c	  Matinv ( array, nterms, det )
c		inverts a symmetric two-dimensional matrix of degree
c		nterms and calculates its determinant.
c	  Legfitc ( theta, y, sigmay, npts, norder, neven,
c     +		mode, ftest, yfit, a, sigmaa, b, sigmab, chisqr )
c

	Program Legfit

	real*8 cosine, p, beta, alpha, chisq
	dimension theta(100), y(100), sigmay(100), ftest(100), 
     +		  yfit(100), a(10), sigmaa(10), b(10), sigmab(10)
	dimension weight(100), p(100,10), beta(10), alpha(10,10)
	character ans, check

	write (6,*) '( )'
	write (6, 1)
1	format (' This is a Legendre Polynomial Fitting routine.')

 2	write (6, *)' Do you have an input file ''Legfit.dat''? (y/n) '
	read (5,170) ans
	check = ans
	if ( ans .eq. 'y'  .or. ans .eq. 'Y' ) then
	  open ( 1, file = 'legfit.dat', status = 'old', 
     +		    form='formatted', err = 222 )
	else
5	  write (6, *)' Enter theta, yield(y) and sigmay: ( 0,0,0 to stop )'
	end if

	i = 1
10	if ( check .eq. 'y' ) then
	  read (1,*) theta(i), y(i), sigmay(i)
	else
	  read (5,*) theta(i), y(i), sigmay(i)
	end if
	if ( y(i) .eq. 0. .and. theta(i) .eq. 0. ) goto 20
	i = i + 1
	goto 10

20	npts = i - 1
	
25	write (6, *)' Enter order of highest Legendre Polynomial: '
	read (5,*) norder

30	write (6, 130)
130	format ('  Enter:  1 - even polynomials only	',/,
     +		'          0 - all P''s			',/,
     +		'         -1 - odd polynomials only	')
	read (5,*) neven
	if ( neven .gt. 1 ) goto 30
	if ( neven .lt. -1 ) goto 30

40	write (6, 140)
140	format ('  Enter:  1 - instrumental weight	',/,
     +		'          0 - no weight		',/,
     +		'         -1 - statistical weight	')
	read (5,*) mode
	if ( mode .gt. 1 ) goto 40
	if ( mode .lt. -1 ) goto 40

	call Legfitc ( theta, y, sigmay, npts, norder, neven,
     +		mode, ftest, yfit, a, sigmaa, b, sigmab, chisqr )

	WRITE (6,*) '( )'
	write (6, *)'    Chisqr = ', chisqr	
	write (6,*) '( )'

	write (6, 150)
150	format ('  Order(i)     Ai       ai     del ai    ')
	write (6, 155)
155	format (' =======================================   ')

	do i=1,norder+1
	  l = i - 1
	  write (6, 160) l, a(i), b(i), sigmab(i)
160	  format (5x,i1, 6X, f7.3, 2x, f7.3, 2x, f7.3 )
	end do
	write (6,*) '( )'

	write (6,*)' Would you like to continue?	(y/n) '
	read (5,170) ans
	if ( ans .eq. 'y' .or. ans .eq. 'Y' ) then
	  write (6, *)' Same data?	(y/n) '
	  read (5,170)  ans
 170	  format (a1)
	  if ( ans .eq. 'y' .or. ans .eq. 'Y' ) then
	    goto 25
	  else
	    check = 'n'
	    goto 2
	  end if
	end if

	goto 200
222	write (6,*) '( )'
	write (6,*)' Input File does not exist! '
	write (6,*) '( )'
	ans = 'n'
	goto 2

200	stop
	end
