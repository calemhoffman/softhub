c
c	subroutine legfitc(ode)
c
c       Notes 1-3-2000, SLT:  This was adapted from Bevington.  In recompiling
c       it I noticed a small error in statement 22 and corrected it.  Also
c       I will reproduce Phil Bevington's comments.
c
c	purpose:
c	make a least squares fit to data with a Legendre polynomial
c	  	Y = A(1) + A(2)*X + A(3)*(3X**2-1)/2 + ...
c		  = A(1) * (1. + B(2)*X + B(3)*(3X**2-1)/2 + ...)
c
c	subroutines:
c	  Matinv ( array, nterms, det )
c		inverts a symmetric two-dimensional matrix of degree
c		nterms and calculates its determinant.
c       theta  - array of angles (in degrees) of the data points
c       y      - array of data points for dependent variable
c       sigmay - array of standard deviations for y data points
c       npts   - number of pairs of data points
c       norder - highest order of polynomial (number of terms - 1)
c       neven  - determines odd or even character of polynomial
c                 +1 fits only to even terms
c                  0 fits to all terms
c                 -1 fits only to odd terms
c       mode   - determines mode of weighting least-squares fit
c                 +1 (instrumental ) weight(i) = 1./sigmay(i)**2
c                  0 (no weighting)  weight(i) = 1.
c                 -1 (statistical)   weight(i) = 1./Y(i)
c       ftest  - array of values of f(i) for an F test
c       yfit   - array of calculated values of y
c       a      - array of coerricients of polynomial
c       sigmaa - array of standard deviations for coefficients
c       chisq  - reduced chi square for fit

	subroutine legfitc ( theta, y, sigmay, npts, norder, neven,
     +		mode, ftest, yfit, a, sigmaa, b, sigmab, chisqr )

	real*8 cosine, p, beta, alpha, chisq
	dimension theta(1), y(1), sigmay(1), ftest(1), yfit(1),
     +		a(1), sigmaa(1), b(1), sigmab(1)
	dimension weight(100), p(100,10), beta(10), alpha(10,10)

c	  Accumulate weight and Legendre Polynomials

11	nterms = 1
	ncoeff = 1
	jmax = norder + 1


20	do 40 i=1,npts

21	if ( mode ) 22,27,29
22	if ( y(i) ) 25,27,23
23	weight(i) = 1./y(i)
	goto 31
25	weight(i) = 1./(-y(i))
	goto 31
27	weight(i) = 1.
	goto 31
29	weight(i) = 1./sigmay(i)**2
31	cosine = cos ( .01725329252 * theta(i) )
	p(i,1) = 1.
	p(i,2) = cosine
	do 36 l=2,norder
	fl = l
36 	p(i,l+1) = ((2.*fl-1.) * cosine*p(i,l) - (fl-1.)*p(i,l-1)) / fl
40	continue

	write (6,*) ' '

c	  accumulate matrices alpha and beta

51	do 54 j=1,nterms
	beta(j) = 0.
	do 54 k=1,nterms
54	alpha(j,k) = 0.
61	do 66 i=1,npts
	do 66 j=1,nterms
	beta(j) = beta(j) + p(i,j)*y(i)*weight(i)
	do 66 k=j,nterms
	alpha(j,k) = alpha(j,k) + p(i,j)*p(i,k)*weight(i)
66	alpha(k,j) = alpha(j,k)

c	  delete fixed coefficients

70	if ( neven ) 71, 91, 81
71	do 76 j=3,nterms,2
	beta(j)=0.
	do 75 k=1,nterms
	alpha(j,k) = 0.
75	alpha(k,j) = 0.
76	alpha(j,j) = 1.
	goto 91
81	do 86 j=2,nterms,2
	beta(j) = 0.
	do 85 k=1,nterms
	alpha(j,k) = 0.
85	alpha(k,j) = 0.
86	alpha(j,j) = 1.

c	  invert curvature matrix alpha

91	do 95 j=1,jmax
	a(j) = 0.
	sigmaa(j) = 0.
	b(j) = 0.
95	sigmab(j) = 0.
	do 97 i=1,npts
97	yfit(i) = 0.
101	call matinv ( alpha, nterms, det )
	if ( det ) 111, 103, 111
103	chisqr = 0.
	goto 170

c	  calculate coefficients, fit, and chi square

111	do 115 j=1,nterms
	do 113 k=1,nterms
113	a(j) = a(j) + beta(k) * alpha(j,k)
	do 115 i=1,npts
115	yfit(i) = yfit(i) + a(j) * p(i,j)
121	chisq = 0.
	do 123 i=1,npts
123	chisq = chisq + (y(i) - yfit(i))**2 * weight(i)
	free = npts - ncoeff
	if ( free .lt. 1 ) free = 1
	chisqr = chisq / free

c	  test for end of fit

131	if ( nterms - jmax ) 132,151,151
132	if ( ncoeff - 2 ) 133, 134, 141
133	if ( neven ) 137, 137, 135
134	if ( neven ) 135, 137, 135
135	nterms = nterms + 2
	goto  138
137 	nterms = nterms + 1
138	ncoeff = ncoeff + 1
	chisq1 = chisq
	goto 51
141	fvalue = ( chisq1 - chisq ) / chisqr
	if ( ftest(nterms) - fvalue ) 134, 143, 143
143	if ( neven ) 144, 146, 144
144	nterms = nterms - 2
	goto 147
146	nterms = nterms - 1
147	ncoeff = ncoeff - 1
	jmax = nterms 
149	goto 51

c	  calculate remainder of output

151	if ( mode ) 152, 154, 152
152	varnce = 1.
	goto 155
154	varnce = chisqr
155	do 156 j=1,nterms
156	sigmaa(j) = sqrt ( varnce * alpha(j,j) )
161	if ( a(1) ) 162, 170, 162
162	do 166 j=2,nterms
	if ( a(j) ) 164, 166, 164
164	b(j) = a(j) / a(1)
165	sigmab(j) = b(j) * sqrt ( (sigmaa(j)/a(j))**2 
     +	  + (sigmaa(1)/a(1))**2 - 2.*varnce*alpha(j,1)/(a(j)*a(1)) )
166	continue
	b(1) = 1.
170	write (6,201)
201	format ('     Theta     Yield     Error     Fit        Diff')
	write (6,202)
202	format ('    ================================================ ')
	do 300 i=1,npts
	DIF = Y(I) - YFIT(I)
	write (6,203) theta(i), y(i), sigmay(i), YFIT(I), DIF
203	format ('     ', f5.1, 3x, f7.3, 3x, f7.3, 3X, F7.3, 3X, F7.3 )

300	CONTINUE
 	return
	end
