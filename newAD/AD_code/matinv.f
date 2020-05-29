c	subroutine matinv
c
c	purpose:
c	invert a symmetric matrix and calculate its determinant.

	subroutine matinv ( array, norder, det )

	double precision array, amax, save
	dimension array(10,10), ik(10), jk(10)

10	det = 1.
11	do 100 k=1,norder

c	  find largest element array(i,j) in rest of matrix

	amax = 0.
21	do 30 i=k,norder
	do 30 j=k,norder
23	if ( dabs(amax) - dabs(array(i,j)) ) 24, 24, 30
24	amax = array(i,j)
	ik(k) = i
	jk(k) = j
30	continue

c	  interchange rows and columns to put amax in array(k,k)

31	if ( amax ) 41, 32, 41
32	det = 0.
	goto 140
41	i = ik(k)
	if ( i-k ) 21, 51, 43
43	do 50 j=1,norder
	save = array(k,j)
	array(k,j) = array(i,j)
50	array(i,j) = -save
51	j = jk(k)
	if ( j-k ) 21, 61, 53
53	do 60 i=1,norder
	save = array(i,k)
	array(i,k) = array(i,j)
60	array(i,j) = -save

c	  accumulate elements of inverse matrix

61	do 70 i=1,norder
	if ( i-k ) 63, 70, 63
63	array(i,k) = -array(i,k) / amax
70	continue
71	do 80 i=1,norder
	do 80 j=1,norder
	if ( i-k ) 74, 80, 74
74	if ( j-k ) 75, 80, 75
75	array(i,j) = array(i,j) + array(i,k) * array(k,j)
80	continue
81	do 90 j=1,norder
	if ( j-k ) 83, 90, 83
83	array (k,j) = array(k,j) / amax
90	continue
	array(k,k) = 1. / amax
100	det = det * amax

c	  restore ordering of matrix

101	do 130	l=1,norder
	k = norder - l + 1
	j = ik(k)
	if ( j-k ) 111, 111, 105
105	do 110 i=1,norder
	save = array(i,k)
	array(i,k) = - array(i,j)
110	array(i,j) = save
111	i = jk(k)
	if ( i-k ) 130, 130, 113
113	do 120	j=1,norder
	save  = array(k,j)
	array(k,j) = - array(i,j)
120	array(i,j) = save
130	continue
140	return
	end

