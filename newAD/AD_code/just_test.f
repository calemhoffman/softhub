	PROGRAM test

		character*50 buff,fname
		open (2, file = '1695a.0', status = 'old')
	    read (2,3010) exxx
3010	format (20x, f6.1)
		write (*,*) exxx

		read (2,3011) buff
3011	format (5x)
		write (*,*) buff


c-------format test---------------
c      x = 0.025

c      write(*,130) 'x=', x
c  130 format (A,F4.3)
c---------------------------------

	  CLOSE (2)

      End

c	gfortran -ffixed-line-length-132 just_test.f 



