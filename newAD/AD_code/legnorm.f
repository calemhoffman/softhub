c     Subroutine legnorm
c     Reconstructed by SLT on 1-4-2000 since the source code has been lost.
c     I assume this is to find the optimum normalization ynorm between a
c     set of data points and a Legendre polynomial of the form
c     y(i) = 1 + a2*P2(cos(thet(i))) + a4*P4(cos(thet(i))).
c     That is, it assumes that a0 = 1, etc.
c
c     Note that I have assumed that the ad.inc file is included to have the
c     yerr as well as lmax, which is the number of data points (not max ell).
c     I have had to remove the variables passed which are also in ad.inc.

      subroutine legnorm(a3,a5,yth,ynorm)
      include 'ad.inc'
      dimension a(10),x(100),y(100)

c     First set up the Legendre polynomial coefficients a(i)

      do 10 i = 1, 10
 10      a(i) = 0.
      a(1) = 1.
      a(3) = a3
      a(5) = a5

c     Now we need to calculate the array of yth for these Legendre coeffs
      do i = 1, lmax
         x(i) = thet(i)
         end do
      lsam = lmax
      call legendre (x, y, a, lsam)

      anum = 0.
      denom = 0.
      do 20 i = 1, lmax
         yth(i) = y(i)
         anum = anum + yth(i)*yexp(i)/yerr(i)**2
         denom = denom + yth(i)**2 / yerr(i)**2
 20      continue
      ynorm = 1.
      if (denom .ne. 0.) ynorm = anum/denom
      do 30 i = 1, lmax
         yth(i) = yth(i) * ynorm
 30      continue
      return
      end







