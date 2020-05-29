c     subroutine legendre
c
c     Written 1-3-2000 by SLT to replace a missing source code dating back 
c     to Uwe's time.  I am guessing a little about what was in it.  I will
c     adapt the code from a section of Bevington's legfit which is
c     completely reproduced in "legfitc.f", also used by "ad.f" for which
c     this is being written.  Legendre will calculate NPTS Y values from
c     NPTS X values (theta in degrees) according to the Legendre
c     coefficients in the array A(10).  Note that this array uses both
c     even and odd order coefficients.  One simply leaves as 0 any that
c     are not being used.

      subroutine legendre (x, y, a, npts)
      dimension x(100), y(100), a(10)
      real*8 p(10), fl, cosine, theta
      do 100 i = 1, npts
         theta = x(i)
         cosine = dcos (0.0174532925 * theta)
         p(1) = 1.
         p(2) = cosine
         do 20 l = 2, 9
            fl = float(l)
            p(l+1) = ((2.*fl-1.)*cosine*p(l) - (fl-1.)* p(l-1))/fl
 20         continue
         y(i) = 0.
         do 30 l = 1, 10
            y(i) = Y(i) + a(l) * P(l)
 30         continue
 100     continue
         return
         end
