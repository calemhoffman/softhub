
	common /PCPLOT/ tminvx, tminvy, tmaxvx, tmaxvy, trealx, trealy,
     .			timagx, timagy, trcosf, trsinf, trscal, trfacx,
     .			trfacy, trpar1, trpar2, trpar3, trpar4, trpar5,
     .			trpar6,	
     .			kgnmod, kpadv,  kachar, koblen, ktrail, klevel,
     .			kpad2,  kbaudr, kgnflg, kgrafl, khomey, kkmode,
     .			khorsz, kversz, ktblsz, ksizef, klmrgn, krmrgn,
     .			kfactr, kterm,  kline,  kzaxis, kbeamx, kbeamy,
     .			kmovef, kdasht, kminsx, kminsy, kmaxsx, kmaxsy,
     .			keycon, kinlft, kotlft, kunit,
     .			kmoflg(2), kpchar(5)
c
c	customized graphics additions 
c
	character*50 ifsnme
	common /FSUPLT/ ifstyp, ifsfil, ifsopn, ifsunt
	common /FSUNME/ ifsnme
c
c		ifstyp   --  determines terminal type used by some of
c			     the commands that go in and out of modes
c			1  =  an IBM PC emulating a 4010 terminal
c			2  =  a Visual 500
c			3  =  a Visual 550
c			4  =  a QUME terminal
c
c		ifsfil   --  determines if the plotting commands should be
CC 			       output to a file as well as the screen
C
C		ifsopn   --  flag that tells if the file is open
C
C		ifsunt   --  unit number of the file opened for echo output
C
C		ifsnme   --  a 50 character file name
C















