c------------------------------------------------------------------------------

	subroutine filout ( icnt, iarray )
	include 'pcplot.inc'
C *
C *	This routine allows passing of all characters to an output file.
C *
	dimension iarray ( icnt )
	character karray (768)
C *
C *	Write out the data.
C *	
	do 10 k=1,icnt
	  karray(k) = char(iarray(k))
10	continue
	if( icnt .lt. 768 ) then
	  do 20 k=icnt+1, 768
	    karray(k) = char(0)
   20	  continue
	end if
C *
C *	Output file format is count, 768 bytes
C *
	write( ifsunt,'(i5,768a)' ) icnt,  karray

	return
	end
   
c------------------------------------------------------------------------------

	subroutine KA12AS ( nchar, ka1, kade )

c	This routine is unnecessary on the VAX
c	since A1 is right justified as is ADE.

	dimension ka1(nchar), kade(nchar)

	if ( nchar .lt. 1 ) goto 900
	do 100 i=1,nchar
100	kade(i) = ka1(i)

900	return
	end

c------------------------------------------------------------------------------

	subroutine KAM2AS ( nchar, kam, kade )

c	This routine translates NCHAR characters from A4
c	format in KAM to ADE format in KADE.

	dimension kade(1)
	character kam(72)

	do 200 i=1,nchar
200	kade(i) = ichar(kam(i))

	return
	end

c------------------------------------------------------------------------------

	subroutine KAS2A1 ( nchar, kade, ka1 )

c	This routine is unnecessary on the VAX
c	since ADE is already in the same format
c	as A1 (right justified).

	dimension kade(nchar), ka1(nchar)

	if ( nchar .lt. 1 ) goto 900
	do 100 i=1,nchar
100	ka1(i) = kade(i) 

900	return
	end

c------------------------------------------------------------------------------

	subroutine KAS2AM ( nchar, kade, kam )

c	This routine translates NCHAR characters from ADE
c	format in KADE to A4 format in KAM.

	dimension kade(1)
	character kam(72)

	do 200 i=1,nchar
200	kam(i) = char(kade(i)) 

	return
	end

	subroutine fsutrm
C
C	This subroutine is used to select from one of the available 
C	terminal types.  It sets the variable in the common block so that
C	it can be used by other subroutines.
C
	character*1 reply
	include 'pcplot.inc'
	write (6,*) '  Enter a terminal type from the following:'
	write (6,*) '        1  -- An IBM PC emulating a 4010 terminal'
	write (6,*) '        2  -- A Visual 500 terminal'
	write (6,*) '        3  -- A Visual 550 terminal'
	write (6,*) '        4  -- A Qume terminal'
   10	continue
	write (6,*) ' Your reply should be a number between 1 and 4'
	read (5,'(a)') reply
	if(     reply .eq. '1' ) then
	  ifstyp = 1
	  ITERM  = 1
	elseif( reply .eq. '2' ) then
          ifstyp = 2
          iterm  = 3
	elseif( reply .eq. '3' ) then
	  ifstyp = 3
	  iterm  = 3
	elseif( reply .eq. '4' ) then
	  ifstyp = 4
	  iterm  = 3
	else
	  write (6,*) ' Your response was not a number between 1 and 4'
	  write (6,*) ' Try again ( 1=PC, 2=V500, 3=V550, 4=QUME )'
	  go to 10
	end if
	IRATE = 0
	IBF = 3
	CALL INITT( IRATE )
	CALL TERM ( ITERM , 1024 )
	CALL SETBUF ( IBF )
	return
	end

	subroutine onlyal
	include 'pcplot.inc'
C
C	Sets the terminal into only alpha mode.  On the Visual this means
C	Return to VT100 mode, on the PC it should mean return to VT220 mode
C	It is not allowed on the QUME as far as I can tell.
C
	if( ifstyp .eq. 1 ) then
	  call vtmode
C
C	  It is necessary that the screen be replotted if you are using
C	  a PC Emulator and have to display a lot of text in this mode.
C
	elseif( ifstyp .eq. 2 .or. ifstyp .eq. 3 ) then
	  call cwsend
	  call toutpt (24)
	  call tsend
	end if

	return
	end
C
C	The subroutine below opens a file for 4010/4014 commands and
C	can be used by other devices to redisplay the plots.
C
	subroutine setfil( filnme, iunit )
	character*(*) filnme
	include 'pcplot.inc'
C
C	this routines requires two variables 
C	filnme is the name of the file to be open for output
C	iunit is the unit number used for this output
C
	ifsnme = filnme
	ifsunt = iunit
	open( unit = ifsunt, file=ifsnme, status='NEW',
     .	      form = 'formatted', err=40 )
	ifsopn = 1
	return
   40	ifsopn = 0
	return
	end
C
C	The subroutine below closes the 4010 echo file.
C	
	subroutine clsfil
	include 'pcplot.inc'
	close( ifsunt )
C
C	Set the file to closed and disable command echoing
C
	ifsopn = 0
	ifsfil = 0
	return
	end
C
C	Filon enables echoing of the 4010 output to a file provided a file
C	is already open for that purpose
C	
	subroutine filon
	include 'pcplot.inc'
	if( ifsopn .ne. 0 ) then
	  ifsfil = 1
	else
	  ifsfil = 0
	end if
	return
	end
C
C	Filoff disables echoing of the 4010 commands to an output file
C
	subroutine filoff
	include 'pcplot.inc'
	ifsfil = 0
	return
	end


	function jint( x )
	jint = ifix( x )
	return
	end
