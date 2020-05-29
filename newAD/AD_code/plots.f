	subroutine alpha
	call cwsend
	call toutpt(24)
	call tsend
	return
	end
C
C
C
C
	SUBROUTINE ACHAR(NUM,ICHAR)
	CHARACTER*1 CH,ICHAR
	DIMENSION CH(26)
	DATA CH/'A','B','C','D','E','F','G','H',
     +	'I','J','K','L','M','N','O','P','Q',
     +	'R','S','T','U','V','W','X','Y','Z'/
	N=NUM-64
	IF (N.GT.32) N=N-32
	ICHAR=CH(N)
	RETURN
	END
C
C
C
C
	SUBROUTINE LAXIS(IX,IY,LONG,IMIN,IMAX,IO)
C
C
C	subroutine laxis for log vertical axis constraction.
C	IX,IY=screen coordinates of the begining of the axis
C	LONG = length of the axis in screen units
C	IMIN , IMAX = minimum and maximum power of 10
C	IO>0 , ticks on the left , numbers on the right
C	IO<0 , ticks on the right , no numbers
C
C
C
c	horizontal=.true.
	N=IMAX-IMIN
	LL=LONG/N
	I10=10
	CALL MOVABS(IX,IY)
	IF(IO.LT.0)GO TO 5
	IX1=IX-60
	CALL MOVABS(IX1,IY)
	CALL ANMODE
        call anmode
	WRITE(6,100)I10
	IX2=IX-50
	IY2=IY+20
	CALL MOVABS(IX2,IY2)
	CALL ANMODE
        call anmode
	WRITE(6,101)IMIN
	CALL MOVABS(IX,IY)
5	DO 10 I=1,N
	DO 9 J=2,10
	IY1=IY+(ALOG10(FLOAT(J))+I-1)*LL
	CALL DRWABS(IX,IY1)
	IF(IO.GE.0)THEN
   	CALL DRWREL(10,0)
	CALL DRWREL(-10,0)
	ELSE
	CALL DRWREL(-10,0)
	CALL DRWREL(10,0)
	END IF
9	CONTINUE
	IF(IO.GE.0)THEN
	CALL DRWREL(20,0)
	CALL DRWREL(-20,0)
	IX1=IX-60
	CALL MOVABS(IX1,IY1)
	CALL ANMODE
        call anmode
	WRITE(6,100)I10
	IX2=IX-50
	IY2=IY1+20
	CALL MOVABS(IX2,IY2)
	NUM=IMIN+I
	CALL ANMODE
	WRITE(6,101)NUM
	CALL MOVABS(IX,IY1)
	ELSE
	CALL DRWREL(-20,0)
	CALL DRWREL(20,0)
	END IF
10	CONTINUE
	CALL MOVABS(IX,IY)
101	FORMAT(1H+,$,I3)
100     FORMAT(1H+,$,I2)
	RETURN
	END
C
C
C
C
C
	SUBROUTINE AXIS(IX,IY,LONG,AMIN,AMAX,N,IT,IO)
C
C
C	subroutine AXIS for linear axis constraction.
C	IX , IY = screen coordinates of the begining of the axis
C	LONG = length of the axis in screen units
C	AMIN , AMAX = minimum and maximun value to apear on the axis
C	N=number of decimal points on the axis label
C	IT >0 , X-axis , IT < 0 , Y-axis
C	IO > 0 , ticks on the left of the axis , numbers on the right
C	IO < 0 , ticks on the right of the axis , no numbers
C
C
C
	CHARACTER*1 TYPEc
	typec='f'
	DX=(AMAX-AMIN)*100./FLOAT(LONG)
	NN=LONG/100
	CALL MOVABS(IX,IY)
	IF(IT.LT.0)GO TO 20
	IF(IO.LT.0)GO TO 5
	CALL NUMBER(-90,-30,AMIN,N,-1,typec)
	CALL MOVABS(IX,IY)
5	DO 10 I=1,NN
	VALUE=AMIN+FLOAT(I)*DX
	LEN=I*100+IX
	CALL DRWABS(LEN,IY)
	IF(IO.GE.0)THEN
	CALL DRWREL(0,20)   
c      ! Possible error changed O to 0
	CALL NUMBER(-70,-50,VALUE,N,-1,typec)
	ELSE
	CALL DRWREL(0,-20)
	END IF
	CALL MOVABS(LEN,IY)
10	CONTINUE
	RETURN
20	IF(IO.LT.0)GO TO 21
	typec='f'
	CALL NUMBER(-130,0,AMIN,N,-1,typec)
	CALL MOVABS(IX,IY)
21	DO 30 I=1,NN
	VALUE=AMIN+FLOAT(I)*DX
	LEN=I*100+IY
	CALL DRWABS(IX,LEN)
	IF(IO.GE.0)THEN
	CALL DRWREL(20,0)
	CALL NUMBER(-150,-5,VALUE,N,-1,typec)
	ELSE
	CALL DRWREL(-20,0)
	END IF
	CALL MOVABS(IX,LEN)
30	CONTINUE
101	FORMAT(1H+,$,F5.1)
	RETURN
	END
C
C
C
	SUBROUTINE NUMBER(IX,IY,ALB,N,IT,typec)
C
C
C	subroutine NUMBER to write a number on the screen.
C	IX,IX=coordinates of posision
C	ALB=real number to be written at IX,IY
C	N=number of decimal points
C	IT>0 for absolute coordinates , IT<0  for relative
C	coordinates
C
C
	character*1 typec
c	horizontal=.true.
	IF( IT.GE.0 )then
            CALL MOVABS(IX,IY)
        END IF    
	IF(IT.LT.0)then
           CALL MOVREL(IX,IY)    
        END IF   
	CALL ANMODE
        call anmode
	IF(N.EQ.0) THEN
	  IALB =ALB
	  WRITE(6,100)IALB
	  RETURN
	ENDIF
	if(typec.eq.'f')then
	GO TO (1,2,3,4)N
1	WRITE(6,11)ALB
	RETURN
2	WRITE(6,12)ALB
	RETURN
3	WRITE(6,13)ALB
	RETURN
4	WRITE(6,14)ALB
	RETURN
11	FORMAT(F6.1)
12	FORMAT(F6.2)
13	FORMAT(F6.3)
14	FORMAT(F6.4)
100	FORMAT(I6)
	else
	write(6,16)alb
16	format(E8.2)
	END IF
	END
C
C
C
c
	SUBROUTINE LINE(IX,IY,LX,LY,X,Y,ER,NUM,XMIN,XMAX,
     z	YMIN,YMAX,KIND,LIN)
C
C
C	subroutine LINE to plot the points
C	IX,IY=screen coordinates of the begining of the axis.
C	LX,LY=length of x and y axises in screen units.
C	X,Y=one dimensional arrays containing the x and y values.
C	ER=one dimensional array containing the errors.
C	NUM=number of data points
C	XMIN,XMAX=minimum and maximun x-value
C	YMIN,YMAX=minimum and maximum y-value
C	KIND<0 for point to point plot with error bars
C	    =-1 draw star
C	    =-2 draw square
C	    = 0 solid line plot
C	    = 1 dotted line plot
C	    = 2 dot - dash line
C	    = 3 dashed line
C
C
C
	CHARACTER*1 STAR
	DIMENSION X(1),Y(1),ER(1),ER1(2048),ER2(2048) 
	DATA STAR /'*'/
c	horizontal=.true.
	IXMAX=IX+LX
	IYMAX=IY+LY
	CALL TWINDO(IX,IXMAX,IY,IYMAX)
	IF(LIN.EQ.0)THEN
	CALL DWINDO(XMIN,XMAX,YMIN,YMAX)
	ELSE
	IMI=NINT(YMIN)
	IMA=NINT(YMAX)
	YIMI=IMI
	YIMA=IMA
	CALL DWINDO(XMIN,XMAX,YIMI,YIMA)
	END IF
	CALL MOVABS(IX,IY)
	DO 100 I=1,NUM
	IF(LIN.EQ.0)THEN
	YY=Y(I)
	ER1(I)=Y(I)+ER(I)
	ER2(I)=Y(I)-ER(I)
	IF(ER1(I).GT.YMAX)ER1(I)=YMAX
	IF(ER2(I).LT.YMIN)ER2(I)=YMIN
	ELSE
	ER1(I)=ALOG10(Y(I)+ER(I))
	IF((Y(I)-ER(I)).LT.10.**(-8))THEN
	ER2(I)=YIMI
	ELSE
	ER2(I)=ALOG10(Y(I)-ER(I))
	END IF
	YY=ALOG10(Y(I))
	IF(ER1(I).GT.YIMA)ER1(I)=YIMA
	IF(ER2(I).LT.YIMI)ER2(I)=YIMI
	END IF
	IF(KIND.GE.0)GO TO 10
C	POINT TO POINT PLOT
	CALL MOVEA(X(I),YY)
	CALL MOVREL(-6,-2)
	IF(KIND.EQ.-1)THEN
	CALL ANMODE
        call anmode
	WRITE(6,101)STAR
	ENDIF
101	FORMAT(A1)
	IF(KIND.EQ.-2)CALL SQUARE
	CALL MOVEA(X(I),YY)
	CALL DRAWA(X(I),ER1(I))
	CALL DRWREL(-5,0)
	CALL DRWREL(10,0)
	CALL MOVEA(X(I),YY)
	CALL DRAWA(X(I),ER2(I))
	CALL DRWREL(-5,0)
	CALL DRWREL(10,0)
	CALL MOVEA(X(I),YY)
	GO TO 100
10	KIND=IABS(KIND)
	IF(KIND.EQ.0)THEN
	IF(I.EQ.1)then
           CALL MOVEA(X(I),YY)
        endif
	CALL DRAWA(X(I),YY)
	ELSE
	IF(I.EQ.1)then
           CALL MOVEA(X(I),YY)
        endif
	CALL DASHA(X(I),YY,KIND)
	END IF
100	CONTINUE
	RETURN
	END
C
C
C
C
	SUBROUTINE SQUARE
	CALL DRWREL(10,0)
	CALL DRWREL(0,10)
	CALL DRWREL(-10,0)
	CALL DRWREL(0,-10)
	RETURN
	END


