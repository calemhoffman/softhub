	PROGRAM DCO2
	DIMENSION IP(10),JP(10),THETA(10),PHI(10,10),UFAC(19)
	DIMENSION RDCO(3,181),W(181),AI1(3),EFF(10,2),E(2)
	CHARACTER*1 ANS
	REAL*8 XX,YY,ERR,FACT,XMIN,XMAX,AT
	COMMON/ALAMB/ALAM(3),ALAM1(19),ALAM2(19),Q(19),BLAM1(3,3)
	COMMON/ANGLE/RAD,PI
	COMMON/ALMIX/DEL34,ALI(3),ALIP(3),ALG,ALGP
	COMMON/DETEC/AD(10),BD(10),CD(10),DD(10)
	COMMON/ECOEF/EFAC(19)
	COMMON/FCOEFS/FO11(19),FO12(19),FO22(19),
     +  FG11(3,3,19),FG12(3,3,19),FG22(3,3,19)
	COMMON/PLOT/XX(10,200),YY(10,200),ERR(10,200),NUMB(10),KIN(10),
     +  FACT(10),XMIN,XMAX,ND
	COMMON/X/XF(19)
	DATA RAD/.017453293/PI/3.141592654/
        integer*2 idim,ltype,ptype,ptype1,ptype2,stype
        integer*2 ltype1,ltype2,ltype3
	CALL LOG1
	DO 1000 K=1,3
	DO 1000 I=1,181
1000	 RDCO(K,I)=0.0
	DEL34=0.0
C
C
1	WRITE (6, 100)
100	FORMAT(' PROGRAM DCO2 - CONVENTIONS OF STEFFEN, ALDER, TWIN,
     +  AND HAMLITON'                          
     +  ,/,' -THE ELECTROMAGNETIC INTERACTION IN NUCLEAR SPECTROSCOPY',/,
     +     '  -HAMILTON EDITOR, NORTH-HOLLAND 1975')	
	WRITE (6,*)' GAMMA CASCADE CHAIN FOR DIRECTIONAL CORRELATION - '
	WRITE (6, 110)
110     FORMAT(/' SIGMA = WIDTH OF GAUSSIAN M1 DISTRIBUTION',/
     +	,/,'      I1=I2 + 0,1, OR 2    --------- I3'
     +  ,/,'                               |       '
     +  ,/,'                     GATE ->   |       '
     +  ,/,'                               V       '
     +  ,/,'                           --------- I4'
     +  ,/,'                               :       '
     +  ,/,' INTERMEDIATE TRANSITIONS ->   :       '
     +  ,/,'                               :       '
     +  ,/,'                           --------- I1'
     +  ,/,'                               |       '
     +  ,/,'   TRANSITION OF INTEREST ->   |       '
     +  ,/,'                               V       '
     +  ,/,'                           --------- I2',//)
	WRITE (6,*)' TYPE IN I2,SIGMA FOR TRANS. OF INTEREST'
	READ(5,*)AI2,SIGMA
	WRITE (6,*)' AI2, SIGMA =',AI2,SIGMA
	 AI1(1)=AI2+1.0
	 AI1(2)=AI2+2.0
         AI1(3)=AI2
	WRITE (6,*)' TYPE IN I3,14'
	READ(5,*)AI3,AI4
	IF((AI3-AI4).EQ.1.0) THEN
	 WRITE (6,*)' TYPE IN MIXING RATIO FOR GATE TRANSITION'
	 READ(5,*)DEL34
	ELSE
	 DEL34=0.0
	ENDIF
C
C	Calculate the de-orientation parameters U(Ii,If,L) as defined
C	in eqs. 14.17 and 14.18 in Hamilton, for any unobserved
C	intermediate transitions in the cascade.
C
	DO 399 K=1,19
399	 UFAC(K)=1.0
C       IF (AI2.GE.AI3) THEN
         DLJ=AI2-AI3
       	IF(DLJ.EQ.0.0)GO TO 460
	WRITE (6,*)' ARE THERE ANY DELTA J = 1  INTERMEDIATE TRANSITIONS IN
     +   THE CASCADE? ( Y OR N)'
	READ(5,'(A1)')ANS
	IF((ANS.EQ.'N').OR.(ANS.EQ.'n'))THEN
	 NGAM=INT(DLJ/2.)
	 DO 400 K=1,19
	  ALM2=ALAM2(K)
	  DO 410 N=1,NGAM
	   AII=AI2-2.0*FLOAT(N-1)
	   AIF=AII-2.0
	   DELIF=0.0
	   UFAC(K)=UFAC(K)*ULAM2(AII,AIF,ALM2,DELIF)
410	  CONTINUE
400	 CONTINUE
	ELSE
	 DO 415 K=1,19
415	  UFAC(K)=1.0
	 WRITE (6,*)' TYPE IN # OF INTERMEDIATE TRANSITIONS'
	 READ(5,*)NGAM
	 DO 420 N=1,NGAM
	  WRITE (6,*)' TYPE IN INITIAL, FINAL SPIN AND MIXING RATIO FOR
     +    GAMMA # ',N
	  READ(5,*)AII,AIF,DELIF
	  DO 430 K=1,19
	   ALM2=ALAM2(K)
	   UFAC(K)=UFAC(K)*ULAM2(AII,AIF,ALM2,DELIF)
430	  CONTINUE
420	 CONTINUE
	ENDIF
C       ENDIF
C	WRITE (6,*)' AI3,AI4,DEL34 =',AI3,AI4,DEL34
460	WRITE (6,*)' TYPE IN # OF DETECTORS'
	READ(5,*)NDET
	WRITE (6,*)' DO YOU WANT TO CORRECT FOR DETECTOR EFFICIENCY ? 
     +   (Y OR N)'
	READ(5,'(A1)')ANS
	IF((ANS.EQ.'Y').OR.(ANS.EQ.'y'))THEN
	WRITE (6,*)' TYPE IN ENERGY OF TRANS. OF INTEREST, GATE'
	READ(5,*)E(1),E(2)
	WRITE (6,*)'  EGAM1, EGAM2 =',E(1),E(2)
C	WRITE (6,*)' EFF(E)=10(A+B*LOG(E)+C*(LOG(E))**2+D/E**2)'
C	WRITE (6,*)' TYPE IN A,B,C,D FOR EACH DETECTOR'
	DO 25 N=1,NDET
C	 WRITE (6,*)' DET # ',N
C	 READ(5,*)AD(N),BD(N),CD(N),DD(N)
	 X1=AD(N)+BD(N)*ALOG10(E(1))+CD(N)*(ALOG10(E(1)))**2
     1	+DD(N)/(E(1)**2)
	 X2=AD(N)+BD(N)*ALOG10(E(2))+CD(N)*(ALOG10(E(2)))**2
     1	+DD(N)/(E(2)**2)
	 EFF(N,1)=10**X1
	 EFF(N,2)=10**A2
25	CONTINUE
	ELSE
	 DO 26 N=1,NDET
	  DO 26 NE=1,2
26	   EFF(N,NE)=1.0
	ENDIF
	WRITE (6,*)' FOR EACH PAIR OF DETECTORS, TYPE IN THE FOLLOWING:'
	WRITE (6,*)' #-FIRST DETECTOR, #-SECOND DETECTOR, THETA-FIRST DET.
     +  ,THETA-SECOND DET., PHI BETWEEN THE TWO DETECTORS'
	WRITE (6,*)' TO END-   TYPE   0,0,0,0,0'
30	READ(5,*)I,J,THETA(I),THETA(J),PHI(I,J)
	IF ((I.EQ.0).AND.(J.EQ.0))THEN
	 GO TO 35
	ELSE IF(I.EQ.J)THEN
	 WRITE (6,*)' YOU CANT HAVE CORRELATIONS IN THE SAME DETECTOR, IDIOT'
	 WRITE (6,*)' TRY AGAIN ! '
	 THETA(I)=0.0
	 THETA(J)=0.0
	 PHI(I,J)=0.0
	 GO TO 30
	ELSE
	 PHI(J,I)=PHI(I,J)
	 NPAIRS=NPAIRS+1
	 IP(NPAIRS)=I
	 JP(NPAIRS)=J
	 GO TO 30
	ENDIF
35	CONTINUE
	DO 36 IJ=1,NPAIRS
	 I=IP(IJ)
	 J=JP(IJ)
	 WRITE (6,*)' I,THETA(I),J,THETA(J)=',I,THETA(I),J,THETA(J)
36	CONTINUE
C
C	Here begins the process of calculating the DCO function
C	for each set of detectors. First calculate the B(I1) statistical
C	tensors for the initial state, assuming axial alignment with
C	a Gaussian distrubution of magnetic substates about the beam
C	axis of width SIGMA.	
C
	CALL BLAM(AI1,SIGMA)
C
C
       DO 200 J=1,3
	 ALI(J)=AI1(J)-AI2
         IF (ALI(J).EQ.0.0) ALI(J)=1.0
200    CONTINUE 
	 ALIP(1)=ALI(1)+1
	 ALIP(2)=ALI(2)
         ALIP(3)=ALI(3)+1
	IF ((AI3-AI4).EQ.1.0) THEN
	 ALG=1
	 ALGP=ALG+1
	ELSE
	 ALG=2
	 ALGP=ALG
	ENDIF 
C
C
C	Calculate the generalized (FG), ordinary (FO) F-coefficients,
C	as defined in Krane, Steffen, and Wheeler, also defined in
C	eqs. 12.163 and 12.168 respectively in Hamilton.
C
C	
	DO 50 I=1,19
	 ALM1=ALAM1(I)
	 ALM2=ALAM2(I)
	 QP=Q(I)
	 IF(ALG.EQ.1.0)THEN
	  FO11(I)=FO(ALM2,ALG,ALG,AI4,AI3)
	  FO12(I)=FO(ALM2,ALG,ALGP,AI4,AI3)
	  FO22(I)=FO(ALM2,ALGP,ALGP,AI4,AI3)
	 ELSE
	  FO11(I)=0.0
	  FO12(I)=0.0
	  FO22(I)=FO(ALM2,ALG,ALG,AI4,AI3)
	 ENDIF
	 DO 55 K=1,3
	 A1=AI1(K)
	 DO 56 N=1,3
	  ALM=ALAM(N)
	  IF(ALI(K).EQ.1.0)THEN
	   FG11(K,N,I)=FG(ALM,ALM1,ALM2,ALI(K),ALI(K),AI2,A1)
	   FG12(K,N,I)=FG(ALM,ALM1,ALM2,ALI(K),ALIP(K),AI2,A1)
	   FG22(K,N,I)=FG(ALM,ALM1,ALM2,ALIP(K),ALIP(K),AI2,A1)
	  ELSE
	   FG11(K,N,I)=0.0
	   FG12(K,N,I)=0.0
	   FG22(K,N,I)=FG(ALM,ALM1,ALM2,ALI(K),ALI(K),AI2,A1)
	  ENDIF
56	 CONTINUE
55	CONTINUE
50	CONTINUE 
C
C	Calculate the E(gam(2)...gam(n-1)) factors defined in eq. 15.7
C	in Hamilton.		
C
	IF(ALG.EQ.1.0)THEN
	 DO 60 K=1,19
	  AG=(FO11(K)+2.*DEL34*FO12(K)+DEL34*DEL34*FO22(K))
	  AG=AG/(1.0+DEL34*DEL34)
	  EFAC(K)=AG*UFAC(K)
60	 CONTINUE
	ELSE
	 DO 65 K=1,19
65	  EFAC(K)=UFAC(K)*FO22(K)
	ENDIF
C
C	Now calculate the DCO ratio as a function of mixing ratio
C
	WRITE (6,*)' NOW CALCULATING THE DCO RATIO'
C
	DO 1010 K=1,3
	IF(ALI(K).EQ.1)THEN
	 DO 1002 I=1,181
	  WNT=0.0
	  WDT=0.0
	  DO 1001 IJ=1,NPAIRS
	   IDET1=IP(IJ)
	   JDET2=JP(IJ)
	   THETA1=THETA(IDET1)
	   THETA2=THETA(JDET2)
	   PHI12=PHI(IDET1,JDET2)
	   CALL WDCO(K,I,THETA1,THETA2,PHI12,W)
	   WN=W(I)*EFF(IDET1,1)*EFF(JDET2,2)
	   CALL WDCO(K,I,THETA2,THETA1,PHI12,W)
C	WRITE (6,*)'W(I),EFF(1),EFF(2)=',W(I),EFF(IDET1,1),EFF(JDET2,2)
	   WD=W(I)*EFF(JDET2,1)*EFF(IDET1,2)
	   WNT=WNT+WN
	   WDT=WDT+WD
1001	  CONTINUE
	  RDCO(K,I)=RDCO(K,I)+WDT/WNT
1002	 CONTINUE
        ELSE
	 I=91
	 WNT=0.0
	 WDT=0.0
	 DO 1003 IJ=1,NPAIRS
	   IDET1=IP(IJ)
	   JDET2=JP(IJ)
	   THETA1=THETA(IDET1)
	   THETA2=THETA(JDET2)
	   PHI12=PHI(IDET1,JDET2)
	   CALL WDCO(K,I,THETA1,THETA2,PHI12,W)
	   WN=W(I)*EFF(IDET1,1)*EFF(JDET2,2)
	   CALL WDCO(K,I,THETA2,THETA1,PHI12,W)
	   WD=W(I)*EFF(JDET2,1)*EFF(IDET1,2)
	   WNT=WNT+WN
	   WDT=WDT+WD
1003	 CONTINUE 
	 RDCO(K,I)=RDCO(K,I)+WDT/WNT
	ENDIF
1010	CONTINUE
C
C	Now plot the DCO ratio and exp. data vs. arctan(delta)
C
C        DO 99 K=1,3
C          WRITE (6,*)'RDCO(',K,'91)=',RDCO(K,91)
C99      CONTINUE
	WRITE (6,*)' FINISHED WITH CALCULATION !'
	WRITE (6,*)' WOULD YOU LIKE TO PLOT RESULTS ( Y OR N)'
	READ(5,'(A1)')ANS
	IF((ANS.EQ.'Y').OR.(ANS.EQ.'y')) THEN
	WRITE (6,*)' TYPE IN EXP. DCO RATIO AND ERROR IN R EXP.'
	READ(5,*)REXP,RERR
201     WRITE (6,*)' TYPE 0 TO PLOT I1=I2+1, I1=I2+2'
        WRITE (6,*)' TYPE 1 TO PLOT I1=I2, I1=I2+2'
        WRITE (6,*)' TYPE 2 TO PLOT I1=I2 (DASHED), I1=I2+1 (SOLID)'
        READ(5,*)NUM
        write (6,*)' Do you want an output file for Lipha?'
        read(5,'(a1)')ans
	 XMIN=-90.
	 XMAX=90.
	 ND=5
	 KIN(1)=0
	 IF ((NUM.EQ.0).OR.(NUM.EQ.1))  KIN(2)=-1
         IF (NUM.EQ.2) KIN(2)=1
	 KIN(3)=0
	 KIN(4)=1
	 KIN(5)=1
	 FACT(1)=1.
	 FACT(2)=1.
	 FACT(3)=1.
	 FACT(4)=1.
	 FACT(5)=1.
	 NUMB(1)=181
	 IF ((NUM.EQ.0).OR.(NUM.EQ.1)) NUMB(2)=1
         IF (NUM.EQ.2) NUMB(2)=181
	 NUMB(3)=181
	 NUMB(4)=181
	 NUMB(5)=181
	 DO 1004 K=1,181
	  KD=91-K
	  XX(1,K)=KD
          IF (NUM.EQ.2) XX(2,K)=KD
	  XX(3,K)=KD
	  XX(4,K)=KD
	  XX(5,K)=KD
	  IF ((NUM.EQ.0).OR.(NUM.EQ.2)) YY(1,K)=RDCO(1,K)
          IF (NUM.EQ.1) YY(1,K)=RDCO(3,K)
          IF (NUM.EQ.2) YY(2,K)=RDCO(3,K)
          YY(3,K)=REXP
	  YY(4,K)=REXP+RERR
	  YY(5,K)=REXP-RERR
	  ERR(1,K)=0.0
          IF (NUM.EQ.2) ERR(2,K)=0.0
	  ERR(3,K)=0.0
	  ERR(4,K)=0.0
	  ERR(5,K)=0.0
1004	CONTINUE
	IF ((NUM.EQ.0).OR.(NUM.EQ.1)) XX(2,1)=0.
C	WRITE (6,*)' RDCO(2,90),RDCO(2,91)=',RDCO(2,90),RDCO(2,91)
	IF ((NUM.EQ.0).OR.(NUM.EQ.1)) YY(2,1)=RDCO(2,91)
	ERR(2,1)=0.0
	CALL PLOTF
	ENDIF
C   prepare to write lipha file if necessary 
        if ((ans .eq. 'y') .or. (ans .eq. 'Y')) then
          open (unit=3,status='new')
          write (6,*)' Type 1 for solid deltaI=1, dashed deltaI=0'
          write (6,*)' Type 2 for solid deltaI=0, dashed deltaI=1'
          read(5,*) num1
          kpn1 = 181
          kpn2 = 1
          idim = 2
          ltype = 0
          ltype1 = 1
          ltype2 = 2
          ltype3 = 3
          ptype1 = 11
          ptype2 = 0
          stype = 3
          write(3,325) kpn2,idim,ltype,ptype1
325       format (1x,i4,2x,i2,2x,i2,2x,i2)
          write(3,326) ltype,rdco(2,91)
326       format (1x,i2,2x,f5.3)
            if (num1 .eq. 2) then
                 write(3,330) kpn1,idim,ltype1,ptype2,stype
330              format (1x,i4,2x,i2,2x,i2,2x,i2,2x,i2)
                 do 340 i = 181,1,-1
                   at = 91 - i
                   write(3,332) at,rdco(3,i)
332                format (1x,f5.1,2x,f5.3)
340              continue
                 write(3,345) kpn1,idim,ltype2,ptype2,stype
345              format (1x,i4,2x,i2,2x,i2,2x,i2,2x,i2)
                 do 350 i = 181,1,-1
                   at = 91 - i
                   write(3,348) at,rdco(1,i)
348                format (1x,f5.1,2x,f5.3)
350              continue
             else
                 write(3,352) kpn1,idim,ltype2,ptype2,stype
352              format (1x,i4,2x,i2,2x,i2,2x,i2,2x,i2)
                 do 356 i = 181,1,-1
                   at = 91 - i
                   write(3,354) at,rdco(3,i)
354                format (1x,f5.1,2x,f5.3)
356              continue
                 write(3,358) kpn1,idim,ltype1,ptype2,stype
358              format (1x,i4,2x,i2,2x,i2,2x,i2,2x,i2)
                 do 362 i = 181,1,-1
                   at = 91 - i
                   write(3,360) at,rdco(1,i)
360                format (1x,f5.1,2x,f5.3)
362              continue
             endif
c    write exp. dco ratio and errors
                write(3,364) kpn1,idim,ltype3,ptype2
364             format (1x,i4,2x,i2,2x,i2,2x,i2)
                do 368 i = 1,181
                  idel = 91 - i
                  write(3,366) idel,yy(4,i)
366               format (1x,i5,2x,f5.2)
368             continue
                write(3,370) kpn1,idim,ltype3,ptype2
370             format (1x,i4,2x,i2,2x,i2,2x,i2,2x,i2)
                do 374 i = 1,181
                  idel = 91 - i
                  write(3,372) idel,yy(5,i)
372               format (1x,i5,2x,f5.2)
374             continue
                write(3,376) kpn1,idim,ltype1,ptype2
376             format (1x,i4,2x,i2,2x,i2,2x,i2)
                do 380 i = 1,181
                  idel = 91 - i
                  write(3,378) idel,yy(3,i)
378               format (1x,i5,2x,f5.2)
380             continue
              close(3)
              write (6,*)' Output file written to FOR003.DAT'                
           endif          
        WRITE (6,*)' DO YOU WANT TO PLOT ANOTHER CASE?'
        READ(5,'(A1)')ANS
        IF ((ANS.EQ.'Y').OR.(ANS.EQ.'y')) GO TO 201
C
	WRITE(4,300)AI1(1),AI1(2),AI1(3),AI2,SIGMA,AI3,AI4,DEL34
300	FORMAT(/' INITIAL SPINS =',F4.1,',',f4.1,',',F4.1
     +  ,/,' FINAL SPIN =',F4.1
     +  ,/,' WIDTH OF GAUSSIAN SUBSTATE DIST. =',F4.1
     +  ,/,' INITIAL, FINAL SPIN , MIXING RATIO FOR GATE =',F4.1,',',
     +  f4.1,F10.5)
	WRITE(4,301)THETA(1),THETA(2)
301	FORMAT(/' DETECTOR ANGLE #1 =',F5.1,'  # 2 =',F5.1)
	WRITE(4,308)
308 	FORMAT(/'  I1, B0(I1), B2(I1), B4(I1)')
	DO 314 K=1,3
314	WRITE(4,309)AI1(K),BLAM1(K,1),BLAM1(K,2),BLAM1(K,3)
309	FORMAT( F4.1,3F8.4)
C	WRITE (4,310)
C310	FORMAT(/'  I2  I1 LAM2 LAM1  LAM    FG11     FG12
C     +      FG22')
C	DO 315 K=1,3
C	DO 315 I=1,19
C	 WRITE(4,320)AI2,AI1(K),ALAM2(I),ALAM1(I),ALAM(I),
C     +   FG11(K,I),FG12(K,I),FG22(K,I)
C315	CONTINUE
C320	FORMAT( 5F4.1,3X,3F8.4)
C	WRITE(4,322)
C322	FORMAT(/'  I4 I3 LAM2    FO11   FO12    FO22')
C	DO 316 I=1,19
C316	 WRITE(4,321)AI4,AI3,ALAM2(I),FO11(I),FO12(I),FO22(I)
C321 	FORMAT(3F4.1,3F8.4)
C	WRITE(4,325)
C325	FORMAT(/' LAM1 LAM LAM2 THETA1 THETA2 PHI12   XFAC')
C	DO 317 N=1,NPAIRS
C	 THETA1=THETA(IP(N))
C	 THETA2=THETA(JP(N))
C	 PHI12=PHI(IDET1,JDET2)
C	 DO 317 I=1,19
C	  XF(I)=XFAC(ALAM1(I),ALAM2(I),Q(I),THETA1,THETA2,PHI12)
C	  WRITE(4,326)ALAM1(I),ALAM2(I),Q(I),THETA1,THETA2,PHI12,
C     +    XF(I)
C317	CONTINUE
C326	FORMAT( 3F4.1,3X,3F6.1,F8.4)
	WRITE (4,303)
303	FORMAT(/'ARCTAN(DELTA)  RDCO(DELTA)')
	DO 304 I=1,181
	 IDEL=91-I
	 WRITE (4,305)IDEL,RDCO(1,I),RDCO(2,I),RDCO(3,I)
304	CONTINUE 
305 	FORMAT(I5,7X,F5.2,7X,F5.2,7X,F5.2)
	STOP
	END
C
C	**************************************************************
C
	SUBROUTINE WDCO(K,I,THETA1,THETA2,PHI12,W)
C
C	This calculates the general DCO function W(THETA1,THETAn,PHI)
C	as defined by eq. 15.6 of Hamilton, page 713.  Where THETA1 is
C	the polar angle of the Gamma ray of interest, THETAn is the 
C	polar angle of the gate, and PHI is the azimuthal angle 
C	between the two radiations.  The DCO function will be 
C	calculated as a function of the mixing ratio of gamma 1.
C
C
	DIMENSION W(181)
	COMMON/ALAMB/ALAM(3),ALAM1(19),ALAM2(19),Q(19),BLAM1(3,3)
	COMMON/ANGLE/RAD,PI
	COMMON/ALMIX/DEL34,ALI(3),ALIP(3),ALG,ALGP
	COMMON/ECOEF/EFAC(19)
	COMMON/FCOEFS/FO11(19),FO12(19),FO22(19),
     +  FG11(3,3,19),FG12(3,3,19),FG22(3,3,19)
	COMMON/X/XF(19)
C
	W(I)=0.0
	DO 55 J=1,19
	 ALM1=ALAM1(J)
	 ALM2=ALAM2(J)
	 QP=Q(J)
	 XF(J)=XFAC(ALM1,ALM2,QP,THETA1,THETA2,PHI12)
55	CONTINUE
C
	WT=0.0
	IF (ALI(K).EQ.1.0) THEN
	 ID=I-91
	 DI=FLOAT(ID)
	 ANG=RAD*DI
	 DEL=TAN(ANG)
	 DELSQ=DEL*DEL
	 DEN=1.0+DELSQ
	 DO 70 J=1,19
	  ALM1=ALAM1(J)
	  ALM2=ALAM2(J)
	  QP=Q(J)
	  PHSE=ALM1+ALM2-QP
	  INTPHSE=INT(PHSE)
	  PHSEFAC=(-1)**INTPHSE
	  PHSEFAC=PHSEFAC/HAT(ALM2)
	  WFAC=PHSEFAC*EFAC(J)*XF(J)
C	WRITE (6,*)'PHSEFAC,EFAC,XF=',PHSEFAC,EFAC(J),XF(J)
	  WTERM=0.0
	  DO 71 N=1,3
C	WRITE (6,*)' FG11,FG12,FG22=',FG11(K,N,J),FG12(K,N,J),FG22(K,N,J)
	   AGFAC=(FG11(K,N,J)+2.*DEL*FG12(K,N,J)+DELSQ*FG22(K,N,J))/DEN
	   ALM=ALAM(N)
	   FAC3J=THREEJ(ALM2,ALM1,ALM,-QP,QP,0.0)
	   WTERM=WTERM+AGFAC*FAC3J*BLAM1(K,N)
C	WRITE (6,*)' AGFAC,FAC3J,BLAM1=',AGFAC,FAC3J,BLAM1(K,N)
71	  CONTINUE
	  WT=WT+WTERM*WFAC
70	 CONTINUE
	 W(I)=WT
	ELSE
	 DO 80 J=1,19
	  ALM1=ALAM1(J)
	  ALM2=ALAM2(J)
	  QP=Q(J)
	  PHSE=ALM1+ALM2-QP
	  INTPHSE=INT(PHSE)
	  PHSEFAC=(-1)**INTPHSE
	  PHSEFAC=PHSEFAC/HAT(ALM2)
	  WFAC=PHSEFAC*EFAC(J)*XF(J)
	  WTERM=0.0
	  DO 81 N=1,3
	   AGFAC=FG22(K,N,J)
	   ALM=ALAM(N)
	   FAC3J=THREEJ(ALM2,ALM1,ALM,-QP,QP,0.0)
	   WTERM=WTERM+AGFAC*FAC3J*BLAM1(K,N)
81	  CONTINUE
	  WT=WT+WTERM*WFAC
80	 CONTINUE
	 W(I)=WT
	ENDIF
	RETURN
	END
C
C	*******************************************************
C
	BLOCK DATA ALAMBDA
	COMMON/ALAMB/ALAM(3),ALAM1(19),ALAM2(19),Q(19),BLAM1(3,3)
	COMMON/DETEC/AD(10),BD(10),CD(10),DD(10)
	DATA ALAM1/ 0.,2.,4.,0.,0.,2.,2.,2.,2.,4.,4.,4.,4.,4.,2.,2.,
     +           4.,4.,4./
     +	  ALAM2/ 0.,0.,0.,2.,4.,2.,2.,4.,4.,2.,2.,4.,4.,4.,2.,4.,2.,
     +            4.,4./
     +       Q/ 0.,0.,0.,0.,0.,0.,2.,0.,2.,0.,2.,0.,2.,4.,1.,1.,1.,
     +         1.,3./
     +          AD/3.968,3.658,3.753,0.,0.,0.,0.,0.,0.,0./
     +          BD/-2.103,-1.799,-1.924,0.,0.,0.,0.,0.,0.,0./
     +          CD/.2196,.1480,.1808,0.,0.,0.,0.,0.,0.,0./
     +     DD/-.5459E+04,-.5475E+04,-.4114E+04,0.,0.,0.,0.,0.,0.,0./
     +    ALAM/ 0.,2.,4./
	END
C
C	**************************************************************
C
	SUBROUTINE BLAM(AI1,SIGMA)
	DIMENSION A(6),BK1(6),AI1(3)
	COMMON/ALAMB/ALAM(3),ALAM1(19),ALAM2(19),Q(19),BLAM1(3,3)
	DO 5 K=1,3
	BLAM1(K,1)=1.0
	JA2=2.0*AI1(K)
	SIGSQ=SIGMA**2
	JA4=2.0*JA2
	SUM1=0.0
	DO 10 M=0,JA4,2
	 AM1=0.5*(M-JA2)
	 AMSQ=AM1**2
	 X=-(AMSQ/(2*SIGSQ))
	 EX=EXP(X)
	 SUM1=SUM1+EX
10	CONTINUE
	CN1=1.0/SUM1
	A(1)=AI1(K)
	A(2)=AI1(K)
	A(6)=0.0
	SFAC=HAT(AI1(K))
	DO 20 IK=2,4,2
	 A(3)=IK
	 BK1(IK)=0.0
	 DO 30 M=0,JA4,2
	  AM1=0.5*(M-JA2)
	  AMSQ=AM1**2
	  X=-(AMSQ/(2.0*SIGSQ))
	  I=AI1(K)-AM1
	  A(4)=AM1
	  A(5)=-AM1
	  C=CLEB(A)
	  EX=EXP(X)
	  TERM=CN1*EX*((-1)**I)*SFAC*C
	  BK1(IK)=BK1(IK)+TERM
30	 CONTINUE
20	CONTINUE
	BLAM1(K,2)=BK1(2)
	BLAM1(K,3)=BK1(4)
5	CONTINUE
	RETURN
	END
C
C	*****************************************************************
C
	FUNCTION ULAM2(AII,AIF,ALM2,DELIF)
	ULAM2=1.0
	IF(ALM2.EQ.0.0)THEN
	 ULAM2=1.0
	 RETURN
	ENDIF
	FA=HAT(AII)*HAT(AIF)
	IF((AII-AIF).EQ.2.0)THEN
	 AL=2.0
	 IPH=INT(AII+AIF+ALM2+AL)
	 FAC=FA*(-1)**IPH
	 FAC6J=SIXJ(AII,AII,ALM2,AIF,AIF,AL)
	 ULAM2=FAC*FAC6J
	ELSE
	 AL=AII-AIF
	 ALP=AL+1
	 IPH=INT(AII+AIF+ALM2+AL)
	 IPHP=IPH+1
	 FAC=FA*(-1)**IPH
	 FACP=FA*(-1)**IPHP
	 FAC6J=SIXJ(AII,AII,ALM2,AIF,AIF,AL)
	 FAC6JP=SIXJ(AII,AII,ALM2,AIF,AIF,ALP)
	 U=FAC*FAC6J
	 UP=FACP*FAC6JP
	 ULAM2=(U+DELIF*DELIF*UP)/(1.0+DELIF*DELIF)
	ENDIF
	RETURN
	END
C
C	*****************************************************************
C	
	FUNCTION FG(ALM1,ALM,ALM2,AL,ALPRIME,AI2,AI1)
	CONSTG=HAT(AI1)*HAT(AI2)*HAT(AL)*HAT(ALPRIME)*HAT(ALM)*
     +  HAT(ALM1)*HAT(ALM2)
	ICG=ALPRIME+ALM1+ALM2+1
	PHG=(-1)**ICG
	A3JG=THREEJ(AL,ALPRIME,ALM,1.,-1.,0.)
	A9JG=WINEJ(AI2,AL,AI1,AI2,ALPRIME,AI1,ALM2,ALM,ALM1)
	FG=CONSTG*PHG*A3JG*A9JG
	RETURN
	END
C	
C	*******************************************************
C
	FUNCTION FO(ALM2,ALO,ALOP,AI4,AI3)
	ICO=AI4+AI3-1
	PHO=(-1)**ICO
	CONSTO=HAT(ALO)*HAT(ALOP)*HAT(ALM2)*HAT(AI3)
	A3JO=THREEJ(ALO,ALOP,ALM2,1.,-1.,0.)
	A6JO=SIXJ(ALO,ALOP,ALM2,AI3,AI3,AI4)
	FO=PHO*CONSTO*A3JO*A6JO
	RETURN
	END
C
C	*******************************************************
C
	FUNCTION HAT(AI)
	HAT=SQRT(2*AI+1)
	RETURN
	END
C
C	*******************************************************
C
	FUNCTION CLB(RJ1,RM1,RJ2,RM2,RJ,RM)
	DIMENSION A(6)
	A(1)=RJ1
	A(2)=RJ2
	A(3)=RJ
	A(4)=RM1
	A(5)=RM2
	A(6)=RM
	CLB=CLEB(A)
	RETURN
	END
C
C	*******************************************************
C
	FUNCTION THREEJ(RJ1,RJ2,RJ,RM1,RM2,RM)
	DIMENSION A(6)
	 A(1)=RJ1
	 A(2)=RJ2
	 A(3)=RJ
	 A(4)=RM1
	 A(5)=RM2
	 A(6)=RM
C	DO 10 I=1,6
C10	WRITE (6,*)'A(',I,')=',A(I)
	 RCG=CLEB(A)
	 R3J=RCG/SQRT(2.*RJ+1)
	 S=RJ1-RJ2-RM
	 IS=NINT(S)
	 THREEJ=R3J*PHASEF(IS)
	RETURN
	END
C
C	*************************************************
C
	FUNCTION SIXJ(RJ1,RJ2,RJ12,RJ3,RJ,RJ23)
	DIMENSION A(6)
	A(1)=RJ1
	A(2)=RJ2
	A(3)=RJ
	A(4)=RJ3
	A(5)=RJ12
	A(6)=RJ23
	RR=RACAH(A)
	S=RJ1+RJ2+RJ3+RJ
	IS=IFIX(S+.001)
	IIS=(IS-(IS/2)*2)*(-2)+1
	S=FLOAT(IIS)
	SIXJ=RR*S
	RETURN
	END    
C
C 	*************************************************
C
	FUNCTION WINEJ(R1,R2,R3,R4,R5,R6,R7,R8,R9)
	J1=IFIX(2.*R1+.001)
	J2=IFIX(2.*R2+.001)
	J3=IFIX(2.*R3+.001)
	J4=IFIX(2.*R4+.001)
	J5=IFIX(2.*R5+.001)
	J6=IFIX(2.*R6+.001)
	J7=IFIX(2.*R7+.001)
	J8=IFIX(2.*R8+.001)
	J9=IFIX(2.*R9+.001)
C	WRITE (6,*,'R1,J1=',R1,J1
	WINEJ=0.0
	MUMIN=MAX(IABS(J1-J9),IABS(J2-J6),IABS(J4-J8))
	MUMAX=MIN(J1+J9,J2+J6,J4+J8)
	IF(MUMAX.LT.MUMIN) GO TO 40
	DO 20 MU=MUMIN,MUMAX,2
	RACA1=RACA(J1,J4,J9,J8,J7,MU)
	RACA2=RACA(J2,J5,MU,J4,J8,J6)
	RACA3=RACA(J9,MU,J3,J2,J1,J6)
C	WRITE (6,*,'RACA1-3=',RACA1,RACA2,RACA3
	 PROD=FLOAT(MU+1)*RACA1*RACA2*RACA3
	WINEJ=WINEJ+PROD
20	CONTINUE
	WINEJ=WINEJ*PHASEF((J1+J3+J5+J8)/2+J2+J4+J9)
40	RETURN
	END
C
C	**************************************************
C
	FUNCTION RACA(J1,J2,J,J3,J12,J23)
	DIMENSION A(6)
C	WRITE (6,*)'J1-J23=',J1,J2,J,J3,J12,J23
	AJ1=FLOAT(J1)
	AJ2=FLOAT(J2)
	AJ=FLOAT(J)
	AJ3=FLOAT(J3)
	AJ12=FLOAT(J12)
	AJ23=FLOAT(J23)
	A(1)=AJ1/2
	A(2)=AJ2/2
	A(3)=AJ/2
	A(4)=AJ3/2
	A(5)=AJ12/2
	A(6)=AJ23/2
C	DO 10 I=1,6
C10	WRITE (6,*)'A(',I,')=',A(I)
	RACA=RACAH(A)
	RETURN
	END
C
C	**********************************************
C
	FUNCTION PHASEF(N)
	PHASEF=FLOAT(1-2*IABS(N-2*(N/2)))
	RETURN
	END
C
C
C	**************************************************
C
	FUNCTION PHAS(N)
	PHAS=FLOAT(1-2*IABS(N-(N/2)*2))
	RETURN
	END
C
C	*********************************************************
C
	FUNCTION XFAC(ALM1,ALM2,QP,THETA1,THETA2,PHI)
	COMMON/DATA/FACLOG(170)
	COMMON/ANGLE/RAD,PI
	XFAC=0.0
	HLAM1=HAT(ALM1)
	HLAM2=HAT(ALM2)
	IA1=INT(ALM1-QP)
	IA2=INT(ALM2-QP)
	IA3=INT(ALM1+QP)
	IA4=INT(ALM2+QP)
	AF1=FACLOG(IA1+1)
	AF2=FACLOG(IA2+1)
	AF3=FACLOG(IA3+1)
	AF4=FACLOG(IA4+1)
	F1=EXP(AF1)
	F2=EXP(AF2)
	F3=EXP(AF3)
	F4=EXP(AF4)
	FAC=(F1*F2)/(F3*F4)
C	WRITE (6,*)' IA1,IA2,IA3,IA4,F1,F2,F3,F4=',IA1,IA2,IA3,IA4,F1,F2,F3,F4
	CONST=HLAM1*HLAM2*FAC
	PHIR=PHI*RAD
	CS=COS(QP*PHIR)
	PLEG1=P(ALM1,QP,THETA1)
	PLEG2=P(ALM2,QP,THETA2)
C	WRITE (6,*)'PLEG1,PLEG2=',PLEG1,PLEG2
	XFAC=CONST*PLEG1*PLEG2*CS
	RETURN
	END
C
C	*********************************************************
C
	FUNCTION P(AL,QP,THETA)
	COMMON/ANGLE/RAD,PI
	P=0.0
	THETAR=THETA*RAD
	SS=SIN(THETAR)
	CS=COS(THETAR)
	IF(AL.EQ.0.)THEN
	 P=1.0
	ELSE IF(AL.EQ.2.)THEN
	 IF (QP.EQ.0.)THEN
	  P=0.5*(3.0*CS*CS-1.)
	 ELSE IF(QP.EQ.1.)THEN
	  P=3.0*CS*SS
	 ELSE IF(QP.EQ.2.)THEN
	  P=3.0*SS*SS
	 ENDIF
	ELSE IF(AL.EQ.4.)THEN
	 IF(QP.EQ.0.)THEN
	  P=.125*(35.0*CS*CS*CS*CS-30.0*CS*CS+3.0)
	 ELSE IF(QP.EQ.1.)THEN
	  P=2.5*(7.0*CS*CS*CS-3.0*CS)*SS
	 ELSE IF(QP.EQ.2.)THEN
	  P=7.5*(7.0*CS*CS-1.0)*SS*SS
	 ELSE IF(QP.EQ.3.)THEN
	  P=105.0*CS*SS*SS*SS
	 ELSE IF(QP.EQ.4.)THEN
	  P=105.0*SS*SS*SS*SS
	 ENDIF	
	ENDIF
	RETURN
	END
C
C	*********************************************************
C


