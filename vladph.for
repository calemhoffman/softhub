      IMPLICIT REAL*8(A-H,O-Z)
      COMMON 
c     *       /COUNT/DC(250),NCP(250)
     *       /SAMS/PCENTR(250,3),PSL(250,2)
c     *       /BLNPF/BLNPF(250,2)
     *       /FBND/EIGF(5000)
     *       /FSCAT/RESC(5000)
      DIMENSION WAVES(5000,2)
      DATA DMH/0.04823D0/,DME/.06939D0/
c      OPEN(7,FILE='OUT.SOGL',ACCESS='SEQUENTIAL',STATUS='OLD')
      OPEN(8,FILE='vladph.out',ACCESS='SEQUENTIAL',STATUS='UNKNOWN')
      open(9,file='inputph.dat',access='sequential',status='old')
      read(9,*) AP
      read(9,*) ZP
      read(9,*) AT
      read(9,*) ZT
      read(9,*) PRM
      if(dabs(PRM).lt.1d-7) PRM=AP*AT/(AP+AT)
c *** for exact calculations factor of reduced mass should be given in
c *** inputvl.dat file as M_p*M_t/(M_p+M_t), where all masses are in MeV,
c *** divided by 938.92 MeV (i.e. by the average nucleon mass used in the
c *** code corresponding to the factor DMH = 0.04823) 
      read(9,*) Uws
      read(9,*) rows
      read(9,*) difws
      read(9,*) Uso
      read(9,*) roso
      read(9,*) difso
      read(9,*) rocul
      read(9,*) rlorb
      read(9,*) rjorb
      read(9,*) Emin
      read(9,*) Emax
      read(9,*) henerg
c *** 'Emin' and 'Emax' give an interval of the scattering energy in which 
c *** the calculations are to be done with step of 'henerg'.
      read(9,*) ibind
      read(9,*) Eleft
      read(9,*) Eright 
c *** if ibind=1 then the discrete level with given lj
c *** (i.e. with 'rlorb' and 'rjorb') is to be found (if any)
c *** within an energy interval Eleft to Eright 
      read(9,*) iprint
c *** if iprint =1 then print potentialsc------------------------------------------------------------------------
C             _____ Preparation of the potential _______

       RWS=rows*(AT+AP)**(1.d0/3.d0)
       RSO=roso*(AT+AP)**(1.d0/3.d0)
       RCU=rocul*(AT+AP)**(1.d0/3.d0)
       npot=250
       hr=0.1d0
       pilam2=2.d0
c  pion Compton wavelength squared pilam2=(hbar/m_pi*c)**2 = 2 fm**2 
       USOC=-Uso*pilam2/difso
       esq=DME/DMH
       ZAV=ZP*ZT
       VCUL1=0.5d0*ZAV*esq/RCU
       VCUL2=ZAV*esq
       do 91 k=1,npot
       r=hr*k 
       PCENTR(k,2)=Uws/(1.d0+dexp((r-RWS)/difws))
       expso=dexp((r-RSO)/difso)
       PSL(k,2)=(USOC/r)*expso/(1.d0+expso)**2
       if(r.le.RCU) PCENTR(k,3)=VCUL1*(3.d0-(r/RCU)**2)
       if(r.gt.RCU) PCENTR(k,3)=VCUL2/r
   91  continue
       if(iprint.ne.1) go to 102
       write(8,98)
 98    format('# radius [fm]   Central    Coulomb   s-o      ll')
       do 101 k=1,200
       r=hr*k
       write(8,100) r,PCENTR(k,2)/10.0,PCENTR(k,3)/10.0,PSL(k,2)/10.0
  100  format(2x,f7.2,1p,3F15.7)
c      3d15.7)
  101  continue
       write(8,99)
 99    format(2x)
  102  continue
c      READ(7,1) (DC(K),K=1,250)
c      READ(7,2) (NCP(I),I=1,250)
c      READ(7,1) ((PCENTR(I,K),I=1,250),K=1,3),
c     *             ((PSL(I,K),I=1,250),K=1,2)
c      READ(7,1) ((BLNPF(I,K),I=1,250),K=1,2)
c      CLOSE(7)
c    1 FORMAT(2D30.20)
c    2 FORMAT(8I10)
c      AMU=931.49432D0
c      ALFA=1.D0/137.036D0
c      PROTM=938.272D0
      PI=DACOS(-1.D0)
      NR=5000
      IT=2
      RJ=rjorb
      RL=rlorb
      REDMAS=PRM
      KEY=ibind
      EL=Eleft
      ER=Eright
      NE=(Emax-Emin)/henerg+1
c *** calculations of wave functions, Ebind, phases and amplitudes
            DO 4 K=1,NE
      E=Emin+henerg*(K-1)
      CALL PREPWF(IT,ZAV,RJ,RL,EL,ER,E,PHASE,REDMAS,KEY)
      IF(KEY.EQ.1) THEN
      DO 111 I=1,NR
      WAVES(I,1)=EIGF(I)
      WAVES(I,2)=RESC(I)
  111 CONTINUE
              ENDIF
      if(k.eq.1) then 
      ampl0=RESC(10)
      write(8,12)

   12 format('#Ecm (MeV)',2x,'phase (degrees, radians)',2x,
     *'relative amplitude at 1 fm',2x,'Cross section')
      endif

      Phasrd=PHASE*PI/180.d0
      xhBarC = 197.3269631
      xNmass = 939.565346
      xamu = 931.494028
      xfragMass = 16.d0
      xredMass = (((xNmass / xamu)* xfragMass)/(xNmass/xamu + 
     * xfragMass))*xamu
      xkSqr = (2.d0 * xredMass * E) / (xhBarC *xhBarC) 
c     Units of fm^2 for the total cross section below
      xsec = ((4 * 3.14) / xkSqr) * sin(Phasrd)*sin(Phasrd)
      write(8,11) E/10.0,PHASE/10.0,Phasrd/10.0,xsec/10.0
   11 format(2x,1p,4f15.7)
      KEY=0
    4 continue
      STOP
      END
      SUBROUTINE PREPWF(IT,ZAV,RJE,RLE,EL,ER,EE,PHASE,REDMAS,KEY)
      IMPLICIT REAL*8(A-H,O-Z)
C Calculates single-particle wave fuction and the energy 
C of the bound level within EL to ER in the mean field (if KEY=1)
C given by PT(250,k=2,3) and SL(250,k=2) (IT=2 - fixed)
C      /FBND/EIGF(5000) (from 0.1 to 500 fm)
C or scattrering wave function with RJE,RLE,EE quantum numbers
C      /FSCAT/FSCT(5000) (from 0.1 to 500 fm)
C             normalized to delta(e-e')
C*** BEFORE CALLING THIS SUBROUTINE THE POTENTIALS SHOULD BE
C*** IN THE COMMON BLOCK /SAMS/
      COMMON
     *      /SAMS/PT(250,3),SL(250,2)
c     *      /BLNPF/BL(250,2)
c     *      /COUNT/DC(250),NCP(250)
      COMMON
     *      /PARFP/AA,ZZ,T,V1,V2,ROO,ROC,DIF,RLAM,RJ,RL
     *      /FORFP/RN,RC,VO,VCO,VCD,RL1,XL,EX
     *      /FBND/EIGF(5000)
     *      /FSCAT/FSCT(5000)
     *      /PRMAS/PRM
     *      /SVO/SD(15,2)
c      PI=DACOS(-1.D0)
c      AA=DC(69)
c      ZZ=DC(70)
C      PRM=1.D0
      AA=777.d0
      ZZ=ZAV 
      PRM=REDMAS 
C*** SD(I,K): I=s1/2,p1/2,p3/2,... K=n,p
      DO 8 I=1,15
      SD(I,1)=1.D0
      SD(I,2)=1.D0
    8 CONTINUE
c *** all SD-factors = 1 !!!     
c      ROO=1.24D0
c      ROC=1.24D0
c      A13=AA**(1.D0/3.D0)
c      RN=ROO*A13
c      RC=ROC*A13
      RJ=RJE
      RL=RLE
      T=IT*1.D0-1.5D0
      CALL PRPOT
      EXK1=7777777.d0
      QR=  7777777.d0
      if(KEY.ne.1) go to 3
      QL=WRODIS(EL,1)
      IF(ER.GE.0.D0) ER=-1.D-2
      QR=WRODIS(ER,1)
      EXK1=0.d0
      if(QR*QL.lt.0.d0) CALL KOREN(EL,QL,ER,QR,EXK1)
      if(EXK1.lt.-1.d-7) QR=WRODIS(EXK1,2)
      if(EXK1.gt.-1.d-10)QR=7777777.d0
C Bound-state wave function is obtained
      WRITE(8,2) RL,RJ,EXK1/10.0
c,QR/10.0
    2 FORMAT(2X,'L,J== ',2F8.2,/2X,' Ebind=',1P,F15.5/
     *10X)
c,'Wronskian=',F30.20)
c      write(8,99)
c 99   format(2X)
    3 CONTINUE
      CALL WFSCAT(EE,PHASE)
C Scattering wave function is obtained
      RETURN
      END
             SUBROUTINE PRPOT
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON /PARFP/A,Z,T,V1,V2,RO,ROC,DIF,RLAM,RJ,RL
     *       /FORFP/RN,RC,VO,VCO,VCD,RL1,XL,EX
     *       /FGP/FP1(250)
     *       /PRMAS/PRM
     *       /SVO/SD(15,2)
     *       /SAMS/PT2(250,3),S2(250,2)
      DATA DMH/0.04823D0/,DME/.06939D0/,H/.1D0/,N/250/
      IND=RL+RJ+1.D0
      IT =T+1.6D0
      RL1=RL*(RL+1.0D0)
      DMHPRM=DMH*PRM
      SP =DMHPRM
      IF(IND.LE.15) SP=SD(IND,IT)*DMHPRM
      XL =(RJ*(RJ+1.D0)-RL1-.75D0)*DMHPRM
      DO 7 K=1,N
      R=K*H
      FP1(K)=SP*PT2(K,IT)+XL*S2(K,IT)+RL1/(R*R)
    7 CONTINUE
      VCO=0.D0
      IF(IT.EQ.1) GO TO 4
      VCO=DME*PRM*Z
c *** here Z=ZAV=ZP*ZT when calling this 'prpot' subroutine
      DO 17 K=1,N
      FP1(K)=FP1(K)+DMHPRM*PT2(K,3)
   17 CONTINUE
    4 CONTINUE
      RETURN
      END
          DOUBLE PRECISION FUNCTION FPOTS(R)
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON /FORFP/RN,RC,VO,VCO,VCD,RL1,XL,EX
      FPOTS=RL1/(R*R)+VCO/R
      RETURN
      END
           DOUBLE PRECISION FUNCTION BESR(RL,W)
      IMPLICIT REAL*8(A-H,O-Z)
      DATA D / 1.D-7 /
      BESR = 1.D0
      B    = 1.D0
      AK   = 0.D0
    1 AK   = AK+1.D0
      B    = -B*W/(AK*(AK+RL+0.5D0))
      BESR = BESR+B
      IF (DABS(B).GT.D) GO TO 1
      RETURN
      END
           DOUBLE PRECISION FUNCTION WRODIS(E,IWORF)
C   if IWORF = 1 - wronskian is calculated only
C   if IWORF = 2 - normalized wave function is calculated also
C               (WRONSKIAN must be close to zero in this case)
C      the wave function is /FBND/FUN(5000) with step 0.1 fm
C   restriction: E<0 (if E>0 then WRONSK = 77777 for any IWORF)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION Y1(5000),Y2(5000)
      COMMON
     *       /PARFP/A,Z,T,V1,V2,RO,ROC,DIF,RLAM,RJ,RL
     *       /FGP/FP1(250)
     *       /PRMAS/PRM
     *       /FBND/FUN(5000)
      DATA RMAX1/500.D0/,NMAX/5000/
      H=0.1D0
      B11=H*H/12.D0
      B22=B11*10.D0
      P1=0.04823D0*PRM
      P2=0.06939D0*PRM
      WRODIS=77777.D0
      IF(E.GE.0.D0) RETURN
      E1=P1*E
      L1=RL+1.01D0
      C1=1.D-4/(10.D0**L1)
      C4=1.D-7/(10.D0**L1)
      L=L1
      IF(L1.GT.3) L=L1/2
      RK2=0.25D0*(E1-FP1(L)+RL*(RL+1.D0)/((L*H)**2))
      L=RL+0.1D0
      L2=L1+1
      L3=L2+1
      DO 1 K=1,L2
      R=K*H
      Y1(K)=C1*R**L1*BESR(RL,RK2*R*R)
    1 CONTINUE
      F1=FP1(L1)-E1
      F2=FP1(L2)-E1
      DO 3 K=L3,52
      F3=FP1(K)-E1
      Y1(K)=((2.D0+B22*F2)*Y1(K-1)-
     *(1.D0-B11*F1)*Y1(K-2))/(1.D0-B11*F3)
      F1=F2
      F2=F3
    3 CONTINUE
C  Y1 is calculated from 0.1 to 5.2 fm (H=0.1 fm)
C****************************************************
C  Now calculate Y2 from some R   to R=24.9 fm (H=0.1 fm)
C                              MAX
C*********************************************************
      RK=DSQRT(-E1)
      RM=RL+0.5D0
      ETA=(T+0.5D0)*P2*0.5D0*Z/RK
      ETA1=ETA+0.5D0
      R=2.D0*(RM*RM-ETA1*ETA1)/RK
      R=DABS(R)
      IF(R.LT.RMAX1) R=RMAX1
      IF(R.GT.RMAX1) THEN
      RTU=R
      R=3.D0*RMAX1
      PRINT 73,R,RTU
   73 FORMAT(2X,31H***WRODIS CALCULATES Y2 FROM R=,1P,E11.3,
     *14H INSTEAD OF R=,E11.3,8H FM  FOR)
      PRINT 11,A,Z,T,E,RK,ETA,RL,RJ
   11 FORMAT(2X,'A,Z,T===',3F5.1,/2x,' E,K,ETA===',1P,3E11.3,
     *                                   ' L,J==',0P,2F5.1)
              ENDIF
      N=R/H+4
      RZ1=2.D0*RK*N*H
      RZ2=2.D0*RK*(N+1)*H
      XXX=1.D0+1.D0/N
      FY2=DEXP(RK*H+ETA*DLOG(XXX))*
     *FW(ETA,RM,RZ1)/FW(ETA,RM,RZ2)
      F1=FPOTS((N+1)*H)-E1
      F2=FPOTS(N*H)-E1
      N1=N-249
      DO 18 K=1,N1
      I=N-K
      F3=FPOTS(I*H)-E1
      IF(I.LE.250) F3=FP1(I)-E1
      FY3=((2.D0+F2*B22)*FY2-(1.D0-F1*B11))/(1.D0-F3*B11)
      FY2=FY3/FY2
      IF(I.LE.NMAX) FUN(I)=FY2
      F1=F2
      F2=F3
   18 CONTINUE
      Y2(250)=C4
      Y2(249)=FY2*C4
      F1=FP1(250)-E1
      F2=FP1(249)-E1
      DO 7 K=1,201
      KT=249-K
      F3=FP1(KT)-E1
      Y2(KT)=((2.D0+B22*F2)*Y2(KT+1)-(1.D0-B11*F1)*
     *                     Y2(KT+2))/(1.D0-B11*F3)
      F1=F2
      F2=F3
    7 CONTINUE
C
C  Y2 is calculated from 25.0 to 4.8 fm (H=0.1 fm)
C
      W=(Y1(50)*(8.D0*(Y2(51)-Y2(49))-(Y2(52)-Y2(48)))-
     *Y2(50)*(8.D0*(Y1(51)-Y1(49))-(Y1(52)-Y1(48))))/1.2D0
      WRODIS=W
      IF(IWORF.EQ.1) RETURN
      CNORM=Y1(50)/Y2(50)
      DO 554 K=1,50
      Y2(K)=Y1(K)**2
  554 CONTINUE
      DO 555 K=51,250
      Y1(K)=Y2(K)*CNORM
      Y2(K)=Y1(K)**2
  555 CONTINUE
      DO 557 K=251,NMAX
      Y1(K)=Y1(K-1)/FUN(K-1)
      Y2(K)=Y1(K)**2
  557 CONTINUE
      CALL SINT(Y2,NMAX)
      CNORM=1.D0/DSQRT(Y2(NMAX)*H)
      DO 556 K=1,NMAX
      R=H*K
      FUN(K)=Y1(K)*CNORM/R
  556 CONTINUE
      RETURN
      END
             SUBROUTINE KOREN(E1,QL,E2,QR,E5)
C  Search for zero of wronskian
      IMPLICIT REAL*8(A-H,O-Z)
      DATA XX/1.D0/,EPS/1.D-10/,EX/1.D-20/
      EL=E1
      ER=E2
      Q1=QL
      Q2=QR
      E5=E1
      IF(DABS(Q1).LT.EX) RETURN
      E5=E2
      IF(DABS(Q2).LT.EX) RETURN
      X1=DSIGN(XX,Q1)
      X2=DSIGN(XX,Q2)
    4 E3=EL+(ER-EL)*DABS(Q1)/(DABS(Q1)+DABS(Q2))
      Q3=WRODIS(E3,1)
      E5=E3
      IF(DABS(Q3).LT.EX) RETURN
      X3=DSIGN(XX,Q3)
      IF(X3*X1.GT.0.D0) GO TO 1
      EM=(E3+EL)/2.D0
      Q4=WRODIS(EM,1)
      E5=EM
      IF(DABS(Q4).LT.EX) RETURN
      X4=DSIGN(XX,Q4)
      IF(X3*X4.GT.0.D0) GO TO 2
      IF(DABS(E3-EM).LE.EPS) GO TO 3
      EL=EM
      ER=E3
      Q1=Q4
      Q2=Q3
      X1=X4
      X2=X3
      GO TO 4
    2 ER=EM
      Q2=Q4
      X2=X4
      IF(DABS(ER-EL).LE.EPS) GO TO 6
      GO TO 4
    1 EM=(ER+E3)/2.D0
      Q5=WRODIS(EM,1)
      E5=EM
      IF(DABS(Q5).LT.EX) RETURN
      X5=DSIGN(XX,Q5)
      IF(X3*X5.GT.0.D0) GO TO 5
      IF(DABS(E3-EM).LE.EPS) GO TO 3
      EL=E3
      ER=EM
      Q2=Q5
      Q1=Q3
      X2=X5
      X1=X3
      GO TO 4
    5 EL=EM
      Q1=Q5
      X1=X5
      IF(DABS(ER-EL).LE.EPS) GO TO 6
      GO TO 4
    3 E5=(E3+EM)/2.D0
      RETURN
    6 E5=(EL+ER)/2.D0
      RETURN
      END
            SUBROUTINE WFSCAT(E,PHASE)
C**********************************************************************
C Calculate scattering radial wave function, normalized
C to delta(e-e'), and phase shift PHASE (in degrees)
C the wave fuction is /FSCAT/FUN(5000) with step 0.1 fm
C**********************************************************************
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION F(51),FPR(51),G(51),GPR(51)
      COMMON
     *       /PARFP/A,Z,T,V1,V2,RO,ROC,DIF,RLAM,RJ,RL
     *       /FGP/FP1(250)
     *       /PRMAS/PRM
     *       /FSCAT/FUN(5000)
      DATA RMAX1/500.D0/,ACCUR/1.D-7/,STEP/100.D0/,NMAX/5000/
      PI=DACOS(-1.D0)
      H=0.1D0
      B11=H*H/12.D0
      B22=B11*10.D0
      P1=0.04823D0*PRM
      P2=0.06939D0*PRM
      E1=P1*E
      L1=RL+1.01D0
      L=L1
      IF(L1.GT.3) L=L1/2
      RK2=0.25D0*(E1-FP1(L)+RL*(RL+1.D0)/((L*H)**2))
      L2=L1+1
      L3=L2+1
      C1=1.D-4/(10.D0**L1)
      DO 1 K=1,L2
      R=K*H
      FUN(K)=C1*R**L1*BESR(RL,RK2*R*R)
    1 CONTINUE
      F1=FP1(L1)-E1
      F2=FP1(L2)-E1
      DO 3 K=L3,250
      F3=FP1(K)-E1
      FUN(K)=((2.D0+B22*F2)*FUN(K-1)-
     *(1.D0-B11*F1)*FUN(K-2))/(1.D0-B11*F3)
      F1=F2
      F2=F3
    3 CONTINUE
C
C FUN IS OBTAINED FROM 0.1 TO 25.0 FM (STEP H=0.1 FM)
C
      DO 178 K=251,NMAX
      F3=FPOTS(K*H)-E1
      FUN(K)=((2.D0+B22*F2)*FUN(K-1)-
     *(1.D0-B11*F1)*FUN(K-2))/(1.D0-B11*F3)
      F1=F2
      F2=F3
  178 CONTINUE
C
C FUN IS OBTAINED FROM 25.0 TO 500 FM (STEP H=0.1 FM)
C
      NM1=NMAX-1
      NM2=NMAX
      Y1S=FUN(NM1)
      Y2S=FUN(NM2)
      RK=DSQRT(E1)
      ETA=(T+0.5D0)*P2*0.5D0*Z/RK
      RTURN=(ETA+DSQRT(ETA*ETA+RL*(RL+1.D0)))/RK
      RM=RTURN+1.D0
      IF(RM.GT.RMAX1) THEN
      NM=RM/H+3.01D0
      NMAX1=NMAX+1
      DO 77 K=NMAX1,NM
      F3=FPOTS(K*H)-E1
      Y3S=((2.D0+B22*F2)*Y2S-
     *(1.D0-B11*F1)*Y1S)/(1.D0-B11*F3)
      F1=F2
      F2=F3
      Y1S=Y2S
      Y2S=Y3S
   77 CONTINUE
      NM1=NM-1
      NM2=NM
                    ENDIF
      L=RL+0.1D0
      ROG1=NM1*H*RK
      CALL RCWFN(ROG1,ETA,L,L,F,FPR,G,GPR,ACCUR,STEP)
      FC1=F(L+1)
      GC1=G(L+1)
      ROG2=NM2*H*RK
      CALL RCWFN(ROG2,ETA,L,L,F,FPR,G,GPR,ACCUR,STEP)
      FC2=F(L+1)
      GC2=G(L+1)
      D=FC1*GC2-FC2*GC1
      AC=(Y1S*GC2-Y2S*GC1)/D
      BC=(FC1*Y2S-FC2*Y1S)/D
      PHASE=ATAN2(BC,AC)
      PHASE=PHASE*180.D0/PI
      CC=DSQRT(AC*AC+BC*BC)
      CN=DSQRT(P1/(PI*RK))/CC
      DO 78 K=1,NMAX
      R=K*H
      FUN(K)=FUN(K)*CN/R
   78 CONTINUE
      RETURN
      END
            FUNCTION FW(ETA,RM,Z)
      IMPLICIT REAL*8(A-H,O-Z)
      DATA EPS/1.D-9/
      RM2=RM*RM
      FW=1.D0
      CNT=1.D0
      IF(DABS(ETA).LT.1.D-13) GO TO 3
      N=1
      GO TO 1
    2 FW=FW+CNT
      N=N+1
    1 X=ETA+N-0.5D0
      X2=X*X
      CN=(RM2-X2)/(N*Z)
      IF(DABS(CN).GE.1.D0) RETURN
      CNT=CNT*CN
      IF(DABS(CNT).LT.EPS) RETURN
      GO TO 2
    3 K=RM
      DO 4 N=1,K
      X=N-0.5D0
      X2=X*X
      CN=(RM2-X2)/(N*Z)
      CNT=CNT*CN
    4 FW=FW+CNT
      RETURN
      END
            SUBROUTINE YYN(RJ1,RJ2,RL1,RL2,RL)
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON /YY4/YY(4)
      YY(1)=0.D0
      YY(2)=0.D0
      YY(3)=0.D0
      YY(4)=0.D0
      X1=RL1-0.6D0
      X2=RL1+0.6D0
      X3=DABS(RL1-RL2)-0.1D0
      X4=DABS(RL1+RL2)+0.1D0
      IF(RJ1.LT.X1.OR.RJ1.GT.X2) GO TO 1
      X1=RL2-0.6D0
      X2=RL2+0.6D0
      IF(RJ2.LT.X1.OR.RJ2.GT.X2) GO TO 1
      X1=DABS(RJ1-RJ2)-0.1D0
      X2=RJ1+RJ2+0.1D0
      IF(RL.LT.X1.OR.RL.GT.X2) GO TO 1
      IF(RL) 1,2,3
    2 X1=DSQRT(2*RJ1+1)
      N=RL1+RL2+0.1D0
      IF(N-2*(N/2)) 4,5,4
    5 IF(X3.GT.0.D0) GO TO 1
      N=RL1+0.1D0
      IF(N-2*(N/2)) 6,7,6
    7 YY(1)=X1
      GO TO 1
    6 YY(1)=-X1
      GO TO 1
    4 IF(X3.GT.1.D0) GO TO 1
      N=RL2+1.1D0
      D=3.D0
      IF(N-2*(N/2)) 8,9,8
    9 YY(4)=X1/DSQRT(D)
      GO TO 1
    8 YY(4)=-X1/DSQRT(D)
      GO TO 1
    3 CONTINUE
      D=0.5D0
      D1=0.D0
      X1=CLEBSH(RJ1,RJ2,D,-D,RL,D1)
      A1=2*RJ1+1.D0
      A2=2*RJ2+1.D0
      A3=2*RL+1.D0
      A4=DSQRT(RL)
      A5=DSQRT(RL+1.D0)
      A9=DSQRT(A1*A2)/A3
      A6=1.D0
      N=RJ1+RJ2+0.1D0
      IF(N-2*(N/2)) 11,12,11
   11 A6=-1.D0
   12 A7=A6
      N=RL+0.1D0
      IF(N-2*(N/2)) 13,14,13
   13 A7=-A6
   14 X2=X1*(A7*A1+A2)/(2*A4*A5)
      N=RL1+RL2+RL+0.1D0
      IF(N-2*(N/2)) 15,16,15
   15 CONTINUE
      A8=1.D0
      N=RL2+RJ1+0.6D0
      IF(N-2*(N/2)) 17,18,17
   17 A8=-1.D0
   18 IF(RL-1.GT.X4.OR.RL-1.LT.X3) GO TO 22
      YY(2)=-A9/DSQRT(A3-2.D0)*(A4*X1*A8+A6*X2*A5)
   22 IF(RL+1.GT.X4.OR.RL+1.LT.X3) GO TO 1
      YY(4)=A9/DSQRT(A3+2.D0)*(A5*X1*A8-A6*X2*A4)
      GO TO 1
   16 CONTINUE
      N=RL1+RL+RJ1+1.6D0
      A8=1.D0
      IF(N-2*(N/2)) 19,21,19
   19 A8=-1.D0
   21 IF(RL.GT.X4.OR.RL.LT.X3) GO TO 1
      YY(1)=A8*A9*X1
      YY(3)=A6*A9*X2
    1 RETURN
      END
            FUNCTION CLEBSH(AJ,BJ,AM,BM,CJ,CM)
C                    CJ,CM
C                   C
C                    AJ,AM,BJ,BM
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON /FACTRL/F(100)
      DATA N/100/,K/0/,X/2.D0/
      IF(K)1,1,2
    1 K=1
      F(1)=0.D0
      F(2)=0.D0
      DO 3 I=3,N
      F(I)=F(I-1)+DLOG(X)
    3 X=X+1.D0
    2 I=AM+BM-CM+.1D0
      IF(I)100,4,100
    4 I1=AJ+BJ-CJ+1.1D0
      IF(I1)100,100,6
    6 I2=AJ-BJ+CJ+1.1D0
      IF(I2)100,100,7
    7 I3=BJ+CJ-AJ+1.1D0
      IF(I3)100,100,8
    8 X=AJ+BJ+CJ+2.1D0
      I4=X
      I=X+.6D0
      I=I4-I
      IF(I)100,5,100
    5 X=AJ+AM+1.1D0
      I5=X
      IF(I5)100,100,9
    9 I=X+.6D0
      I=I-I5
      IF(I)100,10,100
   10 I6=AJ-AM+1.1D0
      IF(I6)100,100,11
   11 X=BJ+BM+1.1D0
      I7=X
      IF(I7)100,100,12
   12 I=X+.6D0
      I=I-I7
      IF(I)100,13,100
   13 I8=BJ-BM+1.1D0
      IF(I8)100,100,14
   14 X=CJ+CM+1.1D0
      I9=X
      IF(I9)100,100,15
   15 I=X+.6D0
      I=I-I9
      IF(I)100,16,100
   16 I10=CJ-CM+1.1D0
      IF(I10)100,100,17
   17 X=F(I1)+F(I2)+F(I3)-F(I4)
      I=I5-I6
      IF(I)18,19,18
   19 I=I7-I8
      IF(I)18,200,18
   18 X=X+F(I5)+F(I6)+F(I7)+F(I8)+F(I9)+F(I10)
      X=X*.5D0
      I10=MIN0(I1,I6,I7)
      I2=I1-I5
      I3=I1-I8
      I9=MAX0(0,I2,I3)+1
      I1=I1+1
      I6=I6+1
      I7=I7+1
      I8=I9/2
      E=1.D0
      I5=I9*.5D0+.6D0
      I8=I8-I5
      IF(I8)20,21,20
   21 E=-1.D0
   20 S=0.D0
      DO 22 I=I9,I10
      C=X-F(I)-F(I1-I)-F(I6-I)-F(I7-I)-
     *F(I-I2)-F(I-I3)
      S=S+E*DEXP(C)
   22 E=1.D0-E-1.D0
      CLEBSH=DSQRT(CJ+CJ+1.D0)*S
      RETURN
  200 I=I4/2
      I5=I4*.5D0+.6D0
      I=I-I5
      IF(I)100,201,100
  201 I6=I5-I6+1
      I7=I5-I8+1
      I8=I5-I10+1
      S=X*0.5D0+F(I5)-F(I6)-F(I7)-F(I8)
      S=DEXP(S)
      I5=I8/2
      I6=I8*.5D0+.6D0
      I5=I5-I6
      IF(I5)202,203,202
  203 S=1.D0-S-1.D0
  202 CLEBSH=S*DSQRT(CJ+CJ+1.D0)
      RETURN
  100 CLEBSH=0.D0
      RETURN
      END
         SUBROUTINE RCWFN(RHO,ETA,MINL,MAXL,FC,FCP,GC,GCP,ACCUR,STEP)
      IMPLICIT REAL*8(A-H,O-Z)
      REAL*8 K,K1,K2,K3,K4,M1,M2,M3,M4
      DIMENSION FC(1),FCP(1),GC(1),GCP(1)
C *** COULOMB WAVEFUNCTIONS
C *** CONTINUED-FRACTION METOD OF STEED
C *** A.R.BARNETT ET AL. COMP.PHYS.COM.8,377(1974)
      VANYA=1.D65
      PACE=STEP
      ACC =ACCUR
      IF(PACE.LT.100.D0) PACE=100.D0
      IF(ACC.LT.1.D-15.OR.ACC.GT.1.D-6) ACC=1.D-6
      R   = RHO
      KTR = 1
      LMAX= MAXL
      LMIN1= MINL+1
      XLL1=DFLOAT(MINL*LMIN1)
      ETA2= ETA*ETA
      TURN= ETA+DSQRT(ETA2+XLL1)
      IF(R.LT.TURN.AND.DABS(ETA).GE.1.D-6) KTR=-1
      KTRP = KTR
      GO TO 2
    1 R    = TURN
      TF   = F
      TFP  = FP
      LMAX = MINL
      KTRP = 1
    2 ETAR = ETA*R
      RHO2 = R*R
      PL=DFLOAT(LMAX+1)
      PMX  = PL+0.5D0
C *** CONTINUED FRACTION FOR FP(MAXL)/F(MAXL)
      FP  = ETA/PL+PL/R
      DK  = ETAR*2.D0
      DEL = 0.D0
      D   = 0.D0
      F   = 1.D0
      K   = (PL*PL-PL+ETAR)*(2.D0*PL-1.D0)
      IF(PL*PL+PL+ETAR.NE.0.D0) GO TO 3
      R=R+1.D-6
      GO TO 2
    3 H   = (PL*PL+ETA2)*(1.D0-PL*PL)*RHO2
      K   = K+DK+6.D0*PL*PL
      D   = 1.D0/(D*H+K)
      DEL = DEL*(D*K-1.D0)
      IF(PL.LT.PMX) DEL=-R*(PL*PL+ETA2)*(PL+1.D0)*D/PL
      PL  = PL+1.D0
      FP=FP+DEL
      IF(D.LT.0.D0) F=-F
      IF(PL.GT.20000.D0) GO TO 11
      IF(DABS(DEL/FP).GE.ACC) GO TO 3
      FP=F*FP
      IF(LMAX.EQ.MINL) GO TO 5
      FC(LMAX+1)=F
      FCP(LMAX+1)=FP
C *** DOWNWARD RECURCION TO MINL FOR F AND FP
      L = LMAX
      DO 4 LP=LMIN1,LMAX
      PL=DFLOAT(L)
      GC(L+1)  =ETA/PL+PL/R
      GCP(L+1) =DSQRT(ETA2+PL*PL)/PL
      FC(L)   =(GC(L+1)*FC(L+1)+FCP(L+1))/GCP(L+1)
      FCP(L)   =GC(L+1)*FC(L)-GCP(L+1)*FC(L+1)
    4 L  = L-1
      F  = FC(LMIN1)
      FP = FCP(LMIN1)
    5 IF(KTRP.EQ.-1) GO TO 1
C *** REPEAT FOR R=TURN IF RHO LT TURN
C ***
C *** NOW OBTAIN P + I*Q FOR MINL FROM CONT. FRACTION (STEED)
      P = 0.D0
      Q = R-ETA
      PL= 0.D0
      AR= -(ETA2+XLL1)
      AI= ETA
      BR= 2.D0*Q
      BI= 2.D0
      WI= 2.D0*ETA
      DR= BR/(BR*BR+BI*BI)
      DI= -BI/(BR*BR+BI*BI)
      DP= -(AR*DI+AI*DR)
      DQ= (AR*DR-AI*DI)
    6 P = P+DP
      Q = Q+DQ
      PL= PL+2.D0
      AR= AR+PL
      AI= AI+WI
      BI= BI+2.D0
      D = AR*DR-AI*DI+BR
      DI= AI*DR+AR*DI+BI
      T = 1.D0/(D*D+DI*DI)
      DR= T*D
      DI= -T*DI
      H = BR*DR-BI*DI-1.D0
      K = BI*DR+BR*DI
      T = DP*H-DQ*K
      DQ= DP*K+DQ*H
      DP= T
      IF(PL.GT.46000.D0) GO TO 11
      IF(DABS(DP)+DABS(DQ).GE.(DABS(P)+DABS(Q))*ACC) GO TO 6
      P = P/R
      Q = Q/R
C *** SOLVE FOR FP,G,GP,AND NORMALISE F AT L=MINL
      G  = (FP-P*F)/Q
      GP = P*G-Q*F
      W  = 1.D0/DSQRT(FP*G-F*GP)
      G  = W*G
      GP = W*GP
      IF(KTR.EQ.1) GO TO 8
      F  = TF
      FP = TFP
      LMAX = MAXL
C *** RUNGE-KUTTA INTEGR. OF G(MINL) AND GP(MINL)
C *** INWARD FROM TURN (FOX AND MAYERS 1968 P. 202)
      IF(RHO.LT.0.2D0*TURN) PACE=999.D0
      R3=1.D0/3.D0
      H  = (RHO-TURN)/(PACE+1.D0)
      H2 = 0.5D0*H
      I2=PACE+0.001D0
      ETAH=ETA*H
      H2LL=H2*XLL1
      S   =(ETAH+H2LL/R)/R-H2
    7 RH2 = R+H2
      T   = (ETAH+H2LL/RH2)/RH2-H2
      K1 = H2*GP
      M1 = S*G
      K2 = H2*(GP+M1)
      M2 = T*(G+K1)
      K3 = H*(GP+M2)
      M3 = T*(G+K2)
      M3 = M3+M3
      K4 = H2*(GP+M3)
      RH = R+H
      S  = (ETAH+H2LL/RH)/RH-H2
      M4 = S*(G+K3)
      G  = G+(K1+K2+K2+K3+K4)*R3
      GP = GP+(M1+M2+M2+M3+M4)*R3
      IF(DABS(GP).GT.VANYA.OR.DABS(G).GT.VANYA) GO TO 11
      R  = RH
      I2 = I2-1
      IF(I2.GE.0) GO TO 7
      W  = 1.D0/(FP*G-F*GP)
C *** UPWARD RECURSION FROM GC(MINL) AND GCP(MINL)
C *** RENORMALISE FC,FCP FOR EACH L-VALUE
    8 GC(LMIN1)=G
      GCP(LMIN1)=GP
      IF(LMAX.EQ.MINL) GO TO 10
      DO 9 L=LMIN1,LMAX
      T = GC(L+1)
      GC(L+1) = (GC(L)*GC(L+1)-GCP(L))/GCP(L+1)
      GCP(L+1)= GC(L)*GCP(L+1)-GC(L+1)*T
      FC(L+1) = W*FC(L+1)
    9 FCP(L+1)= W*FCP(L+1)
      FC(LMIN1) = FC(LMIN1)*W
      FCP(LMIN1)= FCP(LMIN1)*W
      RETURN
   10 FC(LMIN1) = W*F
      FCP(LMIN1)= W*FP
      RETURN
   11 W = 0.D0
      G = 0.D0
      GP= 0.D0
      GO TO 8
      END
                 SUBROUTINE SINT(VP,N)
C   RESTRICTIONS: VP(0)=0, N=5 OR >5.
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION VP(N)
      DATA KL/1/
      N1=N-1
      N2=N-2
      N3=N-3
      IF(KL.EQ.2) GO TO 7
      C11=53.D0/360.D0
      C10=-11.D0/30.D0
      C11M=323.D0/360.D0
      C12M=251.D0/720.D0
      C12=-19.D0/720.D0
      C20=19.D0/30.D0
      C21=-37.D0/360.D0
      C21M=173.D0/360.D0
      C22=11.D0/720.D0
      KL=2
    7 CONTINUE
      V1=VP(1)
      V2=VP(2)
      V3=VP(3)
      V4=VP(4)
      V5=VP(5)
      VP(1)=C11M*V1+C10*V2+C11*V3+C12*V4
      VP(2)=VP(1)+C21M*V1+C20*V2+C21*V3+C22*V4
      DO 1 I=3,N2,2
      VP(I)=VP(I-1)+C12*V1+C21M*V2+C20*V3+C21*V4+C22*V5
      VP(I+1)=VP(I)+C22*V1+C21*V2+C20*V3+C21M*V4+C12*V5
      IF(I.EQ.N3) GO TO 2
      IF(I.EQ.N2) GO TO 3
      V1=V3
      V2=V4
      V3=V5
      V4=VP(I+3)
      V5=VP(I+4)
    1 CONTINUE
    2 V1=V2
      V2=V3
      V3=V4
      V4=V5
      V5=VP(N)
      VP(N1)=VP(N2)+C22*V1+C21*V2+C20*V3+C21M*V4+C12*V5
    3 VP(N)=VP(N1)+C12*V1+C11*V2+C10*V3+C11M*V4+C12M*V5
      RETURN
      END

