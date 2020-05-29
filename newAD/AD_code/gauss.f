       SUBROUTINE GAUSS(XCENTR,W,A,FWHM,IBASE,MRLFT,MRRGT,CHISQ,
     1 IDATA,YBACK,fixwflg)
C        A program stolen, devised, and pirated by P C Womble
C        LAST Edit date: 7/12/88
c        Modified for fixed width fit 4/95 R. Kaye   

       PARAMETER (MAXSIZ=16534)
       logical fixwflg       
c       DIMENSION F(4096)
       dimension IDATA(MAXSIZ),YBACK(MAXSIZ)
5      W=FWHM/1.665
       CHIOLDW=1.E7
       DELW=-.05
       FLAGW=0
10     CHIOLD=1.E7
       DELX=.1
20     CALL CHICALC(XCENTR,W,A,F,CHISQ,IBASE,MRLFT,MRRGT,
     1 IDATA,YBACK)
       if (chisq .eq. -1) return

       IF (CHISQ .LE. CHIOLD) THEN
            CHIOLD=CHISQ
            XCENTR=XCENTR+DELX
            GO TO 20
       ELSE
            IF (ABS(DELX) .LE. .009) GO TO 30
            DELX=-DELX/5
            CHIOLD=CHISQ
            XCENTR=XCENTR + DELX
            GOTO 20
       END IF
30     XCENTR=XCENTR-DELX
       if (fixwflg) return
       CALL CHICALC(XCENTR,W,A,F,CHISQ,IBASE,MRLFT,MRRGT,
     1 IDATA,YBACK)
        if (chisq .eq. -1) return
       IF (FLAGW .EQ. 1) GO TO 50
       IF (CHISQ .LE. CHIOLDW) THEN
            CHIOLDW=CHISQ
            W=W+DELW
            GOTO 20
       ELSE
            IF (ABS(DELW) .LE. .001) GOTO 40
            DELW=-DELW/5
            CHIOLDW=CHISQ
            W=W+DELW
            GOTO 10
       END IF
40     W=W-DELW
       FLAGW=1
       GOTO 10
50     RETURN
       END


      
       SUBROUTINE CHICALC(XCENTR,W,A,F,CHISQ,IBASE,MRLFT,MRRGT,
     1 IDATA,YBACK)
       PARAMETER (MAXSIZ=16534)
       DIMENSION F(8192)
       dimension IDATA(MAXSIZ),YBACK(MAXSIZ)
       YF=0
       FF=0
       DO 10 I=MRLFT,MRRGT
         X=I
         Y=IDATA(IBASE+I)
         ye = y
         if (ye .le. 0) ye = 1
         F(I)=EXP(-((X-XCENTR)/W)**2)
         YF=YF+(Y-YBACK(I))*F(I)/ye
         FF=FF+F(I)*F(I)/ye
10     CONTINUE
       CHISQ=0.
       if (ff .eq. 0) then
              chisq = -1
              return
       end if
       A=YF/FF
       DO 20 I=MRLFT,MRRGT
         F(I)=A*F(I)+YBACK(I)
         ye = idata(ibase+i)
         if (ye .le. 0) ye = 1
         CHISQ=CHISQ+(IDATA(IBASE+I)-F(I))**2/ye
20     CONTINUE
       RETURN
       END



      SUBROUTINE GAUS2(FWHM1,FWHM2,IBASE,MRLFT,MRRGT,IDATA,
     1 YBACK,XCENTR1,XCENTR2,CHISQ,A1,A2,W1,W2)
C     A program for the masses by P. C. Womble
C     Current Edit Date 7/13/88
      PARAMETER (MAXSIZ=16534)
      dimension IDATA(MAXSIZ), YBACK(MAXSIZ)
c      OPEN (4,FILE='TIM.DAT',STATUS='NEW')
C      write (6,*)'Just starting gaus2'
      W1=FWHM1/1.665
      W2=FWHM2/1.665
      W13=W1
      W14=W1
      W23=W2
      W24=W2
C      write (6,*)'Just Inited Wsubs'
      XCENTR11=XCENTR1
      XCENTR12=XCENTR1
      XCENTR13=XCENTR1
      XCENTR14=XCENTR1
      XCENTR22=XCENTR2
      XCENTR23=XCENTR2
      XCENTR24=XCENTR2
C      write (6,*)'Past Xcentrsubs'
      CHIOLD11=1.E7
      CHIOLD22=1.E7
      CHIOLD33=1.E7
      CHIOLD44=1.E7
      CHIOLD4=1.E7
      CHIOLD3=1.E7
      CHIOLD2=1.E7
C      write (6,*)'Past chiolds'
      DELX2=.5
      DELX1=.1
      DELW1=.5
      DELW2=.5
C      write(6,*)'Made it past the initializations in Gaus2'
      IF (W1 .EQ. W2) DELW2=0
10    DELX1= -5*DELX1
c       write(6,*)'@10!!!!!!!!!!'
      CHIOLD1=1.E7
C      write(4,*)'loop 1'
20     CALL CHICALC2(XCENTR1,XCENTR2,W1,W2,A1,A2,IBASE,MRLFT,MRRGT,
     1 IDATA,YBACK,CHISQ)
        if (chisq .eq. -1) return
C       WRITE(4,902)XCENTR1,XCENTR2,W1,W2,CHISQ,CHIOLD1,DELX1,DELX2
C902    FORMAT(' 20 ',2F9.3,2F8.4,X,F9.4,X,F9.3,X,F7.4,X,F7.4)
      IF (CHISQ .LE. CHIOLD1) THEN
         XCENTR11=XCENTR1
         CHIOLD11=CHISQ
         CHIOLD1=CHISQ
         XCENTR1=XCENTR1+DELX1
         GOTO 20
      ELSE
         IF (ABS(DELX1) .LE. .01) GOTO 30
         DELX1=-DELX1/5
         CHIOLD1=CHISQ
         XCENTR1=XCENTR1+DELX1
         GOTO 20
      END IF
30    XCENTR1=XCENTR11
      CHISQ=CHIOLD11
c      WRITE(4,903)XCENTR1,XCENTR2,W1,W2,CHISQ,CHIOLD2,DELX1,DELX2
c 903   FORMAT( ' 30 ',2F9.3,2F8.4,X,F9.4,X,F9.3,X,F7.4,X,F7.4)
      IF (CHISQ .LE. CHIOLD2) THEN
          CHIOLD2=CHISQ
          XCENTR22=XCENTR2
          XCENTR12=XCENTR1
          CHIOLD22=CHISQ
          XCENTR2=XCENTR2+DELX2
          GOTO 10
      ELSE
         IF (ABS(DELX2) .LE. .01) GOTO 40
         DELX2=-DELX2/5
         CHIOLD2=CHISQ
         XCENTR2=XCENTR2+DELX2
         GOTO 10
      END IF
40    XCENTR1=XCENTR12
      XCENTR2=XCENTR22
      CHISQ=CHIOLD22
      CHIOLD2=1.E7
C      WRITE(4,904) delw2,XCENTR2,W1,W2,CHISQ,CHIOLD3,DELX1,
c     1 DELX2,DELW1,DELW2
C 904   FORMAT(' 40 ',2F9.3,2F8.4,X,F9.4,X,F9.3,X,F7.4,X,
c     1 F7.4,X,F7.4,X,F7.4)
      IF (CHISQ .LE. CHIOLD3) THEN
          CHIOLD33=CHISQ
          CHIOLD3=CHISQ
          XCENTR13=XCENTR1
          XCENTR23=XCENTR2
          W13=W1
          DELX2=-5*DELX2
          W1=W1+DELW1
          IF (DELW2 .NE. 0) GOTO 10
          W23=W2
          W2=W2+DELW1
          GOTO 10
      ELSE
          IF (ABS(DELW1) .LE. .01) GO TO 50
          DELW1=-DELW1/5
          CHIOLD3=CHISQ
          W1=W1+DELW1
          DELX2=-5*DELX2
          IF (DELW2 .NE. 0) GOTO 10
          W2= W2 + DELW1
          GOTO 10
      END IF
50    IF (DELW2 .EQ. 0) GOTO 65
      XCENTR1=XCENTR13
      XCENTR2=XCENTR23
      W1=W13
      CHISQ=CHIOLD33
      CHIOLD3=1.E7
C      WRITE(4,905) XCENTR1,XCENTR2,W1,W2,CHISQ,CHIOLD4,DELW1,DELW2
C905   FORMAT (' 50 ',2F9.3,2F8.4,2F9.3,3F7.4)
      IF (CHISQ .LE. CHIOLD4) THEN
          CHIOLD4=CHISQ
          CHIOLD44=CHISQ
          XCENTR14=XCENTR1
          XCENTR24=XCENTR2
          W14=W1
          W24=W2
          W2=W2+DELW2
          DELX2=-5*DELX2
          DELW1=-5*DELW1
          GOTO 10
      ELSE
          IF (ABS(DELW2) .LE. .01) GO TO 60
          DELW2=-DELW2/5
          CHIOLD4=CHISQ
          W2=W2+DELW2
          GOTO 10
      END IF
60    XCENTR1=XCENTR14
      XCENTR2=XCENTR24
      W1=W14
      W2=W24
      CHISQ=CHIOLD44
C      write(6,*)'Going out through line 60'
c      CLOSE(4)
      RETURN
65    XCENTR1=XCENTR13
      XCENTR2=XCENTR23
      W1=W13
      W2=W23
      CHISQ=CHIOLD33
C      write(6,*)'Going out through line 65'
c      CLOSE(4)
      RETURN
      END

C@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#
      SUBROUTINE CHICALC2 (XCENTR1,XCENTR2,W1,W2,A1,A2,IBASE,MRLFT,
     1 MRRGT,IDATA,YBACK,CHISQ)
      PARAMETER (MAXSIZ=16534)
      DIMENSION F(8192),G(8192),H(8192)
      dimension IDATA(MAXSIZ), YBACK(MAXSIZ)
      YF=0
      FF=0
      FG=0
      YG=0
      GG=0
c      write(6,*)'w2,xcentr2',w2,xcentr2
C      IADEEN=IADEEN+1
C      WRITE(4,*)'LOOP #',IADEEN,' W1= ',W1,' W2= ',W2,'XCENTR 1&2',
C     1 XCENTR1,XCENTR2
C       write(6,*)'before do 10 in chicalc'
      DO 10 I=MRLFT,MRRGT
        X=I
        Y=IDATA(IBASE+I)
        ye = y
        if (y .eq. 0) ye = 1
        XPRIME=((X-XCENTR1)/W1)**2
        XSECUN=((X-XCENTR2)/W2)**2
        F(I)=0
        G(I)=0
        IF (XPRIME .LE. 35) F(I)=EXP(-XPRIME)
        IF (XSECUN .LE. 35) G(I)=EXP(-XSECUN)
c        write(6,*)'F(i),G(i)',f(i),g(i)
        YF=YF+(Y-YBACK(I))*F(I)/Ye
        YG=YG+(Y-YBACK(I))*G(I)/Ye
c        write(6,*)'yf-yg',yf,yg
        FF=FF+F(I)**2/Ye
        FG=FG+F(I)*G(I)/Ye
        GG=GG+G(I)**2/Ye
c        write(6,*)'FF,FG,GG',FF,FG,GG
C      WRITE(4,*)f(i),g(i),yf,yg,ff,fg,gg
10    CONTINUE
c        write(6,*)'MADE IT PAST 10 LOOP IN CHICALC'
      DET=FF*GG-FG*FG
      if (det .eq. 0) then
          chisq =-1
          return
      end if
      A1=(YF*GG-YG*FG)/DET
      A2=(YG*FF-YF*FG)/DET
c      write(6,*)'a1,a2,det',A1,A2,DET
      CHISQ=0
c       write(6,*)'just before 20 in Chicalc'
      DO 20 I=MRLFT,MRRGT
        H(I)=A1*F(I)+A2*G(I)+YBACK(I)
        ys = idata(ibase+i)
        yse = ys
        if (ys .eq. 0) yse = 1
        CHISQ=CHISQ+(ys-H(I))**2/yse
C         write (4,*) IDATA(IBASE+I),h(i),chisq
20    CONTINUE
C         WRITE (4,*) XCENTR1,XCENTR2,W1,W2,A1,A2,CHISQ
      RETURN
      END    
