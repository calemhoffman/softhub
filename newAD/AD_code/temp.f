	   IREC=1
 20	   do i=1,2044
	      buffer(i)=0
	   end do
	   iev = 1
      	   READ(3,end=800,err=980) buffer
 26	   head = buffer(iev)
	   iev = iev + 1
	   if(iev .gt. 2044) goto 60
	   IF(HEAD .LT. 0) THEN
	      NUMADC = IAND(15,HEAD)
	      do i = 1,20
		 ic(i) = 0
	      end do
	      IF(NUMADC .GE. 1 .AND. NUMADC .LE. 20) THEN
		 K = (IAND(496,HEAD))/16
		 ic(k) = buffer(iev)
		 IEV = IEV + 1
		 if(iev .gt. 2044) goto 60
		 IF(NUMADC .GE. 2 .AND. NUMADC .LE. 20) THEN
		    K = (IAND(15872,HEAD))/512
		    IC(K) = BUFFER(IEV)
		    IEV = IEV + 1
		    if(iev .gt. 2044) goto 60
		 endif
		 IF(NUMADC .GE. 3 .AND. NUMADC .LE. 20) THEN
		    DO 30 I = 3, NUMADC
		       K = BUFFER(IEV)
		       IF(K .LE. 0) GOTO 26
		       IEV = IEV + 1
		       if(iev .gt. 2044) goto 60
		       IC(K) = BUFFER(IEV)
		       IF(IC(K) .LE. 0) GOTO 26
		       IEV = IEV + 1
		       if(iev .gt. 2044) goto 60
 30		    CONTINUE
		 ELSE IF(NUMADC .LE. 0 .OR. NUMADC .GE. 21) THEN
		    GOTO 995
		 endif
	      ENDIF
	   ELSE IF(HEAD .GE. 0) THEN
	      iev = iev + 1
	      if(iev .gt. 2044) goto 60
	      GOTO 26
	   ENDIF
	   head = 0
	   if (iev .gt. 2044) goto 60

	   DO 46 K=1,IADC
	      EC(K) = 0.
	      KK = IC(K)
	      IF(KK .LE. 0 .OR. KK .GT. 8060) GOTO 46
	      EC(K) = ENC(KK, K)
 46	   CONTINUE
	   IF (NCLVR .EQ. 0) GOTO 49
	   DO 48 N = 1, NCLVR
C       MULT = 0
	      IM = KCLVR(1,N)
C       IF(IC(IM) .GT. 0) MULT = MULT + 1
	      DO 47 J=2,4
		 IN = KCLVR(J,N)
		 EC(IM) = EC(IM) + EC(IN)
 47	      CONTINUE
 48	   CONTINUE
 49	   CONTINUE









