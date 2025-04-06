C
C!!!!!!!!!!! TABU SEARCH MAIN BODY!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C VERSION 2 12-19-01
C CHANGES MADE FROM DTSMAIN.INC
C 1. MOVING WELL LOCATIONS AND PUMPING RATES CAN BE DONE IN THE SAME
C    ITERATIONS; A MOVING WELL MAY HAVE FOUR ATTRIBUTES: PUMPING RATES
C    X, Y, Z COORINDATES.
C 2. A TWO-DIMENSIONAL ARRAY OBJ2(ATTR, MPARAM) IS USED IN STEADY OF OBJ
C 3. ONCE A NEW BEST SOLUTION IS FOUND, IT ENDS THE CURRENT ITERATION
C 
C P. WANG
C SETTING INITIAL VALUES
C
C DEFINE NUMBER OF NEUGHBORS
C
      IF(NACTIVE.GE.0) THEN ! fixed well locations
         NB = 2 * MPARAM    ! +/- rate
      ELSE
         NB = 8 * MPARAM    ! moving wells +/- rate
      ENDIF

      IOUTts=IOUTGA
      PENALTY=HUGE(1.D0)
      IF(MINMAX.EQ.2) PENALTY=-HUGE(1.D0)

      DO N=1,NPARAM !!!MPARAM     CHANGE 1
         PARDEL(N)=PARMAX(N)-PARMIN(N)
         DX(N)=0.D0
         IF (NPOSIBL(N).GT.1) DX(N)=PARDEL(N)/(NPOSIBL(N)-1.D0)
      ENDDO
C
C    READ INITIAL PUMPING RATES FROM WELL FILE IF IT IS A NEW SIMULATION
C
      IF(IRESTRT.EQ.0) THEN

!!pw 6-21-02

!         write(1612,*) ncsn
!         write(*,*) ncsn

         WRITE(*,*) 'READ INITIAL PUMPING RATES FROM WELL FILE.'
         CALL INITIALP(MPARAM,P,NPER,MXWELL,NWELLS0,X(LCWELL0))
         DO N=1,NPARAM !!!MPARAM     CHANGE 1
            BESTVAL(N)=P(N) !!! added 2/2/02
            IF (NPOSIBL(N).EQ.1) THEN   !!!changed on 2/12/01
               IP(N)=1
            ELSE
               IP(N)=INT(.4+(P(N)-PARMIN(N))/DX(N))+1
            ENDIF
            IY(N)=IP(N)
            IF(MAXGEN.GT.0) THEN
!!          XI(N,N)=1.D0  !! USED FOR POWELL SUBROUTINE
            IF(IP(N).LE.0) THEN
            IP(N)=1
            WRITE(*,*)'WARNING:-- INITIAL SOLUTION IS OUT OF RANGE!!!'
            WRITE(*,*)'AN INITIAL RATE IS SET AT MINIMUM:',IP(N)
            WRITE(IOUTts,*) N,IP(N)
            ELSEIF(IP(N).GT.NPOSIBL(N)) THEN
            IP(N)=NPOSIBL(N)
            WRITE(*,*)'WARNING:-- INITIAL SOLUTION IS OUT OF RANGE!!!'
            WRITE(*,*)'AN INITIAL RATE IS SET AT MAXIMUM:',IP(N)
            WRITE(IOUTts,*) N,IP(N)
            ENDIF
           ENDIF
         ENDDO
      ENDIF
C
C DETERMINE MAXIMUM NUMBER OF POSIBILITIES
C
      MAXVAL=0
      do n=1,Nparam
        MAXVAL=MAX(MAXVAL,NPOSIBL(N))
      enddo

      ALLOCATE (FREQ(MAXVAL,NPARAM),STAT=IERR) !!! CHANGE 2 (3/10/00)
      IF(IERR.NE.0) STOP 'ERROR: -- IN ALLOCATING MEMORY TO FREQ.'

      INQUIRE(FILE='MGO.INI',EXIST=FILEEXISTED)
      IF(FILEEXISTED) THEN
         WRITE(*,*)'READ INITIAL TABU SETTINGS FROM [MGO.INI].'
         OPEN(INTABU,FILE='MGO.INI',STATUS='OLD',ERR=109)
         READ(INTABU,TS,ERR=109)
         CLOSE(INTABU)
      ELSE
         WRITE(*,*)'ASSIGN INITIAL TABU SETTINGS.'
         NSIZE0=20
         INC =5
         MAXCYCLE=200
         NSAMPLE=20
         NRESTART=20
         NSTEPSIZE=2
!         TOL=0.0D0
         PNS=.TRUE.
         SCRN=.FALSE.
      ENDIF

!      WRITE(*,TS)
!      WRITE(*,*)
      IF(IDBG.EQ.2) WRITE(IOUTts,TS,ERR=309)

      INQUIRE(FILE='MGOTS.RES',EXIST=FILEEXISTED)
!!      IF(IRESTRT.NE.0.AND.FILEEXISTED) THEN
      IF(IRESTRT.NE.0.AND.FILEEXISTED.AND.MAXGEN.GE.0) THEN  !!2/17/02
         WRITE(*,*) 'READ PREVIOUS RESULTS FROM FILE: [MGOTS.RES].'
         OPEN(INRES,FILE='MGOTS.RES',STATUS='OLD',ERR=209)
         REWIND(INRES)
         READ(INRES,*) NPARAM,NSIZE,NSTEPSIZE,ISTART,NEWMIN
         MAXITER=ISTART+MAXGEN
         ALLOCATE (W(MAXITER),STAT=IERR)
         IF(IERR.NE.0) STOP 'ERROR: -- IN ALLOCATING MEMORY TO W.'

         IF(IDBG.EQ.2) THEN
            WRITE(IOUTts,*)'PREVIOUS SOLUTION'
            WRITE(IOUTts,*)'  N,   IP(N),   SEQ BESTP RECENCY TENURE'
         ENDIF

         DO N=1,NPARAM !!! CHANGE 3/10/00
         READ(INRES,*)IDUM,IP(N),NSEQ(N),BESTVAL(N),RECENCY(N),TENURE(N)
            IF(IDBG.EQ.2) WRITE(IOUTts,FMT="(3I6,G15.8,3I6)",ERR=209)
     &       N,IP(N),NSEQ(N),BESTVAL(N),RECENCY(N),TENURE(N),NPOSIBL(N)
         ENDDO
         READ(INRES,*) FUNCVAL,BESTFUNC,BESTPENT
         IF(IDBG.EQ.2) WRITE(IOUTts,*,ERR=209)
     &    'CURRENT/BEST FUNC VALUE',FUNCVAL,BESTFUNC
         READ(INRES,*) MAXVAL
         DO JV=1,MAXVAL
            READ(INRES,*,ERR=209) IDUM,(FREQ(JV,N),N=1,NPARAM) !!! CHANGE 3/10/00
         ENDDO
!pw      READ(INRES,*)(IDUM,W(I),I=1,ISTART)

         DO I=1,ISTART                    !Sun  05-11-03
            READ(INRES,*) IDUM, W(I)
         ENDDO

         CLOSE(INRES)

         OPEN(IHIST,FILE='HISTORY.DAT',ERR=409)
         REWIND(IHIST)
         READ(IHIST,*) IDUM,NRUN
         NDIMH=NRUN+NB*MAXGEN
         ALLOCATE (HISTORY(NDIMH,NPARAM),STAT=IERR) !!! CHANGE 3/10/00
         IF(IERR.NE.0) STOP 'ERROR: -- IN ALLOCATING MEMORY TO HISTORY.'
         ALLOCATE (HISTOBJ(NDIMH),STAT=IERR)
         IF(IERR.NE.0) STOP 'ERROR: -- IN ALLOCATING MEMORY TO HISTOBJ.'

         DO K=1,NRUN
            READ(IHIST,*)IDUM,HISTOBJ(K),(HISTORY(K,N),N=1,NPARAM)
         ENDDO                                    !!! CHANGE 3/10/00
         WRITE(IOUTts,*)'NUMBER OF PREVIOUS RUNS: ',NRUN
         CLOSE(IHIST)
      ELSE 
         ISTART=0
         NRUN=1
         MAXITER=MAXGEN
         NEWMIN=1
         NSIZE=NSIZE0
         ALLOCATE (W(MAX(1,MAXITER)),STAT=IERR)
         IF(IERR.NE.0) STOP 'ERROR: -- IN ALLOCATING MEMORY TO W.'
         NDIMH=1+NB*MAXGEN ! AT LEAST ONE RECORD 2/14/02
         ALLOCATE (HISTORY(NDIMH,NPARAM),STAT=IERR) !!! CHANGE 3/10/00
         IF(IERR.NE.0) STOP 'ERROR: -- IN ALLOCATING MEMORY TO HISTORY.'
         ALLOCATE (HISTOBJ(NDIMH),STAT=IERR)
         IF(IERR.NE.0) STOP 'ERROR: -- IN ALLOCATING MEMORY TO HISTOBJ.'
!!!??
         IF(MAXGEN.GE.0) THEN

!!! normalize pumping rate !!! 8-3-02

         DO NP=1,NPARAM
            P(NP) = XLOW(NP)+(IY(NP)-1.D0)*DX(NP)
         ENDDO

         CALL FUNCTS(P,FUNCVAL,MPARAM,LENX,X)
         BESTFUNC=FUNCVAL
         BESTPENT=PENTSUM
!         WRITE(*,*) FUNCVAL
!         WRITE(*,*) (P(N),N=1,NPARAM)
         CALL UPDATEHIST(FUNCVAL,IP,NPARAM,HISTOBJ,HISTORY,NDIMH,
     &    NRUN,LDBG)                !!! CHANGE 3/10/00
!! pw 6-21-02
         if(iResponse.ne.0) then
            write(ioutrsp,*) (real(p(kk)),kk=1,mparam)
            if(nparam.gt.mparam)
     &      write(ioutrsp,*) (int(p(kk)),kk=mparam+1, nparam)
            write(ioutrsp,*) real(funcval),real(pentsum)
         endif

         DO N = 1, NPARAM !!! MPARAM CHANGE 3/10/00
            NSEQ(N)=N
            BESTVAL(N)=P(N)
            TENURE(N) = -NSIZE
            RECENCY(N) = IP(N)
            DO JV=1,MAXVAL
               FREQ(JV,N)=0
            ENDDO
            FREQ(IP(N),N)=1
         ENDDO
!!!??
         ENDIF
      ENDIF

C====================================================================

C TABU SEARCH STARTS HERE.    
      
      OPTFUNC=FUNCVAL

      DO N = 1, NPARAM !!! CHANGE 3/10/00
         IY(N) =IP(N)
      ENDDO
C
C PRINT INITILAL SOLUTION
C
!      WRITE(*,FMT="(I10,2G15.8)") ISTART,FUNCVAL,BESTFUNC
!      WRITE(IOUTts,FMT="(I10,2G15.8)") ISTART,FUNCVAL,BESTFUNC
C
C MAIN DO LOOP
C
      pns=.false.
      scrn=.false.
      DO ITER = ISTART+1,MAXITER

        write(*,1119) iter
        write(ioutts,1119) iter

        inner=0         !cz


C
!         IF ((MINMAX.EQ.1.AND.BESTFUNC.LE.TOL) .OR.
!     &      (MINMAX.EQ.2.AND.BESTFUNC.GE.TOL)   ) EXIT

C FIND THE OBJECTIVE FUNCTION VALUES IN NEIGHBORS.
      DO N=1,MPARAM
         DO INDEX=1,8
            OBJ2(INDEX,N)=PENALTY
         ENDDO
      ENDDO
      DO NX = 1, MPARAM  ! FOR EACH WELL
!         N=nx   !!!! pw 7-7-02
         N=NSEQ(NX) !! for pns 
           if(n.gt.mparam) n=n-mparam    !pw 7-10-02
         NATTR=2                                ! # of attributes
         LISTOFCHANGE(1)=+1 !!pw for tooele+1   ! increase rate
         LISTOFCHANGE(2)=-1                     ! decrease rate
         IF(NACTIVE.LT.0) THEN
           NATTR=8
           KPSTART=pmodflow(12,N)
           IPSTART=pmodflow(13,N)
           JPSTART=pmodflow(14,N)
           KPEND=pmodflow(15,N)
           IPEND=pmodflow(16,N)
           JPEND=pmodflow(17,N)
           LISTOFCHANGE(3)=+1                   ! move cell right
           LISTOFCHANGE(4)=-1                   ! move cell left
           LISTOFCHANGE(5)=JPEND-JPSTART+1      ! move one row
           LISTOFCHANGE(6)=-LISTOFCHANGE(5)     ! move back 
           LISTOFCHANGE(7)=(IPEND-IPSTART+1)*(JPEND-JPSTART+1) ! move one layer)
           LISTOFCHANGE(8)=-LISTOFCHANGE(7)     ! move up
         ENDIF
C         write(*,*)'list of changes'
C         write(*,*)(listofchange(kkk),kkk=1,nattr)

C
C CHANGE THE SOLUTION
C
         DO INDEX=1,NATTR
            IF(INDEX.LE.2) THEN
               NDX=N         ! CHANGE RATE
            ELSE
               NDX=N+MPARAM  ! CHANGE LOCATION
            ENDIF
            IY(NDX)=IP(NDX)+LISTOFCHANGE(INDEX)*NSTEPSIZE
!            write(*,*) 'index,ndx and iy(ndx)',index, ndx,iy(ndx)
C
C CHECK TABU STATUS
C
         IF((RECENCY(NDX).EQ.IY(NDX)).AND.((ITER-TENURE(NDX)).LE.NSIZE)
     &      .OR.IY(NDX).GT.NPOSB(NDX).OR.IY(NDX).LT.1) THEN
            FUNCVAL = PENALTY
         ELSE
            IF(SCRN) THEN
               CALL SCREEN(FIND,FUNCVAL,IY,NPARAM,HISTOBJ,HISTORY,
     &         NDIMH,NPARMAX,NRUN)
            ELSE
               CALL FINDHIST(FIND,FUNCVAL,IY,NPARAM,HISTOBJ,HISTORY,
     &         NDIMH,NPARMAX,NRUN)
            ENDIF
            IF(FIND) THEN
               NFOUND=NFOUND+1
            ELSE
               NRUN=NRUN+1
C
C              UPDATE PUMPING BEFORE SIMULATION.
C
               DO NP=1,NPARAM
                  P(NP) = XLOW(NP)+(IY(NP)-1.D0)*DX(NP)
               ENDDO
               CALL FUNCTS(P,FUNCVAL,MPARAM,LENX,X)

               inner=inner+1
               if(.not.transport) then
                 write(*,1112) inner
               else
                 write(*,1113) inner
               endif

               CALL UPDATEHIST(FUNCVAL,IY,NPARAM,HISTOBJ,HISTORY,
     &          NDIMH,NRUN,LDBG)          !!! CHANGE 3/10/00
!! pw 6-21-02
         if(iResponse.ne.0) then
            write(ioutrsp,*) (real(p(kk)),kk=1,mparam)
            if(nparam.gt.mparam)
     &      write(ioutrsp,*) (int(p(kk)),kk=mparam+1, nparam)
            write(ioutrsp,*) real(funcval),real(pentsum)
         endif
            ENDIF
         ENDIF
C
C        RESTORE SOLUTION IY
C
         IY(NDX)=IP(NDX)
         OBJ2(INDEX,N)=FUNCVAL
C
C        CHECK FOR NEW SOLUTION
         IF((MINMAX.EQ.1.AND.FUNCVAL.LT.BESTFUNC).OR.
     &     (MINMAX.EQ.2.AND.FUNCVAL.GT.BESTFUNC) ) THEN
            BESTFUNC=FUNCVAL
            BESTPENT=PENTSUM
            OPTFUNC=FUNCVAL
            NEWMIN=ITER
            DO K =1, NPARAM                !!! CHANGE 3/10/00
               BESTVAL(K) = P(K)
            ENDDO
            I=(N-1)*8+INDEX
!!!pw 2/26/02 for grd
            IF(PNS) GOTO 410    !patial neighborhood search (PNS)
         ENDIF
         ENDDO ! INDEX
      ENDDO ! MPARAM
C
C  SORT THE OBJECTIVE FUNCTION VALUES IN AN INCREASING ORDER

!!       WRITE(*,*) 'BEFORE'
!!       DO N=1,MPARAM
!!         DO IND=1,8
!!        WRITE(*,FMT="(2I5,G15.8)") N,IND,OBJ2(IND,N)
!!       ENDDO
!!       ENDDO

      CALL SORTALL(OBJ2,NORDER,8*MPARAM)
!!       KK=0
!!       WRITE(*,*) 'AFTER'
!!       DO N=1,MPARAM
!!         DO IND=1,8
!!         KK=KK+1
!!        WRITE(*,FMT="(2I5,G15.8,I5)") N,IND,OBJ2(IND,N),NORDER(KK)
!!       ENDDO
!!       ENDDO

C
C  THE I-TH ELEMENT IN THE NEIGHBORHOOD SET HAS THE SMALLEST FUNCTION      
C  VALUE.

      IF(MINMAX.EQ.1) THEN
         I=NORDER(1)            ! FIRST ONE IS THE MINIMUM ONE
         FUNCVAL=OBJ2(1,1)
      ELSE
         I=NORDER(8*MPARAM)           ! LAST ONE IS THE MAXIMUM ONE
         FUNCVAL=OBJ2(8,MPARAM)
      ENDIF
      ! FOR MINIMIZE
      IF(FUNCVAL.GE.PENALTY) THEN
         WRITE(*,*) 'ALL MOVES ARE TABOO!!!'
         GOTO 415
      ENDIF
410   CONTINUE
C
C  CONVERT I INTO NP AND K
C
      NP=INT((I-1)/8)+1  ! INDEX OF VARIABLE
      K=I-(NP-1)*8       ! ATTRIBUTE OF VARIABLE NP
      NX=NSEQ(1)
      IF(K.LE.2)THEN
         NSEQ(1)=NP
         NQ=NP
      ELSE
         NQ=NP+MPARAM
         NSEQ(1)=NQ
      ENDIF
      DO N=1,NPARAM
         IF(NSEQ(N).EQ.NQ) NK=N
      ENDDO
      NSEQ(NK)=NX

!!      WRITE(IOUTts,*) 'I,NP,K',I,NP,K
C
C UPDATE THE SOLUTION AND WORKING VARAIBLES
C

      IF(K.EQ.1) NDX=+1
      IF(K.EQ.2) NDX=-1
      IF(NACTIVE.LT.0) THEN
         IF(K.EQ.3) NDX=+1
         IF(K.EQ.4) NDX=-1
         KPSTART=pmodflow(12,N)
         IPSTART=pmodflow(13,N)
         JPSTART=pmodflow(14,N)
         KPEND=pmodflow(15,N)
         IPEND=pmodflow(16,N)
         JPEND=pmodflow(17,N)
         IF(K.EQ.5) NDX=JPEND-JPSTART+1         ! move one row
         IF(K.EQ.6) NDX=-(JPEND-JPSTART+1)      ! move one row
         IF(K.EQ.7) NDX=(IPEND-IPSTART+1)*(JPEND-JPSTART+1)  ! move one layer down)
         IF(K.EQ.8) NDX=-(IPEND-IPSTART+1)*(JPEND-JPSTART+1) ! move one layer up)
      ENDIF
C
C UPDATE TABU STATUS
C
      IF(K.GT.2) NP=NP+MPARAM
      RECENCY(NP) = IP(NP)
      TENURE(NP) = ITER
      FREQ(IP(NP),NP)=FREQ(IP(NP),NP)+1
      IP(NP)=IP(NP)+NSTEPSIZE*NDX
      IY(NP)=IP(NP)
C
C A NEW SOLUTION SINCE ITERATION NEWMIN IS FOUND.
C
      IF( (MINMAX.EQ.1.AND.FUNCVAL.LT.OPTFUNC).OR.
     &    (MINMAX.EQ.2.AND.FUNCVAL.GT.OPTFUNC)  ) THEN
         OPTFUNC = FUNCVAL
         NEWMIN=ITER                   ! NEW MINIMIUM VALUE
      ENDIF

      INTENSIFY=.FALSE.
C
C-- CHECK SEARCH PATH EVERY NSAMPLE ITERATIONS
C
      IF (MOD((ITER-NEWMIN+1),NSAMPLE).EQ.0) THEN
         WRITE(*,*) ' Check cycles at iteratins:',ITER
         WRITE(IOUTts,*) ' Check cycles at iteration:',ITER
C
C        DETECT THE CYCLE
C
         CALL CHCKCYCL(FIND,NC,W,ITER-1,NSIZE,MAXCYCLE)

         IF(FIND) THEN
C           A CYCLE IS DETECTED.
            NSIZE =  NSIZE + INC      ! INCREASE TABU SIZE
            WRITE(*,*) ' A PERIOD OF ', NC,' IS DETECTED.'
            WRITE(*,*) ' TABU SIZE IS INCREASED TO',NSIZE
            WRITE(IOUTts,*) ' A PERIOD OF ', NC,' IS DETECTED.'
            WRITE(IOUTts,*) ' TABU SIZE IS INCREASED TO',NSIZE
            IF(NSTEPSIZE.GT.1) THEN
               NSTEPSIZE = NSTEPSIZE/2
               WRITE(*,*) ' STEPSIZE DECREASES TO',NSTEPSIZE
               WRITE(IOUTts,*)' STEPSIZE DECREASES TO',NSTEPSIZE
            ELSE
C
C            START AT A NEW POINT
C
               INTENSIFY=.TRUE.
!               WRITE(*,*) 'SEARCH STARTS AT A NEW POINT.', INTENSIFY
!               WRITE(IOUTts,*) 'TABU SEARCH STARTS AT A NEW POINT.'
            ENDIF
         ELSE
C           A CYCLE IS NOT DETECTED. DO NOTHING.
               WRITE(*,*) ' PERIOD IS NOT FOUND.'
               WRITE(IOUTts,*)' PERIOD IS NOT FOUND.'
         ENDIF
      ENDIF ! END CYCLING CHECK.
C
C START A NEW POINT IF NO IMPROVEMENT IN NRESTART ITERATIONS OR
C BEST FUNCTION VALUE IS GREATER THAN PENALTY
C
415   IF((ITER-NEWMIN).GT.NRESTART.OR.FUNCVAL.GE.PENALTY) THEN
        IF(NSTEPSIZE.GT.1) THEN
           nstepsize=Max(nstepsize/2,1)
           WRITE(*,*) 'Decrease TABU step size',nstepsize
        ELSE
           INTENSIFY=.TRUE.
        ENDIF
      endif

      IF(INTENSIFY) THEN
         WRITE(*,*) 'START A NEW SOLUTION AT ITERATION NUMBER:',ITER
         WRITE(*,*) 'TABU LIST & TABU SIZE ARE RESET'
         WRITE(IOUTts,*)'START A NEW SOLUTION AT ITERATION NUMBER:',ITER
         WRITE(IOUTts,*) 'TABU LIST & TABU SIZE ARE RESET'

C
C        INTENSIFYING
C
         CALL NEWSOL(IY,NPARAM,HISTOBJ,HISTORY,NDIMH, !!NPOSIZ 6-19-02
     &           NRUN,MINMAX)
         DO N = 1, NPARAM                    !!! ADDED 2/11/02
            IP(N)=IY(N)
            P(N)=XLOW(N)+DX(N)*(IP(N)-1)    
            TENURE(N) = ITER
            RECENCY(N) = IP(N)
!pw            PRINT*,N,IP(N)           !Sun  05-11-03
            FREQ(IP(N),N)=FREQ(IP(N),N)+1
            NSEQ(N)=N
         ENDDO
C
C        DIVERSIFYING
C
!         DO N = 1, NPARAM                    !!! CHANGE 3/10/00
!            CALL SORT1(JV,FREQ(1,N),NPOSIBL(N))  ! RETURNS JV OF
!            IP(N)=JV
!            IY(N)=JV
!            P(N)=XLOW(N)+DX(N)*(JV-1)        ! THE MINIMUM VALUE
!            TENURE(N) = ITER
!            RECENCY(N) = IP(N)
!            FREQ(JV,N)=FREQ(JV,N)+1
!            NSEQ(N)=N
!         ENDDO

            IF(SCRN) THEN
               CALL SCREEN(FIND,FUNCVAL,IY,NPARAM,HISTOBJ,HISTORY,
     &         NDIMH,NPARMAX,NRUN)
            ELSE
               CALL FINDHIST(FIND,FUNCVAL,IY,NPARAM,HISTOBJ,HISTORY,
     &         NDIMH,NPARMAX,NRUN)
            ENDIF
            IF(FIND) THEN
               NFOUND=NFOUND+1
            ELSE
               NRUN=NRUN+1
               CALL FUNCTS(P,FUNCVAL,MPARAM,LENX,X)
               CALL UPDATEHIST(FUNCVAL,IY,NPARAM,HISTOBJ,HISTORY,NDIMH,
     &         NRUN,LDBG)                 !!! CHANGE 3/10/00
!! pw 6-21-02
            if(iResponse.ne.0) then
               write(ioutrsp,*) (real(p(kk)),kk=1,mparam)
               if(nparam.gt.mparam)
     &         write(ioutrsp,*) (int(p(kk)),kk=mparam+1, nparam)
               write(ioutrsp,*) real(funcval),real(pentsum)
            endif
         ENDIF
         OPTFUNC=FUNCVAL
         NEWMIN=ITER                   ! NEW MINIMIUM VALUE
         NSIZE=NSIZE0                  ! RESET TABU SIZE
      ENDIF ! INTENSIFY
C
      W(ITER) = FUNCVAL
      WRITE(IOUTobf,FMT="(I10,2G15.8)") ITER,BESTFUNC,BESTPENT

         write (*,9119) funcval,bestfunc
         write (ioutts,9119) funcval,bestfunc

!      WRITE(IOUTts,FMT="(I10,2G15.8)") ITER,FUNCVAL,BESTFUNC
!      WRITE(IOUTts,FMT="(8I8)") (IP(K),K=1,NPARAM)
!      WRITE(*,FMT="(I10,2G15.8)") ITER,FUNCVAL,BESTFUNC
!      WRITE(*,FMT="(100I8)") (IP(K),K=1,NPARAM)

      IF(ITER-NEWMIN.GT.NTOL) THEN
         WRITE(*,*) ' Maximum number of generations without ',
     &    'improvement has reached.'
         EXIT
      ENDIF

      ENDDO ! LOOP ITER  
C===================================================================
C
C  END OF TABU SEARCH
C
      ITER=MIN(ITER,MAXITER)

C  SAVE THE RESULTS FOR RESTART
      if(MaxGen.ge.1) then
!        WRITE(*,*)'SAVE TABU PARAMETERS IN [MGOTS.RES]'
         OPEN(INRES,FILE='MGOTS.RES',STATUS='UNKNOWN',ERR=409)
         REWIND(INRES)
         WRITE(INRES,FMT="(5I6)") NPARAM,NSIZE,NSTEPSIZE,ITER,NEWMIN
         DO N=1,NPARAM                    !!! CHANGE 3/10/00
            WRITE(INRES,FMT="(3I6,1X,G15.8,2I6)")
     &      N,IP(N),NSEQ(N),BESTVAL(N),RECENCY(N),TENURE(N)
         ENDDO
         WRITE(INRES,FMT="(G15.8,1X,2G15.8)")FUNCVAL,BESTFUNC,BESTPENT
         WRITE(INRES,FMT="(I6)") MAXVAL
         DO JV=1,MAXVAL
            WRITE(INRES,FMT="(I6,100I6)") JV,(FREQ(JV,N),N=1,NPARAM)
         ENDDO
         WRITE(INRES,FMT="(I6,G15.7)")(I,W(I),I=1,ITER)
         CLOSE(INRES)

!         WRITE(*,*)'SAVE SEARCH PATH IN [HISTORY.DAT]'
         OPEN(IHIST,FILE='HISTORY.DAT',ERR=609)
         REWIND(IHIST)
         WRITE(IHIST,FMT="(2I8)") NPARAM,NRUN
         DO K=1,NRUN
            WRITE(IHIST,FMT="(I8,1X,G15.8,100I8)")
     &      K,HISTOBJ(K),(HISTORY(K,N),N=1,NPARAM)
         ENDDO
         CLOSE(IHIST)
      endif
!!!
c  Run simulation model with optimized parameters
      if(iwrite.gt.0) then
        write(*,*) 'Run Simulation Model with Optimized Parameters...'
!cz     write(IOUTts,*)'Run Simulation Model with Optimized Parameters'
        iwrite=999
        idbg=2
        Ldbg=idbg
        ibudfl=1
        icbcfl=1
        iwelcb=-1
        CALL FUNCTS(BESTVAL,BESTFUNC,mparam,lenx,x)
      endif
C
C  WRITE THE RESULTS
C
!      WRITE(IOUTts,*,ERR=309)'Optimal function value   ='
!      WRITE(*,*,ERR=309)'THE OPTIMAL SOLUTION'
!      DO N = 1, MPARAM
!         WRITE(IOUTts,*,ERR=309) N, BESTVAL(N)!*PSCALE
!         WRITE(*,*,ERR=309) N, BESTVAL(N)!*PSCALE
!      ENDDO
!      WRITE(IOUTts,529,ERR=309) BESTFUNC
!     WRITE(*,529,ERR=309) BESTFUNC
!      WRITE(*,*) '# OF RUNS  # OF RERUNS   TOTAL # OF RUNS '
!      WRITE(*,*) NRUN,NFOUND,NRUN+NFOUND

      CLOSE(IOUTts)

509   FORMAT(1X, 'ITERATION NUMBER:', I5, '   FUNCTION VALUE', F12.6)
519   FORMAT(1X, 5I6, 2F15.6)
529   FORMAT(1X, 'FUNCTION VALUE', G16.8)
539   FORMAT(1X, 101I4)
549   FORMAT(10I4,F10.5)
C
C     NORMAL STOP
C
      STOP
C     ABNORMAL STOP
109   STOP 'ERROR: IN READING FILE: MGO.INI.'
209   STOP 'ERROR: IN READING FILE: MGOTS.RES.'
309   STOP 'ERROR: IN WRITING FILE: MGOTS.OPT.'
409   STOP 'ERROR: IN WRITING FILE: MGOTS.RES.'
609   STOP 'ERROR: IN WRITING FILE: HISTORY.DAT.'

 1119 format(/'################  TS Iteration',i5.4,
     & '  ################')
 9119 format(1x,'Current Function Value             =',g16.5/,
     &       1x,'Optimal Function Value             =',g16.5/)
