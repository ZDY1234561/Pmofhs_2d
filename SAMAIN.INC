C SIMULATED ANNEALING MAIN BODY
C
      nfound=0        !cz Sat  05-10-03
      IOUTsa=IOUTGA
      DO N=1,NPARAM !!!MPARAM     CHANGE 1
         PARDEL(N)=PARMAX(N)-PARMIN(N)
         DX(N)=0.D0
         IF(NPOSIBL(N).GT.1) DX(N)=PARDEL(N)/(NPOSIBL(N)-1.D0)
      ENDDO
      MAXVAL=0
      DO N=1,NPARAM
         MAXVAL=MAX(MAXVAL,NPOSIBL(N))
      ENDDO

      ALLOCATE (FREQ(MAXVAL,NPARAM),STAT=IERR)
      IF(IERR.NE.0) STOP 'ERROR: -- IN ALLOCATING MEMORY TO FREQ.'

      IF(IRESTRT.EQ.0) THEN
         WRITE(*,660)
  660    format(/1x,'READ INITIAL PUMPING RATES FROM WELL FILE.')
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
            IF(IP(N).LE.0) THEN
              IP(N)=1
              WRITE(*,*)'WARNING:-- INITIAL SOLUTION IS OUT OF RANGE!!!'
              WRITE(*,*)'AN INITIAL RATE IS SET AT MINIMUM:',IP(N)
              WRITE(IOUTsa,*) N,IP(N)
            ELSEIF(IP(N).GT.NPOSIBL(N)) THEN
              IP(N)=NPOSIBL(N)
              WRITE(*,*)'WARNING:-- INITIAL SOLUTION IS OUT OF RANGE!!!'
              WRITE(*,*)'AN INITIAL RATE IS SET AT MAXIMUM:',IP(N)
              WRITE(IOUTsa,*) N,IP(N)
            ENDIF
            ENDIF
         ENDDO
      ENDIF

      INQUIRE(FILE='MGO.INI',EXIST=FILEEXISTED)
      IF(FILEEXISTED) THEN
!cz      WRITE(*,*)'READ INITIAL SA SETTINGS FROM FILE: [MGO.INI].'
         WRITE(*,666)
  666    format(/1x,'READ INITIAL SA SETTINGS FROM FILE: [MGO.INI].')
         OPEN(INSA,FILE='MGO.INI',STATUS='OLD',ERR=108)
         READ(INSA,SA,ERR=108)
         WRITE(*,SA)
         WRITE(*,*)
!cz      WRITE(IOUTsa,SA,ERR=308)
         CLOSE(INSA)
      ELSE
!cz         WRITE(*,*) 'ASSIGN INITIAL SA SETTINGS.'
         idum=-10
         TEMPERATURE=1.D5
         REDUCTIONFACTOR=.95
!         TOL=0.0
         nrestart=100
         NSTEPSIZE=1
         WRITE(*,SA)
         WRITE(*,*)
         WRITE(IOUTsa,SA,ERR=308)
      ENDIF


      INQUIRE(FILE='MGOSA.RES',EXIST=FILEEXISTED)
      IF(IRESTRT.NE.0.AND.FILEEXISTED) THEN
         WRITE(*,*) 'LOAD PREVIOUS RESULTS FROM FILE: [MGOSA.RES].'
         OPEN(INRES,FILE='MGOSA.RES',ERR=208)
         REWIND(INRES)
         READ(INRES,*,ERR=208) TEMPERATURE,NSTEPSIZE,ISTART
         MAXITER=ISTART+MAXGEN*NPOPSIZ
         ALLOCATE (W(MAXITER),STAT=IERR)
         IF(IERR.NE.0) STOP 'ERROR: -- IN ALLOCATING MEMORY TO W.'

         DO N=1,NPARAM
            READ(INRES,*,ERR=208) IP(N),P(N),BESTVAL(N)
         ENDDO
         READ(INRES,*,ERR=208) FUNCVAL,BESTFUNC,BESTPENT
         DO JV=1,MAXVAL
            READ(INRES,*,ERR=208) IDUMMY,(FREQ(JV,N),N=1,NPARAM)
         ENDDO

!pw         READ(INRES,*,ERR=208)(W(I),I=1,ISTART)

         DO I=1,ISTART
            READ(INRES,*,ERR=208) idummy,W(I)
         ENDDO

         CLOSE(INRES)
         OPEN(IHIST,FILE='HISTORY.DAT',ERR=408)
         REWIND(IHIST)
         READ(IHIST,*) IDUMMY,NRUN

         NDIMH=NRUN+MAXGEN*NPOPSIZ

         ALLOCATE (HISTORY(NDIMH,NPARAM),STAT=IERR)
         IF(IERR.NE.0) STOP 'ERROR: -- IN ALLOCATING MEMORY TO HISTORY.'
         ALLOCATE (HISTOBJ(NDIMH),STAT=IERR)
         IF(IERR.NE.0) STOP 'ERROR: -- IN ALLOCATING MEMORY TO HISTOBJ.'

         DO K=1,NRUN
            READ(IHIST,*,ERR=408) IDUMMY,HISTOBJ(K),(HISTORY(K,N),
     &      N=1,NPARAM)
         ENDDO
         WRITE(IOUTsa,*)'NUMBER OF PREVIOUS RUNS: ',NRUN
         CLOSE(IHIST)
      ELSE 
         ISTART=0
         NRUN=1
         MAXITER=MAXGEN*NPOPSIZ
         ALLOCATE (W(MAXITER),STAT=IERR)

         NDIMH=NRUN+MAXGEN*NPOPSIZ
         ALLOCATE (HISTORY(NDIMH,NPARAM),STAT=IERR)
         IF(IERR.NE.0) STOP 'ERROR: -- IN ALLOCATING MEMORY TO HISTORY.'
         ALLOCATE (HISTOBJ(NDIMH),STAT=IERR)
         IF(IERR.NE.0) STOP 'ERROR: -- IN ALLOCATING MEMORY TO HISTOBJ.'

         CALL FUNCTS(P,FUNCVAL,MPARAM,LENX,X)
         BESTFUNC=FUNCVAL
         BESTPENT=PENTSUM
         CALL UPDATEHIST(FUNCVAL,IP,NPARAM,HISTOBJ,HISTORY,NDIMH,
     &    NRUN,LDBG)


         if(iResponse.ne.0) then
            write(ioutrsp,*) (real(p(kk)),kk=1,mparam)
            if(nparam.gt.mparam)
     &      write(ioutrsp,*) (int(p(kk)),kk=mparam+1, nparam)
            write(ioutrsp,*) real(funcval),real(pentsum)
         endif


         DO N = 1, NPARAM
            BESTVAL(N)=P(N)
            DO JV=1,MAXVAL
               FREQ(JV,N)=0
            ENDDO
         ENDDO

      ENDIF

C====================================================================
         
C SIMULATED ANNEALING STARTS HERE.    
      
      OPTFUNC=FUNCVAL

      DO N = 1, NPARAM
         IY(N) =IP(N)
      ENDDO

      DO ITER = ISTART+1,MAXGEN+ISTART

      write(*,1118) iter
      write(ioutsa,1118) iter

!         IF ((MINMAX.EQ.1.AND.BESTFUNC.LE.TOL) .OR.
!     &      (MINMAX.EQ.2.AND.BESTFUNC.GE.TOL)   ) EXIT

         NACCEPT=0
         NREJECT=0

         DO INNER=1,NPOPSIZ
C
C        DISTURBE THE SOLUTION
C      

!         CALL RAN3(idum,RAND1)
!         N=INT(RAND1*MPARAM)+1

         N=MOD(INNER,NPARAM)+1

         K=1
         IF(IY(N).EQ.NPOSB(N)) K=-1
         IF(IY(N).GT.1.OR.IY(N).LT.NPOSB(N)) THEN
            CALL RAN3(idum,RAND1)
            IF(RAND1.GE.0.5) K=-1
         ENDIF
         IY(N)=IP(N)+NSTEPSIZE*K
         IY(N)=MIN(NPOSB(N),IY(N))
         IY(N)=MAX(1,IY(N))

         CALL FINDHIST(FIND,FUNCVAL1,IY,NPARAM,HISTOBJ,HISTORY,
     &       NDIMH,NPARMAX,NRUN)

         IF(FIND) THEN
             NFOUND=NFOUND+1
         ELSE
             NRUN=NRUN+1
             DO NP=1,NPARAM
                P(NP) = XLOW(NP)+(IY(NP)-1.D0)*DX(NP)
             ENDDO
             CALL FUNCTS(P,FUNCVAL1,MPARAM,LENX,X)


!cz
            if(.not.transport) then
              write(*,1112) inner     !added to print to screen sample no.
            else
              write(*,1113) inner    !added to print to screen sample no.
            endif
!cz

             CALL UPDATEHIST(FUNCVAL1,IY,NPARAM,HISTOBJ,HISTORY,
     &            NDIMH,NRUN,LDBG)

         if(iResponse.ne.0) then
            write(ioutrsp,*) (real(p(kk)),kk=1,mparam)
            if(nparam.gt.mparam)
     &      write(ioutrsp,*) (int(p(kk)),kk=mparam+1, nparam)
            write(ioutrsp,*) real(funcval),real(pentsum)
         endif

         ENDIF

         IF((MINMAX.EQ.1.AND.FUNCVAL1.LE.FUNCVAL) .OR.
     &      (MINMAX.EQ.2.AND.FUNCVAL1.GE.FUNCVAL)) THEN
            PROB=1.0
            IF((MINMAX.EQ.1.AND.FUNCVAL1.LT.BESTFUNC) .OR.
     &         (MINMAX.EQ.2.AND.FUNCVAL1.GT.BESTFUNC)) THEN
               BESTFUNC=FUNCVAL1
               BESTPENT=PENTSUM
               DO K=1,NPARAM
                  BESTVAL(K)=P(K)
               ENDDO
!         WRITE(*,*) ITER,INNER,BESTFUNC
!         WRITE(IOUTsa,*) ITER,INNER,BESTFUNC

            ENDIF
         ELSE
            DF=ABS(FUNCVAL1-FUNCVAL)
            PROB=EXP(-DF/TEMPERATURE)
         ENDIF
         CALL RAN3(idum,RAND)

         IF(RAND.LE.PROB) THEN
            ! ACCEPT
            NACCEPT=NACCEPT+1
            FUNCVAL=FUNCVAL1
            IP(N)=IY(N)
            FREQ(IP(N),N)=FREQ(IP(N),N)+1
         ELSE
            ! REJECT
            NREJECT=NREJECT+1
            IY(N)=IP(N)
            P(N) = XLOW(N)+(IY(N)-1)*DX(N)
         ENDIF

!      WRITE(*,989) FUNCVAL,(IY(N),N=1,NPARAM)
987   FORMAT(1X,3I5,2F10.6)
989   FORMAT(1X,G15.6,10I5)
      END DO  ! END INNER LOOP
!!!      STOP
C
C     REDUCE THE TEMPERATURE
C
      TEMPERATURE=TEMPERATURE*REDUCTIONFACTOR

      WRITE(IOUTobf,FMT="(I10,2G15.8)") ITER,BESTFUNC,BESTPENT

!cz   write (*,1118) iter,naccept,nreject,funcval,bestfunc
      write (*,9118) naccept,nreject,funcval,bestfunc
!cz   write (ioutsa,1118) iter,naccept,nreject,funcval,bestfunc
      write (ioutsa,9118) naccept,nreject,funcval,bestfunc
!      WRITE(*,298) ITER,NACCEPT,NREJECT,BESTFUNC
!      WRITE(IOUTsa,298) ITER,NACCEPT,NREJECT,BESTFUNC

298   FORMAT(1X,'GENERATION:',I6,' ACCEPTED:',I6,' REJECTED',I6,
     & ' BEST OBJECTIVE:',G16.6)


      IF( (MINMAX.EQ.1.AND.FUNCVAL.LT.OPTFUNC).OR.
     &    (MINMAX.EQ.2.AND.FUNCVAL.GT.OPTFUNC)  ) THEN
         OPTFUNC = FUNCVAL
         NEWMIN=ITER                   ! NEW MINIMIUM VALUE
      ENDIF

      W(ITER) = FUNCVAL

!!!      WRITE(*,988) ITER, (IP(N),N=1,NPARAM),TEMPERATURE,FUNCVAL
988   FORMAT(1X,I10,10I3,2F10.5)
CSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
C START A NEW POINT IF NO IMPROVEMENT IN NRESTART ITERATIONS
      IF((ITER-NEWMIN).GT.nrestart) THEN
      WRITE(*,*)'START A NEW SOLUTION AT ITERATION NUMBER:',ITER
      WRITE(IOUTSA,*)'START A NEW SOLUTION AT ITERATION NUMBER:',ITER

         CALL NEWSOL(IY,NPARAM,HISTOBJ,HISTORY,NDIMH, !!NPOSIZ 6-19-02
     &           NRUN,MINMAX)
         DO N = 1, NPARAM                    !!! ADDED 2/11/02
            IP(N)=IY(N)
            P(N)=XLOW(N)+DX(N)*(IP(N)-1)    
            FREQ(IP(N),N)=FREQ(IP(N),N)+1
         ENDDO

!         DO N = 1, NPARAM
!            CALL SORT1(JV,FREQ(1,N),MAXVAL)  ! RETURNS THE INDEX I OF
!            IP(N)=JV
!            IY(N)=JV
!            P(N)=XLOW(N)+DX(N)*(JV-1)            ! THE MINIMUM VALUE
!            FREQ(JV,N)=FREQ(JV,N)+1
!         END DO
         CALL FINDHIST(FIND,FUNCVAL,IY,NPARAM,HISTOBJ,HISTORY,NDIMH,
     &           NPARMAX,NRUN)

         IF(FIND) THEN
            NFOUND=NFOUND+1
         ELSE
            NRUN=NRUN+1
            CALL FUNCTS(P,FUNCVAL,MPARAM,LENX,X)
            CALL UPDATEHIST(FUNCVAL,IY,NPARAM,HISTOBJ,HISTORY,NDIMH,
     &      NRUN,LDBG)

         if(iResponse.ne.0) then
            write(ioutrsp,*) (real(p(kk)),kk=1,mparam)
            if(nparam.gt.mparam)
     &      write(ioutrsp,*) (int(p(kk)),kk=mparam+1, nparam)
            write(ioutrsp,*) real(funcval),real(pentsum)
         endif

         ENDIF
         OPTFUNC=FUNCVAL
         NEWMIN=ITER                   ! NEW MINIMIUM VALUE
         NSIZE=NSIZE0
!         NSTEPSIZE=MIN(NSTEPSIZE-1,1)

         NSTEPSIZE=MAX(NSTEPSIZE-1,1)  !Sun  05-11-03

      ENDIF
CSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
      OPEN(INRES,FILE='MGOSA.RES',ERR=408)
      REWIND(INRES)
      WRITE(INRES,FMT="(1X,G15.8,2I6)",ERR=408) TEMPERATURE,NSTEPSIZE,
     & ITER
      DO N=1,NPARAM
         WRITE(INRES,FMT="(I6,1X,2G15.8)",ERR=408)
     &   IP(N),P(N),BESTVAL(N)
      ENDDO
      WRITE(INRES,FMT="(1X,3G15.8)",ERR=408) FUNCVAL,BESTFUNC,BESTPENT
      DO JV=1,MAXVAL
         WRITE(INRES,FMT="(I6,1X,100I6)") JV, (FREQ(JV,N),N=1,NPARAM)
      ENDDO
!pw      WRITE(INRES,FMT="(I6,G15.8)",ERR=408)(I,W(I),I=1,ITER)

      DO I=1,ITER
        WRITE(INRES,FMT="(I6,G15.8)",ERR=408) I,W(I)
      ENDDO

      CLOSE(INRES)
      IF(ITER-NEWMIN.GT.NTOL) THEN
         WRITE(*,*) ' Maximum number of generations without ',
     &    'improvement has been reached.'
         EXIT
      ENDIF
      END DO ! OUTSIDE LOOP ITER  
C====================================================================
C  END OF SEARCH

      ITER=MIN(ITER,MAXGEN+ISTART)


C  WRITE THE RESULTS 

!      WRITE(IOUTsa,*,ERR=308)'THE OPTIMAL SOLUTION'
!      DO N = 1, MPARAM
!         WRITE(IOUTsa,FMT="(I5,F15.6)",ERR=308)N,BESTVAL(N)*PSCALE
!         WRITE(*,FMT="(I5,F15.6)",ERR=308) N, BESTVAL(N)*PSCALE
!      END DO
!      WRITE(IOUTsa,528,ERR=308) BESTFUNC
!      WRITE(*,528,ERR=308) BESTFUNC


!      WRITE(*,*) '# OF RUNS  # OF RERUNS   TOTAL # OF RUNS '
!      WRITE(*,*) NRUN,NFOUND,NRUN+NFOUND

c  Run simulation model with optimized parameters
      if(iwrite.gt.0) then
        write(*,*) 'Run Simulation Model with Optimized Parameters...'
!cz     write(IOUTsa,*)'Run Simulation Model with Optimized Parameters'
        iwrite=999
        idbg=2
        Ldbg=idbg
        ibudfl=1
        icbcfl=1
        iwelcb=-1
        CALL FUNCTS(BESTVAL,BESTFUNC,mparam,lenx,x)
      endif

      CLOSE(IOUTsa)
C  SAVE THE RESULTS FOR RESTART


!      WRITE(*,*) 'SAVE SEARCH PATH IN [HISTORY.DAT]'
      OPEN(IHIST,FILE='HISTORY.DAT',ERR=608)
      REWIND(IHIST)
      WRITE(IHIST,*,ERR=608) NPARAM, NRUN
      DO K=1,NRUN
         WRITE(IHIST,FMT="(I6,1X,G15.8,100I6)")
     &    K,HISTOBJ(K),(HISTORY(K,N),N=1,NPARAM)
      ENDDO
      CLOSE(IHIST)

508   FORMAT(1X, 'ITERATION NUMBER:', I5, '   FUNCTION VALUE', F12.6)
518   FORMAT(1X, 5I6, 2F15.6)
528   FORMAT(1X, 'FUNCTION VALUE', G16.6)
538   FORMAT(1X, 101I4)
548   FORMAT(10I4,F10.5)

C     NORMAL STOP
C
      STOP
C     ABNORMAL STOP
108   STOP 'ERROR: IN READING FILE: MGO.INI.'
208   STOP 'ERROR: IN READING FILE: MGOSA.RES.'
308   STOP 'ERROR: IN WRITING FILE: MGOSA.OPT.'
408   STOP 'ERROR: IN WRITING FILE: MGOSA.RES.'
608   STOP 'ERROR: IN WRITING FILE: HISTORY.DAT.'

 1118 format(/'################  SA Iteration',i5.4,
     &  '  ################'/)
 9118 format(1x,'Number of acceted moves            =',i10/,
     &       1x,'Number of rejected moves           =',i10/,
     &       1x,'Current Function Value             =',g16.5/,
     &       1x,'Optimal Function Value             =',g16.5/)
