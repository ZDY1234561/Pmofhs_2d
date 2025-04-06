C     Last change:  PW    9 May 103    1:37 pm
C====================================================================
      SUBROUTINE SORTALL(A,NP,N)
C====================================================================
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION A(N), NP(N)
      
      DO I = 1, N
         NP(I)=I
      ENDDO

      DO I = 1, N
         X=A(I)
         Y=X
         K=I
         K1=NP(I)
         K2=K1
         DO J=I+1, N
            IF (A(J).LT.X) THEN
               X=A(J)
               K=J
               K1=NP(J)
            ENDIF
         ENDDO
         A(I)=X
         A(K)=Y
         NP(I)=K1
         NP(K)=K2
      ENDDO
      RETURN
      END

C====================================================================
      SUBROUTINE SORT1(IP,A,N)
C====================================================================
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER A,XMIN
      DIMENSION A(N)
      XMIN=A(1)
      IP=1
      DO I = 2, N
         IF (A(I).LT.XMIN) THEN
            XMIN=A(I)
            IP=I
         ENDIF
      ENDDO
      RETURN
      END

C====================================================================
      SUBROUTINE UPDATEHIST(FUNCVAL,IY,NP,HISTOBJ,HISTORY,NPOPSIZ,
     & NRUN,LDBG)
C====================================================================
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER HISTORY(NPOPSIZ,NP),IY(NP)
      DIMENSION HISTOBJ(NPOPSIZ)
      IF(NRUN.LE.0.OR.NRUN.GT.NPOPSIZ) THEN
         WRITE(*,*) 'ERROR: IN WRITING AT SUBROUTINE UPDATEHIST!'
         WRITE(*,*) 'NUMBER OF RUNS:      ', NRUN
         WRITE(*,*) 'DIMENSION OF HISTORY:', NPOPSIZ
         STOP ' INCREASE ARRAY SIZE HISTORY IN MAIN PROGRAM.'
      ENDIF
      DO N=1,NP
         HISTORY(NRUN,N)=IY(N)
      ENDDO
      HISTOBJ(NRUN)=FUNCVAL

      IF (LDBG.GE.3) THEN
         WRITE(*,FMT="(I6,50I3)") NRUN,(IY(N),N=1,NP)
         WRITE(*,FMT="(F16.6)") FUNCVAL
      ENDIF
100   FORMAT(1X,I6,2I6)
199   FORMAT(1X,I6,4I4,F20.6)
      RETURN
      END

C====================================================================
      SUBROUTINE FINDHIST(FIND,FUNCVAL,IY,NP,W,HISTORY,NPOPSIZ,
     & MAXNP,NRUN)
C====================================================================

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER HISTORY(NPOPSIZ,MAXNP),IY(MAXNP)
      DIMENSION W(NPOPSIZ)

      LOGICAL FIND

      FIND=.FALSE.
      I=0
100   I=I+1
      IF(I.LE.NRUN) THEN
         DO N=1,NP
            IF(HISTORY(I,N).NE.IY(N)) GOTO 100
         ENDDO 
         FIND=.TRUE.
         FUNCVAL=W(I)
      ENDIF
      RETURN
      END

C====================================================================
      SUBROUTINE CHCKCYCL(FIND,NC,W,ITER,MINCYCLE,MAXCYCLE)
C====================================================================
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION W(ITER)
      LOGICAL FIND

      MAXCYCLE=MIN(MAXCYCLE,INT(ITER/2))
      NCMIN=MIN(MINCYCLE,MAXCYCLE)
      NCMAX=MAX(MINCYCLE,MAXCYCLE)

      FIND=.FALSE.

      PRINT*,ITER,NCMIN,NCMAX

      DO NC = NCMIN,NCMAX  ! A CYCLE MAY START WITH NSIZE
         IF(2*NC.GT.ITER) RETURN   ! DO NOT HAVE ENOUGH DATA
         DSUM=0.D0
         DO I = MAX(NC,ITER-NC+1), ITER
            DSUM=DSUM+(W(I)-W(I-NC))**2
         END DO
         IF (DSUM.LE.1.D-7) THEN
            FIND=.TRUE.
            EXIT                  ! EXIT DO LOOP
         END IF
      ENDDO
      RETURN
      END
c
c
c#######################################################################
c    
      Subroutine INITIALP(nparam,p,nper,mxwell,nwells0,well)
C
C!!! This subroutine reads well files and assigns to p
C!!! warning: it only works for one period.
C
      implicit double precision (a-h,o-z)
      real     well
      include 'mgo.inc'

      dimension p(2*nparam),well(5,mxwell,nper),nwells0(nper)
c

      do ns=1,nper
        do nw=1,nwells0(ns)

          np=well(5,nw,ns)
          if(np.eq.0) cycle

!!          QW=p(np)*pscale
!!          well(4,nw,ns)=QW
          ! READ WELL LOCATION FROM WELL FILE
          KWELL=WELL(1,NW,NS)
          IWELL=WELL(2,NW,NS)
          JWELL=WELL(3,NW,NS)
!!
          DO IPARAM=1,NPARAM
          ! READ WELL LOCATION FROM INPUT FILE [.INP]
             KP=pmodflow(4,IPARAM)
             IP=pmodflow(5,IPARAM)
             JP=pmodflow(6,IPARAM)
             NSTART=pmodflow(7,iParam) ! STARTING PERIOD
             NSTOP =pmodflow(8,iParam) ! ENDING PERIOD

             if(KP.eq.KWELL.and.IP.eq.IWELL.and.JP.eq.JWELL !) THEN
     &         .AND.NSTART.LE.NS.AND.NSTOP.GE.NS) THEN !! unfinished???
                ! FOUND A MATCH AND ASSIGN PUMPING RATE
                P(IPARAM)=well(4,nw,ns)/PSCALE
                ! IDENTIFY WELL RELATIVE LOCATION
                IF(NACTIVE.LT.0) THEN
                   KS=PMODFLOW(12,IPARAM)
                   IS=PMODFLOW(13,IPARAM)
                   JS=PMODFLOW(14,IPARAM)
                   KE=PMODFLOW(15,IPARAM)
                   IE=PMODFLOW(16,IPARAM)
                   JE=PMODFLOW(17,IPARAM)
                   !RELATIVE WELL LOCATION
                   KIJ=(JP-JS+1)
     &                +(JE-JS+1)*(IP-IS)
     &                +(JE-JS+1)*(IE-IS+1)*(KP-KS)
                   !ASSIGN RELATIVE WELL LOCATION
                   P(IPARAM+NPARAM)=KIJ
                ELSEIF(NACTIVE.GT.0) THEN
                   !ASSIGN ON/OFF INDICATOR PW 5-8-03
                   IF(ABS(P(IPARAM)).GT.0.0) THEN
                      P(IPARAM+NPARAM)=1
                   ELSE
                      P(IPARAM+NPARAM)=0
                   ENDIF
                ENDIF

             ENDIF
          enddo
        ENDDO
      enddo
c
C NORMAL RETURN
c
      return
      end

C====================================================================
      SUBROUTINE NEWSOL(IY,NP,HISTOBJ,HISTORY,NPOPSIZ,
     & NRUN,MINMAX)
C====================================================================
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER HISTORY(NPOPSIZ,NP),IY(NP),NC

      DIMENSION HISTOBJ(NPOPSIZ),V(:),NC(:)

      ALLOCATABLE :: V,NC

      IF(NRUN.LE.0.OR.NRUN.GT.NPOPSIZ) THEN
         WRITE(*,*) 'ERROR: IN SUBROUTINE NEWSOL!'
         WRITE(*,*) 'NUMBER OF RUNS:      ', NRUN
         WRITE(*,*) 'DIMENSION OF HISTORY:', NPOPSIZ
         STOP ' INCREASE ARRAY SIZE HISTORY IN MAIN PROGRAM.'
      ENDIF

      MAXVAL=1
      DO N=1,NP
         DO I=1,NRUN
            IF(HISTORY(I,N).GT.MAXVAL) MAXVAL=HISTORY(I,N)
         ENDDO
      ENDDO

      ALLOCATE (V(MAXVAL),STAT=IERR)
      IF(IERR.NE.0) STOP 'ERROR: -- IN ALLOCATING MEMORY TO NEWSOL.'
      ALLOCATE (NC(MAXVAL),STAT=IERR)
      IF(IERR.NE.0) STOP 'ERROR: -- IN ALLOCATING MEMORY TO NEWSOL.'

      DO N=1,NP
         V=0.0
         NC=0
         DO I=1,NRUN
            INDX=HISTORY(I,N)
            V(INDX)=V(INDX)+HISTOBJ(I)
            NC(INDX)=NC(INDX)+1
         ENDDO

         VMIN=1.E30
         NVMIN=1
         DO I=1,MAXVAL
            IF(NC(I).GT.0) THEN
!               WRITE(*,*) I,V(I),NC(I)
               V(I)=V(I)/NC(I)
               IF((MINMAX.EQ.1.AND.V(I).LT.VMIN) .OR.
     &            (MINMAX.EQ.2.AND.V(I).GT.VMIN)) THEN
                  VMIN=V(I)
                  NVMIN=I
               ENDIF
            ENDIF
         ENDDO
         IY(N)=NVMIN
      ENDDO

      RETURN
100   FORMAT(1X,I6,2I6)

      END

C====================================================================
      SUBROUTINE SCREEN(FIND,FUNCVAL,IY,NP,W,HISTORY,NDIMH,
     & MAXNP,NRUN)
C====================================================================
                     
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER HISTORY(NDIMH,MAXNP),IY(MAXNP)
      DIMENSION W(NDIMH)

      LOGICAL FIND
C
C IT PRESCREENS OUT POOR SOLUTIONS
C IF P IS INFEASIBLE AND Q<P THEN Q IS ALSO INFEASIBLE.
C IF P IS FEASIBLE AND Q>P THEN Q IS ALSO FEASIBLE.
C THIS METHOD IS PARTICULARLY DESIGNED FOR VARIABLE PUMPING RATES
C
C P. WANG 6-17-2002

      FIND=.FALSE.

      DO I=1,NRUN

         CALL PREFUNC(FUNCVAL,HISTORY(I,1:NP),NP)
C
         IF(ABS(FUNCVAL-W(I)).LE.1.E-5) THEN
C
C           CHECK FEASIBILITY

            DO N=1,NP
               IF(IY(N).GT.HISTORY(I,N)) GOTO 100
            ENDDO
            FIND=.TRUE.
            CALL PREFUNC(FUNCVAL,IY,NP)
            write(*,*) 'it is feasible covered at',i
            RETURN
         ELSE
C
C           CHECK INFEASIBILITY
            DO N=1,NP
               IF(IY(N).LT.HISTORY(I,N)) GOTO 100
            ENDDO
            FIND=.TRUE.
            FUNCVAL=W(I)   !!!!-FUNCVAL+FUNCVAL1
!cz         write(*,*) 'it is infeasible covered at',i
            RETURN
         ENDIF
100      CONTINUE
      ENDDO
      RETURN
      END
C====================================================================
      SUBROUTINE PREFUNC(FUNCVAL,IY,NP)
C====================================================================
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER IY(NP)
      INCLUDE 'MGO.INC'
      FUNCVAL=0.0
      DO N=1,NP
         PARDEL=PMODFLOW(2,N)-PMODFLOW(1,N)
         NPOSIBL=PMODFLOW(3,N)
         DX=0.D0
         IF (NPOSIBL.GT.1) DX=PARDEL/(NPOSIBL-1.D0)
         Y = PMODFLOW(1,N)+(IY(N)-1.D0)*DX
         COEF=PMODFLOW(10,N)
         FUNCVAL=FUNCVAL+ABS(Y)*COEF
      ENDDO
      RETURN
      END
