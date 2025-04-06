C
C     ****************************************************************
C
      Subroutine MODSUB1(ioptype,nparam,lenx,isum,x,iskip)
C
C     MAIN SUBROUTINE 1 FOR MODFLOW
C     Purpose: read and prepare model input information
C
C     ioptype: type of optimization
C              =1 management optimization
C              =2 parameter estimation
C     nparam: number of wells to be optimized for ioptype=1
C
C     ///
C     /// PCG2 changed from iunit(13) to iunit(15) for GWV
C     ///
C
C     ****************************************************************
C

      include 'modflow.inc'
	integer irandom
	character*80 flname1
	common /rand  / irandom,imodel 
      dimension x(lenx)


      if(iskip.ne.0) goto 123
C
C--ASSIGN BASIC INPUT UNIT AND PRINTER UNIT.
      INBAS=1
      IOUT=6
C
C--OPEN MAIN INPUT & OUTPUT FILES
      FINDEX='[Modflow] Standard Output File: '
      ISTAT=0
	open(iout,file='ex2a.mod')
c      CALL UOPFIL(IOUT,ISTAT,FLNAME,0,FINDEX)
      FINDEX='BAS Package Input File: '
      ISTAT=1

	flname1='bas??/ex2a?????.bas'
      write(flname1(11:15),'(i5.5)') irandom
	write(flname1(4:5),'(i2.2)') imodel
	open(inbas,file=flname1,status='old')
c      CALL UOPFIL(INBAS,ISTAT,FLNAME,0,FINDEX)
C
C--DEFINE PROBLEM__ROWS,COLUMNS,LAYERS,STRESS PERIODS,PACKAGES
      CALL BAS1DF(ISUM,HEADNG,NPER,ITMUNI,TOTIM,NCOL,NROW,NLAY,
     1             NODES,INBAS,IOUT,IUNIT)

      nnper=nper
      if(nnper.gt.maxsp)
     & Stop 'Redimension Maximum Number of Stress Periods [maxsp]'

C
C--OPEN INPUT FILES FOR VARIOUS PACKAGES

      IF(IUNIT(1).GT.0) THEN
	  flname1='bcf??/ex2a?????.bcf'
        write(flname1(11:15),'(i5.5)') irandom
	  write(flname1(4:5),'(i2.2)') imodel

        FINDEX='BCF Package Input File: '
        ISTAT=1
	open(iunit(1),file=flname1,status='old')
c        CALL UOPFIL(IUNIT(1),ISTAT,FLNAME,0,FINDEX)
      ENDIF
      IF(IUNIT(2).GT.0) THEN
        FINDEX='WEL Package Input File: '
        ISTAT=1
	open(iunit(2),file='ex2a.wel',status='old')
c        CALL UOPFIL(IUNIT(2),ISTAT,FLNAME,0,FINDEX)
      ENDIF
      IF(IUNIT(3).GT.0) THEN
        FINDEX='DRN Package Input File: '
        ISTAT=1
        CALL UOPFIL(IUNIT(3),ISTAT,FLNAME,0,FINDEX)
      ENDIF
      IF(IUNIT(4).GT.0) THEN
        FINDEX='RIV Package Input File: '
        ISTAT=1
        CALL UOPFIL(IUNIT(4),ISTAT,FLNAME,0,FINDEX)
      ENDIF
      IF(IUNIT(14).GT.0) THEN
        FINDEX='STR Package Input File: '
        ISTAT=1
        CALL UOPFIL(IUNIT(14),ISTAT,FLNAME,0,FINDEX)
      ENDIF
      IF(IUNIT(5).GT.0) THEN
        FINDEX='EVT Package Input File: '
        ISTAT=1
        CALL UOPFIL(IUNIT(5),ISTAT,FLNAME,0,FINDEX)
      ENDIF
      IF(IUNIT(7).GT.0) THEN
        FINDEX='GHB Package Input File: '
        ISTAT=1
        CALL UOPFIL(IUNIT(7),ISTAT,FLNAME,0,FINDEX)
      ENDIF
      IF(IUNIT(8).GT.0) THEN
        FINDEX='RCH Package Input File: '
        ISTAT=1
        CALL UOPFIL(IUNIT(8),ISTAT,FLNAME,0,FINDEX)
      ENDIF

      IF(IUNIT(16).GT.0) THEN
        FINDEX='HFB Package Input File: '
        ISTAT=1
        CALL UOPFIL(IUNIT(16),ISTAT,FLNAME,0,FINDEX)
      ENDIF

      IF(IUNIT(20).GT.0) THEN
        FINDEX='CHD Package Input File: '
        ISTAT=1
        CALL UOPFIL(IUNIT(20),ISTAT,FLNAME,0,FINDEX)
      ENDIF
      IF(IUNIT(9).GT.0) THEN
        FINDEX='SIP Package Input File: '
        ISTAT=1
	open(iunit(9),file='ex2a.sip',status='old')
c        CALL UOPFIL(IUNIT(9),ISTAT,FLNAME,0,FINDEX)
      ENDIF
      IF(IUNIT(11).GT.0) THEN
        FINDEX='SOR Package Input File: '
        ISTAT=1
        CALL UOPFIL(IUNIT(11),ISTAT,FLNAME,0,FINDEX)
      ENDIF
      IF(IUNIT(15).GT.0) THEN
        FINDEX='PCG Package Input File: '
        ISTAT=1
        CALL UOPFIL(IUNIT(15),ISTAT,FLNAME,0,FINDEX)
      ENDIF
      IF(IUNIT(12).GT.0) THEN
        FINDEX='OpC Options Input File: '
        ISTAT=1
	open(iunit(12),file='ex2a.oc',status='old')
c        CALL UOPFIL(IUNIT(12),ISTAT,FLNAME,0,FINDEX)
      ENDIF
C
C--ALLOCATE SPACE IN "X" ARRAY.
      CALL BAS1AL(ISUM,LENX,LCHNEW,LCHOLD,LCIBOU,LCCR,LCCC,LCCV,
     1              LCHCOF,LCRHS,LCDELR,LCDELC,LCSTRT,LCBUFF,LCIOFL,
     2              INBAS,ISTRT,NCOL,NROW,NLAY,IOUT)

      LCh0=isum
      isum=isum+ncol*nrow*nlay
      LCib0=isum
      isum=isum+ncol*nrow*nlay
      LCcr0=isum
      isum=isum+ncol*nrow*nlay
      LCcc0=isum
      isum=isum+ncol*nrow*nlay
      LCcv0=isum
      isum=isum+ncol*nrow*(nlay-1)

      IF(IUNIT(1).GT.0) CALL BCF2AL(ISUM,LENX,LCSC1,LCHY,
     1     LCBOT,LCTOP,LCSC2,LCTRPY,IUNIT(1),ISS,
     2     NCOL,NROW,NLAY,IOUT,IBCFCB,LCWETD,IWDFLG,LCCVWD,
     3     WETFCT,IWETIT,IHDWET,HDRY)
      IF(IUNIT(2).GT.0) then
        CALL WEL1AL(ISUM,LENX,LCWELL,MXWELL,NWELLS,
     1                 IUNIT(2),IOUT,IWELCB)
        LCWELL0=LCWELL
      ENDIF
      IF(IUNIT(3).GT.0) then
        CALL DRN1AL(ISUM,LENX,LCDRAI,NDRAIN,MXDRN,
     1                 IUNIT(3),IOUT,IDRNCB)
        LCDRAI0=LCDRAI
      ENDIF
      IF(IUNIT(8).GT.0) then
        CALL RCH1AL(ISUM,LENX,LCIRCH,LCRECH,NRCHOP,
     1            NCOL,NROW,IUNIT(8),IOUT,IRCHCB)
        LCIRCH0=LCIRCH
        LCRECH0=LCRECH
      ENDIF
      IF(IUNIT(5).GT.0) then
        CALL EVT1AL(ISUM,LENX,LCIEVT,LCEVTR,LCEXDP,
     1            LCSURF,NCOL,NROW,NEVTOP,IUNIT(5),IOUT,IEVTCB)
        LCIEVT0=LCIEVT
        LCEVTR0=LCEVTR
        LCEXDP0=LCEXDP
        LCSURF0=LCSURF
      ENDIF
      IF(IUNIT(4).GT.0) then
        CALL RIV1AL(ISUM,LENX,LCRIVR,MXRIVR,NRIVER,
     1            IUNIT(4),IOUT,IRIVCB)
        LCRIVR0=LCRIVR
      ENDIF
      IF(IUNIT(14).GT.0) then
        CALL STR1AL(ISUM,LENX,LCSTRM,ICSTRM,MXSTRM,
     1                 NSTREM,IUNIT(14),IOUT,ISTCB1,ISTCB2,NSS,NTRIB,
     2                  NDIV,ICALC,CONST,LCTBAR,LCTRIB,LCIVAR,LCFGAR)
        LCSTRM0=LCSTRM
        ICSTRM0=ICSTRM
        LCTBAR0=LCTBAR
        LCIVAR0=LCIVAR
      ENDIF
      IF(IUNIT(7).GT.0) then
        CALL GHB1AL(ISUM,LENX,LCBNDS,NBOUND,MXBND,
     1            IUNIT(7),IOUT,IGHBCB)
        LCBNDS0=LCBNDS
      ENDIF
      IF(IUNIT(9).GT.0) CALL SIP1AL(ISUM,LENX,LCEL,LCFL,LCGL,LCV,
     1          LCHDCG,LCLRCH,LCW,MXITER,NPARM,NCOL,NROW,NLAY,
     2          IUNIT(9),IOUT)
      IF(IUNIT(11).GT.0) CALL SOR1AL(ISUM,LENX,LCA,LCRES,LCHDCG,LCLRCH,
     1           LCIEQP,MXITER,NCOL,NLAY,NSLICE,MBW,IUNIT(11),IOUT)
      IF(IUNIT(15).GT.0) CALL PCG2AL(ISUM,LENX,LCV,LCSS,LCP,LCCD,
     1       LCHCHG,LCLHCH,LCRCHG,LCLRCH,MXITER,ITER1,NCOL,NROW,NLAY,
     2       IUNIT(15),IOUT,NPCOND,LCIT1)
      IF(IUNIT(20).GT.0) then
        CALL CHD1AL(ISUM,LENX,LCCHDS,NCHDS,MXCHD,
     1           IUNIT(20),IOUT)
        LCCHDS0=LCCHDS
      ENDIF
      IF(IUNIT(16).GT.0) CALL HFB1AL(ISUM,LENX,LCHFBR,NHFB,IUNIT(16),
     1           IOUT)
C
C--IF THE "X" ARRAY IS NOT BIG ENOUGH THEN STOP.
  123 IF(ISUM-1.GT.LENX) return !stop
C
C--READ AND PREPARE INFORMATION FOR ENTIRE SIMULATION.
      rewind(IUNIT(12))
      CALL BAS1RP(X(LCIBOU),X(LCHNEW),X(LCSTRT),X(LCHOLD),
     1       ISTRT,INBAS,HEADNG,NCOL,NROW,NLAY,NODES,VBVL,X(LCIOFL),
     2       IUNIT(12),IHEDFM,IDDNFM,IHEDUN,IDDNUN,IOUT)
      IF(IUNIT(1).GT.0) CALL BCF2RP(X(LCIBOU),X(LCHNEW),X(LCSC1),
     1          X(LCHY),X(LCCR),X(LCCC),X(LCCV),X(LCDELR),
     2     X(LCDELC),X(LCBOT),X(LCTOP),X(LCSC2),X(LCTRPY),IUNIT(1),
     3     ISS,NCOL,NROW,NLAY,NODES,IOUT,X(LCWETD),IWDFLG,X(LCCVWD))
      IF(IUNIT(16).GT.0) CALL HFB1RP(X(LCCR),X(LCCC),X(LCDELR),
     1         X(LCDELC),X(LCHFBR),IUNIT(16),NCOL,NROW,NLAY,NODES,
     1         NHFB,IOUT)
      IF(IUNIT(9).GT.0) CALL SIP1RP(NPARM,MXITER,ACCL,HCLOSE,X(LCW),
     1          IUNIT(9),IPCALC,IPRSIP,IOUT)
      IF(IUNIT(11).GT.0) CALL SOR1RP(MXITER,ACCL,HCLOSE,IUNIT(11),
     1         IPRSOR,IOUT)
      IF(IUNIT(15).GT.0) CALL PCG2RP(MXITER,ITER1,HCLOSE,RCLOSE,
     1         NPCOND,NBPOL,RELAX,IPRPCG,IUNIT(15),IOUT,MUTPCG,
     2         NITER,X(LCIT1),DAMP)
C
C--OPEN OPTIONAL UNFORMATTED FILES
      IF(IHEDUN.GT.0) THEN
        FINDEX='Unformatted Head File: '
        ISTAT=0
        ITMP=-IHEDUN
	open(abs(ITMP),file='ex2a.ufh',form='BINARY')
c        CALL UOPFIL(ITMP,ISTAT,FLNAME,0,FINDEX)
      ENDIF
      IF(IDDNUN.GT.0) THEN
        FINDEX='Unformatted Drawdown File: '
        ISTAT=0
        ITMP=-IDDNUN
        CALL UOPFIL(ITMP,ISTAT,FLNAME,0,FINDEX)
      ENDIF
      IF(IBCFCB.GT.0) THEN
        FINDEX='Unformatted Flow File: '
        ISTAT=0
        ITMP=-IBCFCB
        CALL UOPFIL(ITMP,ISTAT,FLNAME,0,FINDEX)
      ENDIF
      IF(IUNIT(22).GT.0) THEN
        FINDEX='Modflow-MT3D Link File: '
        ISTAT=0
        ITMP=-IUNIT(22)
	open(abs(ITMP),file='ex2a.ftl',form='BINARY')
c        CALL UOPFIL(ITMP,ISTAT,FLNAME,0,FINDEX)
      ENDIF
C
C--READ AND PREPARE INFORMATION FOR EACH STRESS PERIOD
      DO 300 KPER=1,NPER
        kkper=KPER
C
C--READ STRESS PERIOD TIMING INFORMATION.
C
        CALL BAS1ST(NSTP,DELT,TSMULT,PERTIM,kkper,INBAS,IOUT)
        nstp0(kkper)=NSTP
        delt0(kkper)=DELT
        tsmult0(kkper)=TSMULT
        perlen0(kkper)=DELT*FLOAT(NSTP)
        if(tsmult.ne.1) then
          perlen0(kkper)=DELT*(1.-TSMULT**NSTP)/(1.-TSMULT)
        endif
C
C--READ SINKS/SOURCES FOR CURRENT STRESS PERIOD
        IF(IUNIT(2).GT.0) then
          LCWELL=LCWELL0+(kkper-1)*MXWELL*5
          CALL WEL1RP(X(LCWELL),NWELLS,MXWELL,IUNIT(2),IOUT)
          nwells0(kkper)=NWELLS
        ENDIF
        IF(IUNIT(3).GT.0) then
          LCDRAI=LCDRAI0+(kkper-1)*MXDRN*5
          CALL DRN1RP(X(LCDRAI),NDRAIN,MXDRN,IUNIT(3),IOUT)
          ndrain0(kkper)=NDRAIN
        ENDIF
        IF(IUNIT(8).GT.0) then
          if(NRCHOP.eq.2) LCIRCH=LCIRCH0+(kkper-1)*NCOL*NROW
          LCRECH=LCRECH0+(kkper-1)*NCOL*NROW
          CALL RCH1RP(NRCHOP,X(LCIRCH),X(LCRECH),
     1            X(LCDELR),X(LCDELC),NROW,NCOL,IUNIT(8),IOUT)
        ENDIF
        IF(IUNIT(5).GT.0) then
          if(NEVTOP.eq.2) LCIEVT=LCIEVT0+(kkper-1)*NCOL*NROW
          LCEVTR=LCEVTR0+(kkper-1)*NCOL*NROW
          LCEXDP=LCEXDP0+(kkper-1)*NCOL*NROW
          LCSURF=LCSURF0+(kkper-1)*NCOL*NROW
          CALL EVT1RP(NEVTOP,X(LCIEVT),X(LCEVTR),
     1         X(LCEXDP),X(LCSURF),X(LCDELR),X(LCDELC),NCOL,NROW,
     2         IUNIT(5),IOUT)
        ENDIF
        IF(IUNIT(4).GT.0) then
          LCRIVR=LCRIVR0+(kkper-1)*MXRIVR*6
          CALL RIV1RP(X(LCRIVR),NRIVER,MXRIVR,IUNIT(4),IOUT)
          nriver0(kkper)=NRIVER
        ENDIF
        IF(IUNIT(14).GT.0) then
          LCSTRM=LCSTRM0+(kkper-1)*MXSTRM*11
          ICSTRM=ICSTRM0+(kkper-1)*MXSTRM*5
          LCTBAR=LCTBAR0+(kkper-1)*NSS*NTRIB
          LCIVAR=LCIVAR0+(kkper-1)*NSS
          CALL STR1RP(X(LCSTRM),X(ICSTRM),NSTREM,
     1                   MXSTRM,IUNIT(14),IOUT,X(LCTBAR),NDIV,NSS,
     2                   NTRIB,X(LCIVAR),ICALC,IPTFLG)
          nstrem0(kkper)=NSTREM
          iptflg0(kkper)=IPTFLG
        ENDIF
        IF(IUNIT(7).GT.0) then
          LCBNDS=LCBNDS0+(kkper-1)*MXBND*5
          CALL GHB1RP(X(LCBNDS),NBOUND,MXBND,IUNIT(7),IOUT)
          nbound0(kkper)=NBOUND
        ENDIF
        IF(IUNIT(20).GT.0) then
          LCCHDS=LCCHDS0+(kkper-1)*MXCHD*5
          CALL CHD1RP(X(LCCHDS),NCHDS,MXCHD,X(LCIBOU),
     1        NCOL,NROW,NLAY,PERLEN,DELT,NSTP,TSMULT,IUNIT(20),IOUT)
          nchds0(kkper)=NCHDS
        ENDIF

C
C--COPY CURRENT STRESS PERIOD TO NEXT ONE FOR REUSE PURPOSES
        IF(KKPER.EQ.NPER) GOTO 300
C
        IF(IUNIT(2).GT.0) then
          ip1=LCWELL
          ip2=LCWELL+MXWELL*5
          do n=0,NWELLS*5-1
            x(ip2+n)=x(ip1+n)
          enddo
        ENDIF
        IF(IUNIT(3).GT.0) then
          ip1=LCDRAI
          ip2=LCDRAI+MXDRN*5
          do n=0,NDRAIN*5-1
            x(ip2+n)=x(ip1+n)
          enddo
        ENDIF
        IF(IUNIT(8).GT.0) then
          if(NRCHOP.eq.2) then
            ip1=LCIRCH
            ip2=LCIRCH+NCOL*NROW
            do n=0,NCOL*NROW-1
              x(ip2+n)=x(ip1+n)
            enddo
          endif
          ip1=LCRECH
          ip2=LCRECH+NCOL*NROW
          do n=0,NCOL*NROW-1
            x(ip2+n)=x(ip1+n)
          enddo
        ENDIF
        IF(IUNIT(5).GT.0) then
          if(NEVTOP.eq.2) then
            ip1=LCIEVT
            ip2=LCIEVT+NCOL*NROW
            do n=0,NCOL*NROW-1
              x(ip2+n)=x(ip1+n)
            enddo
          endif
          ip1=LCEVTR
          ip2=LCEVTR+NCOL*NROW
          do n=0,NCOL*NROW-1
            x(ip2+n)=x(ip1+n)
          enddo
          ip1=LCEXDP
          ip2=LCEXDP+NCOL*NROW
          do n=0,NCOL*NROW-1
            x(ip2+n)=x(ip1+n)
          enddo
          ip1=LCSURF
          ip2=LCSURF+NCOL*NROW
          do n=0,NCOL*NROW-1
            x(ip2+n)=x(ip1+n)
          enddo
        ENDIF
        IF(IUNIT(4).GT.0) then
          ip1=LCRIVR
          ip2=LCRIVR+MXRIVR*6
          do n=0,NRIVER*6-1
            x(ip2+n)=x(ip1+n)
          enddo
        ENDIF
        IF(IUNIT(14).GT.0) then
          ip1=ICSTRM
          ip2=LCSTRM+MXSTRM*11
          do n=0,NSTREM*11-1
            x(ip2+n)=x(ip1+n)
          enddo
          ip1=ICSTRM
          ip2=ICSTRM+MXSTRM*5
          do n=0,NSTREM*5-1
            x(ip2+n)=x(ip1+n)
          enddo
          ip1=LCTBAR
          ip2=LCTBAR+NSS*NTRIB
          do n=0,NSS*NTRIB-1
            x(ip2+n)=x(ip1+n)
          enddo
          ip1=LCIVAR
          ip2=LCIVAR+NSS
          do n=0,NSS-1
            x(ip2+n)=x(ip1+n)
          enddo
        ENDIF
        IF(IUNIT(7).GT.0) then
          ip1=LCBNDS
          ip2=LCBNDS+MXBND*5
          do n=0,NBOUND*5-1
            x(ip2+n)=x(ip1+n)
          enddo
        ENDIF
        IF(IUNIT(20).GT.0) then
          ip1=LCCHDS
          ip2=LCCHDS+MXCHD*5
          do n=0,NCHDS*5-1
            x(ip2+n)=x(ip1+n)
          enddo
        ENDIF
C
  300 CONTINUE
C
c--store arrays that may be changed during simulation
      call Backup(ncol,nrow,nlay,x(lcibou),x(lcib0),
     & x(lchold),x(lch0),x(lccr),x(lccr0),
     & x(lccc),x(lccc0),x(lccv),x(lccv0))


c--link optimization parameter numbers with modflow wells
      if(ioptype.eq.1) then
        call AssignP(nparam,nper,mxwell,nwells0,x(lcwell0))
      endif

c
c--close files no longer needed
      close (inbas)
      do iu=1,24
        if(iunit(iu).gt.0) then
          if(iu.ne.12.and.iu.ne.22) then
            close (iunit(iu))
          endif
        endif
      enddo
c
      RETURN
      END
C
C
C     ****************************************************************
C
      Subroutine MODSUB2(ioptype,ncnvg,kkiter,lenx,x)
C
C     MAIN SUBROUTINE 2 FOR MODFLOW
C     Purpose: complete one modflow simulation
C
C     ioptype: type of optimization
C              =1 management optimization
C              =2 parameter estimation
C     ncnvg:   number of times modflow does not converge
C     kkiter:  final iteration number for each converged solution
C
C     ****************************************************************
C

      include 'modflow.inc'
      dimension x(lenx)
	real dd
	dimension dd(100,100,1)
	common / drawdown / dd

C
C--SIMULATE EACH STRESS PERIOD.
      DO 300 KPER=1,NPER
      kkper=KPER
C
C--assign stress period timing information

      PERTIM=0
      DELT=delt0(kkper)
      TSMULT=tsmult0(kkper)
      NSTP=nstp0(kkper)
C
C--locate sink/source information for each stress period

      IF(IUNIT(20).GT.0) then
        LCCHDS=LCCHDS0+(kkper-1)*MXCHD*5
        NCHDS=nchds0(kkper)
      ENDIF

      IF(IUNIT(2).GT.0) then
        LCWELL=LCWELL0+(kkper-1)*MXWELL*5
        NWELLS=nwells0(kkper)
      ENDIF

      IF(IUNIT(3).GT.0) then
        LCDRAI=LCDRAI0+(kkper-1)*MXDRN*5
        NDRAIN=ndrain0(kkper)
      ENDIF

      IF(IUNIT(8).GT.0) then
        LCIRCH=LCIRCH0
        if(NRCHOP.eq.2) LCIRCH=LCIRCH0+(kkper-1)*NCOL*NROW
        LCRECH=LCRECH0+(kkper-1)*NCOL*NROW
      ENDIF

      IF(IUNIT(5).GT.0) then
        LCIEVT=LCIEVT0
        if(NEVTOP.eq.2) LCIEVT=LCIEVT0+(kkper-1)*NCOL*NROW
        LCEVTR=LCEVTR0+(kkper-1)*NCOL*NROW
        LCEXDP=LCEXDP0+(kkper-1)*NCOL*NROW
        LCSURF=LCSURF0+(kkper-1)*NCOL*NROW
      ENDIF

      IF(IUNIT(4).GT.0) then
        LCRIVR=LCRIVR0+(kkper-1)*MXRIVR*6
        NRIVER=nriver0(kkper)
      ENDIF

      IF(IUNIT(14).GT.0) then
        LCSTRM=LCSTRM0+(kkper-1)*MXSTRM*11
        ICSTRM=ICSTRM0+(kkper-1)*MXSTRM*5
        LCTBAR=LCTBAR0+(kkper-1)*NSS*NTRIB
        LCIVAR=LCIVAR0+(kkper-1)*NSS
        NSTREM=nstrem0(kkper)
        IPTFLG=iptflg0(kkper)
      ENDIF

      IF(IUNIT(7).GT.0) then
        LCBNDS=LCBNDS0+(kkper-1)*MXBND*5
        NBOUND=nbound0(kkper)
      ENDIF

C
C--SIMULATE EACH TIME STEP.
      DO 200 KSTP=1,NSTP
      KKSTP=KSTP
C
C--CALCULATE TIME STEP LENGTH. SET HOLD=HNEW..
      CALL BAS1AD(DELT,TSMULT,TOTIM,PERTIM,X(LCHNEW),X(LCHOLD),KKSTP,
     1             NCOL,NROW,NLAY)
      IF(IUNIT(20).GT.0) CALL CHD1FM(NCHDS,MXCHD,X(LCCHDS),X(LCIBOU),
     1          X(LCHNEW),X(LCHOLD),PERLEN,PERTIM,DELT,NCOL,NROW,NLAY)
      IF(IUNIT(1).GT.0) CALL BCF2AD(X(LCIBOU),X(LCHOLD),X(LCBOT),
     1             X(LCWETD),IWDFLG,ISS,NCOL,NROW,NLAY)
C
C--ITERATIVELY FORMULATE AND SOLVE THE EQUATIONS.
      IFLAG=0
      DO 100 KITER=1,MXITER
      KKITER=KITER
C
C--FORMULATE THE FINITE DIFFERENCE EQUATIONS.
      CALL BAS1FM(X(LCHCOF),X(LCRHS),NODES)
      IF(IUNIT(1).GT.0) CALL BCF2FM(X(LCHCOF),X(LCRHS),X(LCHOLD),
     1          X(LCSC1),X(LCHNEW),X(LCIBOU),X(LCCR),X(LCCC),X(LCCV),
     2          X(LCHY),X(LCTRPY),X(LCBOT),X(LCTOP),X(LCSC2),
     3          X(LCDELR),X(LCDELC),DELT,ISS,KKITER,KKSTP,kkper,NCOL,
     4          NROW,NLAY,IOUT,X(LCWETD),IWDFLG,X(LCCVWD),WETFCT,
     5          IWETIT,IHDWET,HDRY)
      IF(IUNIT(16).GT.0) CALL HFB1FM(X(LCHNEW),X(LCCR),X(LCCC),
     1          X(LCBOT),X(LCTOP),X(LCDELR),X(LCDELC),X(LCHFBR),
     2          NCOL,NROW,NLAY,NHFB)
      IF(IUNIT(2).GT.0) CALL WEL1FM(NWELLS,MXWELL,X(LCRHS),X(LCWELL),
     1           X(LCIBOU),NCOL,NROW,NLAY)
      IF(IUNIT(3).GT.0) CALL DRN1FM(NDRAIN,MXDRN,X(LCDRAI),X(LCHNEW),
     1         X(LCHCOF),X(LCRHS),X(LCIBOU),NCOL,NROW,NLAY)
      IF(IUNIT(8).GT.0) CALL RCH1FM(NRCHOP,X(LCIRCH),X(LCRECH),
     1            X(LCRHS),X(LCIBOU),NCOL,NROW,NLAY)
      IF(IUNIT(5).GT.0) CALL EVT1FM(NEVTOP,X(LCIEVT),X(LCEVTR),
     1            X(LCEXDP),X(LCSURF),X(LCRHS),X(LCHCOF),X(LCIBOU),
     1            X(LCHNEW),NCOL,NROW,NLAY)
      IF(IUNIT(4).GT.0) CALL RIV1FM(NRIVER,MXRIVR,X(LCRIVR),X(LCHNEW),
     1            X(LCHCOF),X(LCRHS),X(LCIBOU),NCOL,NROW,NLAY)
      IF(IUNIT(14).GT.0) CALL STR1FM(NSTREM,X(LCSTRM),X(ICSTRM),
     1                     X(LCHNEW),X(LCHCOF),X(LCRHS),X(LCIBOU),
     2              MXSTRM,NCOL,NROW,NLAY,IOUT,NSS,X(LCTBAR),
     3              NTRIB,X(LCTRIB),X(LCIVAR),X(LCFGAR),ICALC,CONST)
      IF(IUNIT(7).GT.0) CALL GHB1FM(NBOUND,MXBND,X(LCBNDS),X(LCHCOF),
     1            X(LCRHS),X(LCIBOU),NCOL,NROW,NLAY)
C
C--MAKE ONE CUT AT AN APPROXIMATE SOLUTION.
      IF(IUNIT(9).GT.0) CALL SIP1AP(X(LCHNEW),X(LCIBOU),X(LCCR),X(LCCC),
     1     X(LCCV),X(LCHCOF),X(LCRHS),X(LCEL),X(LCFL),X(LCGL),X(LCV),
     2     X(LCW),X(LCHDCG),X(LCLRCH),NPARM,KKITER,HCLOSE,ACCL,ICNVG,
     3     KKSTP,KKPER,IPCALC,IPRSIP,MXITER,NSTP,NCOL,NROW,NLAY,NODES,
     4     IOUT)
      IF(IUNIT(11).GT.0) CALL SOR1AP(X(LCHNEW),X(LCIBOU),X(LCCR),
     1     X(LCCC),X(LCCV),X(LCHCOF),X(LCRHS),X(LCA),X(LCRES),X(LCIEQP),
     2     X(LCHDCG),X(LCLRCH),KKITER,HCLOSE,ACCL,ICNVG,KKSTP,KKPER,
     3     IPRSOR,MXITER,NSTP,NCOL,NROW,NLAY,NSLICE,MBW,IOUT)
      IF(IUNIT(15).GT.0) CALL PCG2AP(X(LCHNEW),X(LCIBOU),X(LCCR),
     1      X(LCCC),X(LCCV),X(LCHCOF),X(LCRHS),X(LCV),X(LCSS),X(LCP),
     2      X(LCCD),X(LCHCHG),X(LCLHCH),X(LCRCHG),X(LCLRCH),KKITER,
     3      NITER,HCLOSE,RCLOSE,ICNVG,KKSTP,KKPER,IPRPCG,MXITER,ITER1,
     4      NPCOND,NBPOL,NSTP,NCOL,NROW,NLAY,NODES,RELAX,IOUT,MUTPCG,
     5      0,0,SN,SP,SR,X(LCIT1),DAMP)
C
C--IF CONVERGENCE CRITERION HAS BEEN MET STOP ITERATING.
      IF(IFLAG.EQ.1) GO TO 110
      IF(ICNVG.EQ.1) GO TO 110
  100 CONTINUE
      KITER=MXITER
  110 CONTINUE

      IF(idbg.lt.2) GOTO 150

C
C--DETERMINE WHICH OUTPUT IS NEEDED.
      CALL BAS1OC(NSTP,KKSTP,ICNVG,X(LCIOFL),NLAY,
     1  IBUDFL,ICBCFL,IHDDFL,IUNIT(12),IOUT)
C
C--CALCULATE BUDGET TERMS. SAVE CELL-BY-CELL FLOW TERMS.
      MSUM=1
      IF(IUNIT(1).GT.0) CALL BCF1BD(VBNM,VBVL,MSUM,X(LCHNEW),
     1     X(LCIBOU),X(LCHOLD),X(LCSC1),X(LCCR),X(LCCC),X(LCCV),
     2     X(LCTOP),X(LCSC2),DELT,ISS,NCOL,NROW,NLAY,KKSTP,KKPER,
     3     IBCFCB,ICBCFL,X(LCBUFF),IOUT)
      IF(IUNIT(2).GT.0) CALL WEL1BD(NWELLS,MXWELL,VBNM,VBVL,MSUM,
     1     X(LCWELL),X(LCIBOU),DELT,NCOL,NROW,NLAY,KKSTP,KKPER,IWELCB,
     1     ICBCFL,X(LCBUFF),IOUT)
      IF(IUNIT(3).GT.0) CALL DRN1BD(NDRAIN,MXDRN,VBNM,VBVL,MSUM,
     1     X(LCDRAI),DELT,X(LCHNEW),NCOL,NROW,NLAY,X(LCIBOU),KKSTP,
     2     KKPER,IDRNCB,ICBCFL,X(LCBUFF),IOUT)
      IF(IUNIT(8).GT.0) CALL RCH1BD(NRCHOP,X(LCIRCH),X(LCRECH),
     1     X(LCIBOU),NROW,NCOL,NLAY,DELT,VBVL,VBNM,MSUM,KKSTP,KKPER,
     2     IRCHCB,ICBCFL,X(LCBUFF),IOUT)
      IF(IUNIT(5).GT.0) CALL EVT1BD(NEVTOP,X(LCIEVT),X(LCEVTR),
     1     X(LCEXDP),X(LCSURF),X(LCIBOU),X(LCHNEW),NCOL,NROW,NLAY,
     2     DELT,VBVL,VBNM,MSUM,KKSTP,KKPER,IEVTCB,ICBCFL,X(LCBUFF),IOUT)
      IF(IUNIT(4).GT.0) CALL RIV1BD(NRIVER,MXRIVR,X(LCRIVR),X(LCIBOU),
     1     X(LCHNEW),NCOL,NROW,NLAY,DELT,VBVL,VBNM,MSUM,
     2     KKSTP,KKPER,IRIVCB,ICBCFL,X(LCBUFF),IOUT)
      IF(IUNIT(14).GT.0) CALL STR1BD(NSTREM,X(LCSTRM),X(ICSTRM),
     1   X(LCIBOU),MXSTRM,X(LCHNEW),NCOL,NROW,NLAY,DELT,VBVL,VBNM,MSUM,
     2   KKSTP,KKPER,ISTCB1,ISTCB2,ICBCFL,X(LCBUFF),IOUT,NTRIB,NSS,
     3   X(LCTRIB),X(LCTBAR),X(LCIVAR),X(LCFGAR),ICALC,CONST,IPTFLG)
      IF(IUNIT(7).GT.0) CALL GHB1BD(NBOUND,MXBND,VBNM,VBVL,MSUM,
     1     X(LCBNDS),DELT,X(LCHNEW),NCOL,NROW,NLAY,X(LCIBOU),KKSTP,
     2     KKPER,IGHBCB,ICBCFL,X(LCBUFF),IOUT)

  150 continue

C
C--SAVE HEADS AND CELL-BY-CELL FLOW TERMS FOR USE IN MT3D
      INCLUDE 'lkmt3.inc'

      if(ioptype.eq.2) then
!       call gethobs(kkstp,kkper,ncol,nrow,nlay,x(lchnew),x(lcibou))
      endif

      IF(idbg.lt.2) GOTO 201

C
C--PRINT AND OR SAVE HEADS AND DRAWDOWNS. PRINT OVERALL BUDGET.
      CALL BAS1OT(X(LCHNEW),X(LCSTRT),ISTRT,X(LCBUFF),X(LCIOFL),
     1        MSUM,X(LCIBOU),VBNM,VBVL,KKSTP,KKPER,DELT,
     2        PERTIM,TOTIM,ITMUNI,NCOL,NROW,NLAY,ICNVG,
     3     IHDDFL,IBUDFL,IHEDFM,IHEDUN,IDDNFM,IDDNUN,IOUT)
C
C--IF ITERATION FAILED TO CONVERGE THEN STOP.
  201 IF(ICNVG.EQ.0) THEN
        ncnvg=ncnvg+1  !modified from 'Stop' statement
      ENDIF
C
  200 CONTINUE


      if(ioptype.eq.1) then
        call checkflow(kkstp,kkper,ncol,nrow,nlay,x(lchnew),x(lcibou))
      endif
        call readdd(kper,ncol,nrow,nlay,x(lchnew),dd)

  300 CONTINUE
       close(iout)
C
      RETURN
      END
C
C
C     ****************************************************************
C
      Subroutine RESET(ncol,nrow,nlay,totim,vbvl,
     &                 ibound,ib0,hnew,h0,cr,cr0,cc,cc0,cv,cv0)
C
C     ****************************************************************
C

      real             totim,vbvl,h0,cr,cr0,cc,cc0,cv,cv0
      double precision hnew
      dimension        vbvl(4,20),
     &                 ibound(ncol,nrow,nlay),ib0(ncol,nrow,nlay),
     &                 hnew(ncol,nrow,nlay),h0(ncol,nrow,nlay),
     &                 cr(ncol,nrow,nlay),cr0(ncol,nrow,nlay),
     &                 cc(ncol,nrow,nlay),cc0(ncol,nrow,nlay),
     &                 cv(ncol,nrow,nlay-1),cv0(ncol,nrow,nlay-1)

      totim=0.

      do m=1,4
        do n=1,20
          vbvl(m,n)=0.
        enddo
      enddo

      do k=1,nlay
        do i=1,nrow
          do j=1,ncol
            ibound(j,i,k)=ib0(j,i,k)
            hnew(j,i,k)=h0(j,i,k)
            cr(j,i,k)=cr0(j,i,k)
            cc(j,i,k)=cc0(j,i,k)
            if(k.lt.nlay) cv(j,i,k)=cv0(j,i,k)
          enddo
        enddo
      enddo

      return
      end
C
C
C     ****************************************************************
C
      Subroutine BACKUP(ncol,nrow,nlay,
     &                 ibound,ib0,hold,h0,cr,cr0,cc,cc0,cv,cv0)
C
C     ****************************************************************
C

      real             hold,h0,cr,cr0,cc,cc0,cv,cv0
      dimension        ibound(ncol,nrow,nlay),ib0(ncol,nrow,nlay),
     &                 hold(ncol,nrow,nlay),h0(ncol,nrow,nlay),
     &                 cr(ncol,nrow,nlay),cr0(ncol,nrow,nlay),
     &                 cc(ncol,nrow,nlay),cc0(ncol,nrow,nlay),
     &                 cv(ncol,nrow,nlay-1),cv0(ncol,nrow,nlay-1)

      do k=1,nlay
        do i=1,nrow
          do j=1,ncol
            ib0(j,i,k)=ibound(j,i,k)
            h0(j,i,k)=hold(j,i,k)
            cr0(j,i,k)=cr(j,i,k)
            cc0(j,i,k)=cc(j,i,k)
            if(k.lt.nlay) cv0(j,i,k)=cv(j,i,k)
          enddo
        enddo
      enddo

      return
      end

