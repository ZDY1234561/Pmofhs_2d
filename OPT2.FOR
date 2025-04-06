c
c#######################################################################
c
      subroutine func(igen,j,funcval,nobj,Nnoisy,nparam,lenx,x)
c
      implicit double precision (a-h,o-z)
      save
c
      include 'ga.inc'
      include 'modflow.inc'
      include 'mgo.inc'
      dimension x(lenx)
c
      real rand,pwfunc
	integer irandom 
	real funcvaltem(Maxinoisy),funcvaltem1(Maxinoisy)
!
	real mfun_avg(2,10),mfun_var(2,10)
	real weight(10)

!
      dimension funcval(2,nobjmax)
      dimension parent(indmax,nparmax), p(nparmax)
      dimension iparent(indmax,nchrmax)
	real*8 r,r8_uniform_01_sample

c
      common / ga3    / parent,iparent
	common / rand   / irandom,imodel
	COMMON /funcval2/ pwfunc

C
C
      Nmodel=6
	weight=1.0
	weight(1)=0.1069 
	weight(2)=0.0933
	weight(3)=0.1122
	weight(4)=0.2066
	weight(5)=0.1881
	weight(6)=0.2929
	
 
 
 

 


      do imodel=1,Nmodel
      do inoisy=1,Nnoisy 

        r = r8_uniform_01_sample ( )
	  irandom=int(8000*r)+1 !Choose one relization stochastically
        imodflow=0


        Call ModSUB1(1,nparam,lenx,isum,x,imodflow)  !get the modsub function

c--copy parameter information from 'parent' to 'p'
        ionoff=1                    !控制井是否可以不装,1可以优化,0必须装
	  do np=1,nparam
        if(nactive.gt.0) then
          ionoff=nint(parent(j,np+nparam))
        endif
        parent(j,np)=parent(j,np)*float(ionoff)
        p(np)=parent(j,np)
        if(nactive.eq.-1) p(np+nparam)=nint(parent(j,np+nparam))
      enddo
c--check global minmax constraints  此问题中没有
      do nc=1,ncnstr6

        gmin=cnstr6(npmodmax+1,nc)
        gmax=cnstr6(npmodmax+2,nc)
        gsum=0.
        SumLower=0.
        SumUpper=0.
        do np=1,nparam
          pcoeff=cnstr6(np,nc)
          gsum=gsum+p(np)*pcoeff
          SumLower=SumLower+pmodflow(1,np)*pcoeff
          SumUpper=SumUpper+pmodflow(2,np)*pcoeff
        enddo

        if(gsum.gt.gmax.and.gsum.ne.SumLower) then

          rescale=(gmax-SumLower)/(gsum-SumLower)
          do np=1,nparam
            pcoeff=cnstr6(np,nc)
            if(abs(pcoeff).gt.0) then
            p(np)=pmodflow(1,np)+rescale*(p(np)-pmodflow(1,np))
            parent(j,np)=p(np)
            endif
          enddo

        elseif(gsum.lt.gmin.and.gsum.ne.SumUpper) then

          rescale=(gmin-gsum)/(SumUpper-gsum)
          do np=1,nparam
            pcoeff=cnstr6(np,nc)
            if(abs(pcoeff).gt.0) then
              p(np)=p(np)+rescale*(pmodflow(2,np)-p(np))
              parent(j,np)=p(np)
            endif
          enddo

        endif

      enddo

c
c--update well flow rate and locaion in 'modflow'
      call UpdateP(nparam,p,nper,mxwell,nwells0,x(lcwell0),
     &  x(lctop),x(lcbot),x(lchy),x(lcibou),laycon,ncol,nrow,nlay)
c
c--reset penalty due to violation of flow and transport constraints
      pentsum=0.
c
c--run 'modflow' and check flow constraints
      ncnvg=0
      if(iunit(22).gt.0) rewind(iunit(22))
      call modsub2(1,ncnvg,kkiter,lenx,x)           !call modflow程序
      if(ncnvg.ne.0) write(ioutga,201) ncnvg,j,igen
  201 format(1x,'Modflow fails to converge in',i3,
     & ' time steps for Individual #',i4.4,
     & ' in Generation ',i3)


      if(Ldbg.eq.1) then
        if(iunit(9).gt.0.or.iunit(11).gt.0) then
          dhlast=x(lchdcg+kkiter-1)
          write(*,1000) kkiter
          write(*,1010) real(dhlast)
        elseif(iunit(13).gt.0) then
          dhlast=x(lchchg+niter-1)
          write(*,1000) niter
          write(*,1010) real(dhlast)
        endif
 1000   format(1x,'Number of iterations =',i5)
 1010   format(1x,'Max. head change at last iteration=',g15.7//)
      endif

c
c--run 'mt3d' and check transport constraints, if necessary
      ncnvg=0
      if(Transport.and.iunit(22).gt.0) then
        rewind(iunit(22))
        call mt3dsub(1,ncnvg,1,Ldbg,iunit(22))    !call mt3d程序
      elseif(Transport.and.iunit(22).eq.0) then
        write(*,2011)
        write(ioutga,2011)
 2011   format(/1x,'Error: Unformatted flow-transport link not saved',
     &         /1x,'       by MODFLOW LMT Package.')
        stop
      endif
      if(ncnvg.ne.0) write(ioutga,202) j,igen
  202 format(/1x,'MT3D fails to converge in',i3,
     & ' transport steps for individual #',i4.4,
     & ' in Generation ',i3)

c
c--evaluate obj function                           
      call objf(funcvalue,nparam,p,nper,perlen0,mxwell,nwells0
     +	,x(lcwell0))


      funcvaltem(inoisy)=funcvalue
      funcvaltem1(inoisy)=pwfunc
c
c--reset flow model
      call Reset(ncol,nrow,nlay,totim,vbvl,
     &           x(lcibou),x(lcib0),x(lchnew),x(lch0),x(lccr),
     &               x(lccr0),x(lccc),x(lccc0),x(lccv),x(lccv0))

	  

c######The Second Objective Value##########################
	enddo 

	funcvalsum=0.0   
	funcvalsum1=0.0  
	funcvalvarsum=0.0
	funcvalvarsum1=0.0
	do inoisy=1,Nnoisy 
	funcvalsum=funcvalsum+funcvaltem(inoisy)
      funcvalsum1=funcvalsum1+funcvaltem1(inoisy)
	enddo
      funcvalavg=funcvalsum/Nnoisy 
	funcval1avg=funcvalsum1/Nnoisy 
      do inoisy=1,Nnoisy 
      funcvalvarsum=funcvalvarsum+(funcvaltem(inoisy)-funcvalavg)**2
      funcvalvarsum1=funcvalvarsum1+(funcvaltem1(inoisy)-funcval1avg)**2
	enddo
	funcvalvar=funcvalvarsum/Nnoisy 
      funcval1var=funcvalvarsum1/Nnoisy 

      mfun_avg(1,imodel)=funcvalavg
	mfun_avg(2,imodel)=funcval1avg
	mfun_var(1,imodel)=funcvalvar
	mfun_var(2,imodel)=funcval1var
	enddo
!funcval(2,nobjmax)      
	funcval=0       
      do imodel=1,Nmodel
	funcval(1,1)=funcval(1,1)+mfun_avg(1,imodel)*weight(imodel)
	funcval(1,2)=funcval(1,2)+mfun_avg(2,imodel)*weight(imodel)
	enddo
      do imodel=1,Nmodel
	funcval(2,1)= funcval(2,1)+mfun_var(1,imodel)*weight(imodel)
     +             +(mfun_avg(1,imodel)-funcval(1,1))**2*weight(imodel) 
	funcval(2,2)= funcval(2,2)+mfun_var(2,imodel)*weight(imodel)
     +             +(mfun_avg(2,imodel)-funcval(1,2))**2*weight(imodel)
	enddo  

      return
      end
c
c
c#######################################################################
c
      subroutine objf(funcvalue,nparam,p,nper,perlen0,mxwell
     +	,nwells0,well)

c      subroutine objf(funcval,nparam,p,nper,perlen0,mxwell,nwells0,well
c     +,funcvalpw)
c
      implicit double precision (a-h,o-z)
      real perlen0,well,pwfunc
      dimension well(5,mxwell,nper),nwells0(nper)
      save
C
      include 'mgo.inc'
	real dd

      dimension p(nparam),perlen0(nper)
	dimension dd(100,100,1)
	common   / drawdown / dd
	COMMON   / funcval2/ pwfunc
c
c  Evaluate objective function
c  ===========================
c
c  fixed installation cost
c  Make sure that the number of wells do not repeat statistical
      c1=0.
      nw=0
      do np=1,nparam
        kp=pmodflow(4,np)
        ip=pmodflow(5,np)
        jp=pmodflow(6,np)
        pc1=pmodflow(18,np)  !release 2003
        qp=p(np)*pscale
        if(qp.eq.0) goto 11
        do nn=np-1,1,-1
          kk=pmodflow(4,nn)
          ii=pmodflow(5,nn)
          jj=pmodflow(6,nn)
          qq=p(nn)*pscale
          if(kk.eq.kp.and.ii.eq.ip.and.
     &     jj.eq.jp.and.qq.ne.0) goto 11
        enddo
        nw=nw+1
        c1 = c1 + cobj(1) * pc1  !release 2003
   11   continue
      enddo
c
c  drilling cost
c  According the depth of the wells
      c2=0.
      if(cobj(2).le.0) goto 2
      do np=1,nparam
        kp=pmodflow(4,np)
        ip=pmodflow(5,np)
        jp=pmodflow(6,np)
        depth=pmodflow(10,np)
        pc2=pmodflow(19,np)       !release 2003
        qp=p(np)*pscale
        if(qp*depth.eq.0) goto 22
        do nn=np-1,1,-1
          kk=pmodflow(4,nn)
          ii=pmodflow(5,nn)
          jj=pmodflow(6,nn)
          depth_old=pmodflow(10,nn)
          qq=p(nn)*pscale
          if(kk.eq.kp.and.ii.eq.ip.and.
     &     jj.eq.jp.and.qq*depth_old.ne.0) goto 22
        enddo
         c2 = c2 + cobj(2)*depth*pc2   !release 2003
   22   continue
      enddo
    2 continue
c
c  pumping and treatment costs
      c3=0.
      qsum=0.0
c different periods and different wells
      do ns=1,nper
        do nn=1,nwells0(ns)
          np=well(5,nn,ns)
          kp=pmodflow(4,np)
          ip=pmodflow(5,np)
          jp=pmodflow(6,np)
          if(np.le.0 .or. np.gt.nparam+ncnstr5) cycle
          pc3=pmodflow(20,np)                               !release 2003
          QW=well(4,nn,ns)    !!! *perlen0(ns)              !release 2003
          
c         c3 = c3 + cobj(3)* abs(QW) * perlen0(ns) * pc3   ! release 2003
          c3 = c3 + cobj(3)* abs(QW) * perlen0(ns) * pc3 * 
     +		 (abs(dd(jp,ip,kp)))     
          qsum = qsum + abs(QW)
        enddo
      enddo

      totalQ=qsum
!!      c3=cobj(3)*qsum   !release 2003
    3 continue
!
!  total mass removed by wells or total mass remaining in aquifer
      c4=0.
      if(cobj(4).le.0) goto 4
      if(MinMax.eq.1) then      !minimize total mass in aquifer
        qcsum=wmass(1)*0.000001
      elseif(MinMax.eq.2) then  !maximize total mass removal by wells
        qcsum=wmass(2)*0.000001
      endif
      c4=cobj(4)*abs(wmass(2))*0.000001

    4 continue
! Remaining percentage of the contaminant   
	pwfunc=abs(wmass(1))/(abs(wmass(1))+abs(wmass(2)))  !
!      pwfunc=abs(wmass(1))/abs(wmass(3))
! what other?
      usrobj=0
      if(cobj(5).ne.0) then
         c5 = usrobj  
      else
         c5=0.
      endif
c
c  total objective function value before penalty
      sum = c1 + c2 + c3 + c4 + c5
c
c  check type 0 constraints -- nactive wells out of nparam
      if(nactive.gt.0 .and. nw.gt.nactive) then
        pentsum=pentsum+pent0*float(nw-nactive)
      endif
c
c Define the range of the second objective 
C Give a penalty if out of range 
	if(pwfunc.lt.0.001)then
	pentsum=pentsum+(0.001-pwfunc)*pent0
	end if        !  add penalty function
	if(pwfunc.gt.0.2) then
	pentsum=pentsum+(pwfunc-0.2)*pent0
	end if
c
c  add penalty due to violation of flow and transport constraints
      sum=sum+pentsum
	pwfunc=pwfunc+pentsum
c
c--return fitness function
	funcvalue = sum/1000000
	pwfunc=pwfunc*100

c
 100  RETURN
      END
c
c
c#######################################################################
c
      Subroutine AssignP(nparam,nper,mxwell,nwells0,well)
c
      implicit double precision (a-h,o-z)
      real     well
      logical  found
      include 'mgo.inc'

      dimension well(5,mxwell,nper),nwells0(nper)
c
      do np=1,nparam

        kk=pmodflow(4,np)
        ii=pmodflow(5,np)
        jj=pmodflow(6,np)
        NStart=pmodflow(7,np)
        NStop=pmodflow(8,np)

        found=.false.
        do ns=nstart,nstop
          do nw=1,nwells0(ns)
            kw=well(1,nw,ns)
            iw=well(2,nw,ns)
            jw=well(3,nw,ns)
            if(kw.eq.kk.and.iw.eq.ii.and.jw.eq.jj) then
              well(5,nw,ns)=np
              found=.true.
              exit
            endif
          enddo
        enddo
        if(.not.found)  then
          write(*,100) np,kk,ii,jj
          stop
        endif

      enddo
  100 format(1x,'Parameter #',i4.4,' defined at (K,I,J):',3i4,
     &      /1x,'not found in MODFLOW WELL package input file.')

c
      do nc=1,NCnstr5

        kk=Cnstr5(1,nc)
        ii=Cnstr5(2,nc)
        jj=Cnstr5(3,nc)
        NStart=Cnstr5(7,nc)
        NStop=Cnstr5(8,nc)

        found=.false.
        do ns=nstart,nstop
          do nw=1,nwells0(ns)
            kw=well(1,nw,ns)
            iw=well(2,nw,ns)
            jw=well(3,nw,ns)
            if(kw.eq.kk.and.iw.eq.ii.and.jw.eq.jj) then
              well(5,nw,ns)= nparam + nc
              found=.true.
              exit
            endif
          enddo
        enddo
        if(.not.found)  then
          write(*,110) nc,kk,ii,jj
          stop
        endif

      enddo
  110 format(1x,'Group Constraint #',i4.4,' defined at (K,I,J):',3i4,
     &      /1x,'not found in MODFLOW WELL package input file.')


c
      do nc=1,NCnstr3

        kk=Cnstr3(1,nc)
        ii=Cnstr3(2,nc)
        jj=Cnstr3(3,nc)
        NStart=1
        NStop=nper

        found=.false.
        do ns=nstart,nstop
          do nw=1,nwells0(ns)
            kw=well(1,nw,ns)
            iw=well(2,nw,ns)
            jw=well(3,nw,ns)
            if(kw.eq.kk.and.iw.eq.ii.and.jw.eq.jj) then
              well(5,nw,ns)=        - nc
              found=.true.
              exit
            endif
          enddo
        enddo
        if(.not.found)  then
          write(*,120) nc,kk,ii,jj
          stop
        endif

      enddo
  120 format(1x,'Balance Constraint #',i4.4,' defined at (K,I,J):',3i4,
     &      /1x,'not found in MODFLOW WELL package input file.')


      return
      end
c
c
c#######################################################################
c
      Subroutine UpdateP(nparam,p,nper,mxwell,nwells0,well,
     & top,bot,hycond,ibound,laycon,ncol,nrow,nlay)
c
      implicit double precision (a-h,o-z)
      real     well,top,bot,hycond
      logical  outside
      include 'mgo.inc'

      dimension p(2*nparam),well(5,mxwell,nper),nwells0(nper),
     &          top(ncol,nrow,nlay),bot(ncol,nrow,nlay),
     &    hycond(ncol,nrow,nlay),bound(ncol,nrow,nlay),laycon(80)
c
c update primary parameters
c
      do ns=1,nper
        do nw=1,nwells0(ns)

          np=well(5,nw,ns)
          if(np.le.0 .or. np.gt.nparam) cycle

          QW=p(np)*pscale

          well(4,nw,ns)=QW

          KPStart=pmodflow(12,np)
          IPStart=pmodflow(13,np)
          JPStart=pmodflow(14,np)
          KPEnd=pmodflow(15,np)
          IPEnd=pmodflow(16,np)
          JPEnd=pmodflow(17,np)
          if(KPStart.eq.KPEnd.and.IPStart.eq.IPEnd
     &      .and.JPStart.eq.JPEnd) cycle

          npos=p(np+nparam)

          nnode=0
          do KK=KPStart,KPEnd
            do II=IPStart,IPEnd
              do JJ=JPStart,JPEnd
                nnode=nnode+1
                if(nnode.eq.npos) then
                  well(1,nw,ns)=kk
                  well(2,nw,ns)=ii
                  well(3,nw,ns)=jj
                  pmodflow(4,np)=kk
                  pmodflow(5,np)=ii
                  pmodflow(6,np)=jj
                  if(ibound(jj,ii,kk).le.0) well(4,nw,ns)=0.
                  goto 100
                endif
              enddo
            enddo
          enddo
  100     continue

        enddo
      enddo
c
c update well group constraints
c
      if(NCnstr5.le.0) goto 222

      do n=1,NCnstr5  !check modflow layer type if internal T dependence

         iDepend=Cnstr5(6,n)
         if(iDepend.ne.0) cycle

         do K=1,nlay
           if(laycon(K).ne.3) then
             STOP 'Modflow layer type (LAYCON) must be 3'
           endif
         enddo

      enddo


      do ns=1,nper
        do nw=1,nwells0(ns)

          ntmp=well(5,nw,ns)
          nc=ntmp-nparam
          if(nc.le.0 .or. nc.gt.NCnstr5) cycle

          kk=Cnstr5(1,nc)
          ii=Cnstr5(2,nc)
          jj=Cnstr5(3,nc)
          nphost=Cnstr5(4,nc)

          kshift=Cnstr5(9, nc)
          ishift=Cnstr5(10,nc)
          jshift=Cnstr5(11,nc)

          khost=pmodflow(4,nphost)
          ihost=pmodflow(5,nphost)
          jhost=pmodflow(6,nphost)

          kdep=khost+kshift
          idep=ihost+ishift
          jdep=jhost+jshift

          outside=.false.
          if(kdep.lt.1) then
            kdep=1
            outside=.true.
          elseif(kdep.gt.nlay) then
            kdep=nlay
            outside=.true.
          endif
          if(idep.lt.1) then
            idep=1
            outside=.true.
          elseif(idep.gt.nrow) then
            idep=nrow
            outside=.true.
          endif
          if(jdep.lt.1) then
            jdep=1
            outside=.true.
          elseif(jdep.gt.ncol) then
            jdep=ncol
            outside=.true.
          endif

          well(1,nw,ns)=kdep
          well(2,nw,ns)=idep
          well(3,nw,ns)=jdep
          Cnstr5(1,nc)=kdep
          Cnstr5(2,nc)=idep
          Cnstr5(3,nc)=jdep

          iDepend=Cnstr5(6,nc)
          if(outside.or.ibound(jdep,idep,kdep).le.0) then
            factor_depend=0.
          elseif(iDepend.ne.0) then
            factor_depend=Cnstr5(5,nc)
          elseif(iDepend.eq.0) then
            T_host=hycond(jhost,ihost,khost)*
     &             (top(jhost,ihost,khost)-bot(jhost,ihost,khost))
            T_dep=hycond(jdep,idep,kdep)*
     &             (top(jdep,idep,kdep)-bot(jdep,idep,kdep))
            factor_depend=T_dep/T_host
          endif

          QW=p(nphost)*factor_depend*pscale
          well(4,nw,ns)=QW

        enddo
      enddo

  222 continue

c
c update balance constraints
c
      do nc=1,NCnstr3

        iParam1=Cnstr3(4,nc)
        iParam2=Cnstr3(5,nc)
        Coeff1=Cnstr3(6,nc)
        Coeff2=Cnstr3(7,nc)

        do ns=1,nper  !for each stress period

          QSUM=0.               !get sum Q
          do ip=iParam1,iParam2
            NStart=pmodflow(7,ip)
            NStop=pmodflow(8,ip)
            if(ns.lt.nstart.or.ns.gt.nstop) cycle
            do nw=1,nwells0(ns)
              iptmp=well(5,nw,ns)
              ictmp=iptmp-nparam
              iphost=0
              if(ictmp.gt.0) iphost=cnstr5(4,ictmp)
              if( iptmp.eq.ip .or. iphost.eq.ip ) then
                QW=well(4,nw,ns)
                QSUM=QSUM+QW
              endif
            enddo
          enddo

          do nw=1,nwells0(ns)
            nb=well(5,nw,ns)
            if(nc.eq.-nb) then
              well(4,nw,ns)=QSUM*Coeff1+Coeff2
              exit
            endif
          enddo

        enddo !for each stress period

      enddo !for each balance constraint
c
c save well file if needed
c
      if(iwrite.ne.999) goto 1000

      write(ioutwel,'(i10)') mxwell

      do kper=1,nper

        itmp=nwells0(kper)
        write(ioutwel,500) itmp,kper
        if(itmp.le.0) cycle
        do n=1,itmp
          kw=Well(1,n,kper)
          iw=Well(2,n,kper)
          jw=Well(3,n,kper)
          qw=Well(4,n,kper)
          write(ioutwel,600) kw,iw,jw,qw
        enddo

      enddo

  500 format(i10,'  /Stress Period:',i3)
  600 format(3i10,1p,e10.3)
c
 1000 continue


      return
      end
c
c
c#######################################################################
c
      Subroutine OPT2RP(nparam,npopsiz,maxgen,NTOL,irestrt,iResponse,
     & iOptSolver)
c
      implicit double precision (a-h,o-z)
      save
      include 'mgo.inc'
c
    1 read(inga,'(a)') TextLine
      if(TextLine.eq.' ') goto 1
      if(TextLine(1:1).eq.'#') goto 1

      backspace (inga)
      read(inga,'(a)') (title(i),i=1,2)
      write(ioutga,1060)
      write(ioutga,1070) (title(i),i=1,2)
      write(ioutga,1060)
 1060 format(1x,' ------- ')
 1070 format(1x,'| M G O | ',A80/1X,'| r2003 | ',A)

      read(inga,*) iOptSolver,iResponse
!                                       !for release 2003
!     if(iOptSolver.ne.1) then
!       write(*,889)
! 889   format(/1x,'Only the GA solver is',
!    &   ' included in this release;',
!    &   /1x,'Please contact Chunmiao Zheng for more information.')
!pw         stop
!     endif
!
      if    (iOptSolver.eq.1) then
        write(ioutga,1071)
      elseif(iOptSolver.eq.2) then
        write(ioutga,1072)
      elseif(iOptSolver.eq.3) then
        write(ioutga,1073)
!     elseif(iOptSolver.eq.0) then    !release 2003
!       write(ioutga,1074)
      else
        write(ioutga,9074)
        write(*,9074)
        stop
      endif
      if(iResponse.eq.0) then
        write(ioutga,1075)
      else
        write(ioutga,1076)
      endif

 1071 format(/1x,'Optimization Solver Selected: Genetic Algorithms')
 1072 format( 1x,'Optimization Solver Selected: Simulated Annealing')
 1073 format( 1x,'Optimization Solver Selected: Tabu Search')
 1074 format( 1x,'Optimization Solver Selected: Linear Programming')
 9074 format(/1x,'Error: Invalid Code for the Optimization Solver')
 1075 format( 1x,'Response Database Not Saved')
 1076 format( 1x,'Response Database Saved in File [MGO.RSP]')

      read(inga,*) maxgen,npopsiz,NTOL,irestrt,ldbg,iwrite
      write(ioutga,1080) maxgen,npopsiz,NTOL
 1080 format( 1x,'Maximum Number of Iterations =',i5,
     &       /1x,'Number of Forward Simulations per Iteration =',i5,
     &       /1x,'Number of Iterations for Convergence =',i5)
      if(irestrt.gt.0) then
        write(ioutga,1085)
      else
        write(ioutga,1090)
      endif
      if(ldbg.eq.0) then
        write(ioutga,1040) ldbg
      elseif(ldbg.eq.1) then
        write(ioutga,1041) ldbg
      elseif(ldbg.ge.2) then
        write(ioutga,1042) ldbg
      endif
      if(iwrite.le.0) then
        write(ioutga,2040)
      else
        write(ioutga,2041)
      endif
 1040 format(1x,'Debugging Level =',i3,
     & ' (Intermediate Simulations Not Saved)')
 1041 format(1x,'Debugging Level =',i3,
     & ' (Intermediate Simulations Shown on Screen)')
 1042 format(1x,'Debugging Level =',i3,
     & ' (All Intermediate Simulations Saved -- SLOW!)')
 2040 format(1x,
     & 'Final Simulation with Optimized Parameters Not Required')
 2041 format(1x,'Final Simulation with Optimized Parameters Required')
 1085 format(1x,'Restart Option Activated; ',
     &          'Restart Data Read from File [MGO.RES]')
 1090 format(1x,'Restart Option Not Activated')

      read(inga,*) minmax,(cobj(i),i=1,5)
      if(minmax.eq.1) then
        write(ioutga,1091)
 1091   format(/1x,'Direction of Optimization -> Minimization')
      elseif(minmax.eq.2) then
        write(ioutga,1092)
 1092   format(1x,'Direction of Optimization -> Maximization')
      else
        write(ioutga,1093)
        stop
 1093   format(1x,'Direction of Optimization -> Undefined')
      endif
      write(ioutga,1094) (cobj(i),i=1,5)
 1094 format(/1x,'Coefficients for the Objective Function',
     &       /1x,'---------------------------------------',
     & /1x,'Fixed capital cost per well =',g11.4,
     & /1x,'Drilling/Installation cost per unit depth of well =',g11.4,
     & /1x,'Pumping/Treatment cost per unit volume of flow =',g11.4,
     & /1x,'Mass removal cost factor =',g11.4,
     & /1x,'External objective function multiplier =',g11.4)

!
      Transport=.False.
      if(cobj(4).ne.0) Transport=.True.

!
      if(cobj(5).ne.0) then
        write(*,888)
  888   format(/1x,'User-supplied objective function'
     &   ' not supported in this release;',
     &   /1x,'Please contact Chunmiao Zheng [czheng@ua.edu]',
     &    ' for more information.')
         stop
      endif
!

      read(inga,*) nparam,pscale,nactive,pent0
      write(ioutga,1100) nparam,pscale,nactive,pent0
 1100 format(/1x,'Number of Optimization Parameters =',i5,
     &       /1x,'Parameter Scaling Factor =',g15.7
     &       /1x,'Number of Parameters Active at Any Time =',i5,
     &       /1x,'Penalty if Active Parameters',
     &           ' More than Specified Maximum =',g15.7
     &       /1x,'ParamNo. Mimimum   Maximum  # of Discret.',
     &           ' K   I   J    SP1  SP2  Depth  iOptOnOff',
     &           ' Starting KIJ   Ending KIJ',
     &           '                   Weighting Factors',
     &        /1x,158('-'))
      if(nparam.gt.npmodmax) then
         write(*,1700) npmodmax,nparam
         write(ioutga,1700) npmodmax,nparam
         close(ioutga)
         stop
      endif
      do n=1,nparam
        KPStart=0
        IPStart=0
        JPStart=0
        KPEnd=0
        IPEnd=0
        JPEnd=0
        pc1=1.    !release 2003
        pc2=1.
        pc3=1.
        pc4=1.
        pc5=1.
        read(inga,*) iParam,Parmin,Parmax,NPOSIBL,
     &               KP,IP,JP,NStart,NStop,depth,Nonoff,
     &               KPStart,IPStart,JPStart,KPEnd,IPEnd,JPEnd,
     &               pc1,pc2,pc3,pc4,pc5     !release 2003
        if(nactive.ge.0) then
          KPStart=KP
          IPStart=IP
          JPStart=JP
          KPEnd=KP
          IPEnd=IP
          JPEnd=JP
        elseif(KPStart*KPEnd.le.0
     &    .or. IPStart*IPEnd.le.0
     &    .or. JPStart*JPEnd.le.0) then
          write(ioutga,1104) n
          KPStart=KP
          IPStart=IP
          JPStart=JP
          KPEnd=KP
          IPEnd=IP
          JPEnd=JP
        endif
 1104   format(1x,'Error: moving well region invalid or undefined ',
     &    'for parameter',i4/1x,7x,'Reset to fixed initial location.')
        write(ioutga,1105) iParam,Parmin,Parmax,NPOSIBL,
     &               KP,IP,JP,NStart,NStop,depth,Nonoff,
     &               KPStart,IPStart,JPStart,KPEnd,IPEnd,JPEnd,
     &               pc1,pc2,pc3,pc4,pc5   !release 2003
 1105   format(1x,i4,2x,1p,2g12.4,1x,i4,4x,3i4,1x,2i5,1x,g11.4,i3,
     &         1x,6i5,4x,5g10.3)  !release 2003

        pmodflow(1,iParam)=Parmin
        pmodflow(2,iParam)=Parmax
        pmodflow(3,iParam)=NPOSIBL
        pmodflow(4,iParam)=KP
        pmodflow(5,iParam)=IP
        pmodflow(6,iParam)=JP
        pmodflow(7,iParam)=NStart
        pmodflow(8,iParam)=NStop
!no #9        
        pmodflow(10,iParam)=depth
        pmodflow(11,iParam)=Nonoff
        pmodflow(12,iParam)=KPStart
        pmodflow(13,iParam)=IPStart
        pmodflow(14,iParam)=JPStart
        pmodflow(15,iParam)=KPEnd
        pmodflow(16,iParam)=IPEnd
        pmodflow(17,iParam)=JPEnd

        pmodflow(18,iParam)=pc1     !release 2003
        pmodflow(19,iParam)=pc2
        pmodflow(20,iParam)=pc3
        pmodflow(21,iParam)=pc4
        pmodflow(22,iParam)=pc5

      enddo
c
      read(inga,*) NCnstr6

      write(ioutga,3100) NCnstr6
 3100 format(/1x,'Number of global MinMax constraints =',i5)
      if(NCnstr6.gt.0) then
        write(ioutga,3101)
 3101   format(1x,'Constraint #   Minimum    Maximum    Coefficients-->',
     &        /1x,57('-'))
      endif

      if (NCnstr6.gt.ncnstrmax) then
         write(*,1705) ncnstrmax,NCnstr6
         write(ioutga,1705) ncnstrmax,NCnstr6
         close(ioutga)
         stop
      endif

      do n=1,ncnstr6

        read(inga,*) gmin,gmax,(cnstr6(i,n),i=1,nparam)
        write(ioutga,3105) n,gmin,gmax,(cnstr6(i,n),i=1,nparam)
 3105   format(1x,5x,i3,5x,2g10.4,1x,8g10.4/(27x,8g10.4))

        Cnstr6(npmodmax+1,n)=gmin
        Cnstr6(npmodmax+2,n)=gmax

      enddo


      read(inga,*) NCnstr5
      write(ioutga,2100) NCnstr5
 2100 format(/1x,'Number of Dependent Parameters =',i5)
      if(NCnstr5.gt.0) then
        write(ioutga,2101)
 2101   format(1x,'   K   I   J    HostParamNo   Factor_Depend',
     &   '  Option_Depend'/1x,57('-'))
      endif

      if (NCnstr5.gt.ncnstrmax) then
         write(*,1706) ncnstrmax,NCnstr5
         write(ioutga,1706) ncnstrmax,NCnstr5
         close(ioutga)
         stop
      endif

      do n=1,ncnstr5
        read(inga,*) KP,IP,JP,iParam_depend,Factor_depend,iDepend
        write(ioutga,2105) KP,IP,JP,
     &               iParam_depend,Factor_depend,iDepend
 2105   format(1x,3i4,3x,i5,12x,g11.4,5x,i3)

        Cnstr5(1,n)=KP
        Cnstr5(2,n)=IP
        Cnstr5(3,n)=JP
        Cnstr5(4,n)=iParam_Depend
        Cnstr5(5,n)=Factor_Depend
        Cnstr5(6,n)=iDepend

        KHost=pmodflow(4,iParam_depend)
        IHost=pmodflow(5,iParam_depend)
        JHost=pmodflow(6,iParam_depend)
        NStart=pmodflow(7,iParam_depend)
        NStop=pmodflow(8,iParam_depend)

        Cnstr5(7,n)=NStart
        Cnstr5(8,n)=NStop
        Cnstr5(9,n)=KP-KHost
        Cnstr5(10,n)=IP-IHost
        Cnstr5(11,n)=JP-JHost

      enddo

c
      read(inga,*) NCnstr1,Pent1
      if(pent1.lt.0) stop 'Penalty coefficient 1 < 0'
      write(ioutga,1110) NCnstr1,Pent1
 1110 format(/1x,'Number of Constraints (Head Diff.) = ',i5,
     &       /1x,'Penalty for Violating Constraints = ',g15.7,
     &       /1x, 'Starting SP Ending SP  Outside (K,I,J)',4x,
     &            'Inside (K,I,J)',6x,'Head Diff.'/1x,72('-'))
      if (NCnstr1.gt.ncnstrmax) then
         write(*,1707) ncnstrmax,NCnstr1
         write(ioutga,1707) ncnstrmax,NCnstr1
         close(ioutga)
         stop
      endif
      do n=1,NCnstr1
        read(inga,*) nstart,nstop,kout,iout,jout,kin,iin,jin,hdiff
        write(ioutga,1115) nstart,nstop,kout,iout,jout,kin,iin,jin,hdiff
 1115   format(1x,2(i7,2x),3x,6(i4,2x),3x,1p,e11.4)
        cnstr1(1,n)=kout
        cnstr1(2,n)=iout
        cnstr1(3,n)=jout
        cnstr1(4,n)=kin
        cnstr1(5,n)=iin
        cnstr1(6,n)=jin
        cnstr1(7,n)=hdiff
        cnstr1(8,n)=nstart
        cnstr1(9,n)=nstop
      enddo

      read(inga,*) NCnstr2,Pent2
      if(pent2.lt.0) stop 'Penalty coefficient 2 < 0'
      write(ioutga,1120) NCnstr2,Pent2
 1120 format(/1x,'Number of Constraints (Head Bounds) = ',i5,
     &       /1x,'Penalty for Violating Constraints = ',g15.7,
     &       /1x,'Starting SP Ending SP  Starting (K,I,J)',3x,
     &           'Ending (K,I,J)',5x,
     &           'Lower Bound    Upper Bound '/1x,87('-'))
      if (NCnstr2.gt.ncnstrmax) then
         write(*,1710) ncnstrmax,NCnstr2
         write(ioutga,1710) ncnstrmax,NCnstr2
         close(ioutga)
         stop
      endif
      do n=1,NCnstr2
        read(inga,*) nstart,nstop,k1,i1,j1,k2,i2,j2,hlower,hupper
        write(ioutga,1125) nstart,nstop,k1,i1,j1,k2,i2,j2,hlower,hupper
 1125   format(1x,2(i7,2x),3x,6(i4,2x),3x,1p,e11.4,4x,e11.4)
        cnstr2(1,n)=k1
        cnstr2(2,n)=i1
        cnstr2(3,n)=j1
        cnstr2(4,n)=k2
        cnstr2(5,n)=i2
        cnstr2(6,n)=j2
        cnstr2(7,n)=hlower
        cnstr2(8,n)=hupper
        cnstr2(9,n)=nstart
        cnstr2(10,n)=nstop
      enddo

      read(inga,*) NCnstr3
      Pent3=0.
      write(ioutga,1130) NCnstr3,Pent3
 1130 format(/1x,'Number of Constraints (Balance) = ',i5,
     &       /1x,'Penalty for Violating Constraints = ',g15.7,
     &       /1x,'Location (K,I,J)  Depend on Param No.',
     &           '     Coeff. 1     Coeff. 2'/1x,63('-'))
      if (NCnstr3.gt.ncnstrmax) then
         write(*,1715) ncnstrmax,NCnstr3
         write(ioutga,1715) ncnstrmax,NCnstr3
         close(ioutga)
         stop
      endif
      do n=1,NCnstr3
        read(inga,*) Kw, Iw, Jw, iParam1, iParam2, Coeff1, Coeff2
        write(ioutga,1135) Kw, Iw, Jw, iParam1, iParam2, Coeff1, Coeff2
 1135   format(3(i4,2x),1x,'from ',i3,' to ',i3,5x,1P,e11.3,3x,e11.3)
        cnstr3(1,n)=Kw
        cnstr3(2,n)=Iw
        cnstr3(3,n)=Jw
        cnstr3(4,n)=iParam1
          cnstr3(5,n)=iParam2
        cnstr3(6,n)=Coeff1
        cnstr3(7,n)=Coeff2
      enddo

      read(inga,*) NCnstr4,Pent4
      if(pent4.lt.0) stop 'Penalty coefficient 4 < 0'
      write(ioutga,1140) NCnstr4,Pent4
 1140 format(/1x,'Number of Constraints (Conc Bounds) = ',i5,
     &       /1x,'Penalty for Violating Constraints = ',g15.7,
     &       /1x,'Species  Starting SP Ending SP  Starting (K,I,J)',
     &        3x,'Ending (K,I,J)',5x,
     &           'Lower Bound    Upper Bound '/1x,106('-'))
      if (NCnstr4.gt.ncnstrmax) then
         write(*,1720) ncnstrmax,NCnstr4
         write(ioutga,1720) ncnstrmax,NCnstr4
         close(ioutga)
         stop
      endif

      if(NCnstr4.gt.0) Transport=.true.

      do n=1,NCnstr4
        read(inga,*) iComp,nStart,nStop,k1,i1,j1,k2,i2,j2,cLower,cUpper
        write(ioutga,2125)
     &               iComp,nStart,nStop,k1,i1,j1,k2,i2,j2,cLower,cUpper
        cnstr4(1,n)=k1
        cnstr4(2,n)=i1
        cnstr4(3,n)=j1
        cnstr4(4,n)=k2
        cnstr4(5,n)=i2
        cnstr4(6,n)=j2
        cnstr4(7,n)=cLower
        cnstr4(8,n)=cUpper
        cnstr4(9,n)=nStart
        cnstr4(10,n)=nStop
        cnstr4(11,n)=iComp
      enddo
 2125 format(1x,i5,4x,2(i7,2x),3x,6(i4,2x),3x,1p,e11.4,4x,e11.4)
c Nga parameters
      read(inga,*) Uncertainty
	if (Uncertainty) then
	  read(inga,*) Nnoisy0,Ngen,Maxinoisy
	  write(ioutga,1119) Nnoisy0,Ngen,Maxinoisy
	  read(inga,*) pcon
	  write(ioutga,*) ' The percentage confidence level = ',pcon
	else
	  write(ioutga,1118)
	endif
c
        if(MinMax.eq.2) then
          Pent0=-Pent0
          Pent1=-Pent1
          Pent2=-Pent2
          Pent3=-Pent3
          Pent4=-Pent4
        endif
c
 1700 format(/1x,'Error: maximum # of opt. parameters',
     & ' (decision variables) allowed:',i5,
     &       /1x,'       # of opt. parameters',
     & ' (decision variables) for current run:',i5)
 1705 format(/1x,'Error: maximum # of global minmax constraints',
     &          ' allowed:',i5,
     &       /1x,'       # of global minmax constraints',
     &          ' for current run:',i5)
 1706 format(/1x,'Error: maximum # of dependent variables allowed:',i5,
     &       /1x,'       # of dependent variables for current run:',i5)
 1707 format(/1x,'Error: maximum # of head diff constraints',
     &          ' allowed:',i5,
     &       /1x,'       # of head diff constraints',
     &          ' for current run:',i5)
 1710 format(/1x,'Error: maximum # of head bound constraints',
     &         ' allowed:',i5,
     &       /1x,'       # of head bound constraints',
     &           ' for current run:',i5)
 1715 format(/1x,'Error: maximum # of balance constraints allowed:',i5,
     &       /1x,'       # of balance constraints for current run:',i5)
 1720 format(/1x,'Error: maximum # of conc bound constraints',
     &          ' allowed:',i5,
     &       /1x,'       # of conc bound constraints',
     &          ' for current run:',i5)
 1119 format(/1x,'Conductivity/Transmissivity(K/T) exists Uncertainty,',
     #       /1x,' and Using a noisy GA to search the optimal design,'
     #       /1x,' number of initial sample sets for noisy GA   =',i3,
     #       /1x,' increment of generations that Nnoisy changed =',i3,
     #       /1x,' maximum number of the noisy sample sets      =',i3)
 1118 format(/1x,'The Conductivity/Transmissivity is Deterministic.')
c
      return
      end
c
c#######################################################################
c
      Subroutine checkflow(kkstp,kkper,ncol,nrow,nlay,hnew,ibound)
c
      implicit double precision (a-h,o-z)
      include 'mgo.inc'

      dimension hnew(ncol,nrow,nlay),ibound(ncol,nrow,nlay)
c
c  check type 1 constraints -- head diff between obs well pair
c  and add penalty to obj func if constraint not met
      if(NCnstr1.gt.0.and.Ldbg.eq.2) write(ioutcsn,5)

      do m=1,NCnstr1
        nStart=Cnstr1(8,m)
        nStop=Cnstr1(9,m)

        if(kkper.ge.nStart.and.kkper.le.nStop) then
          kout=Cnstr1(1,m)
          iout=Cnstr1(2,m)
          jout=Cnstr1(3,m)
          kin =Cnstr1(4,m)
          iin =Cnstr1(5,m)
          jin =Cnstr1(6,m)
          hdiff=Cnstr1(7,m)
          hdiff_cal=-999.
          if(ibound(jout,iout,kout)*ibound(jin,iin,kin).ne.0) then
            hdiff_cal=hnew(jout,iout,kout)-hnew(jin,iin,kin)
            if(hdiff_cal.lt.hdiff) then
              pentsum=pentsum + Pent1*(hdiff-hdiff_cal)
            endif
          endif
          if(Ldbg.eq.2) write(ioutcsn,10)
     &     kkper,kout,iout,jout,kin,iin,jin,hdiff,hdiff_cal
        endif

      enddo
    5 format(1x,'Stress Period  Outside(K,I,J)  Inside(K,I,J)',
     & ' Requested Hdiff  Calculated Hdiff')
   10 format(1x,2x,i3,11x,6(i3,2x),2(g13.6,1x))
c
c  check type 2 constraints -- lower and upper head bounds
c  and add penalty to obj func if constraint not met
      if(NCnstr2.gt.0.and.Ldbg.eq.2) write(ioutcsn,15)

      do m=1,NCnstr2
        nStart=Cnstr2(9,m)
        nStop=Cnstr2(10,m)

        if(kkper.ge.nStart.and.kkper.le.nStop) then
          k1=Cnstr2(1,m)
          i1=Cnstr2(2,m)
          j1=Cnstr2(3,m)
          k2 =Cnstr2(4,m)
          i2 =Cnstr2(5,m)
          j2 =Cnstr2(6,m)
          hlower=Cnstr2(7,m)
          hupper=Cnstr2(8,m)
          do kk=k1,k2
            do ii=i1,i2
              do jj=j1,j2
                if(ibound(jj,ii,kk).eq.0) goto 100
                if(hnew(jj,ii,kk).lt.hlower) then
                  pentsum=pentsum + Pent2*(hlower-hnew(jj,ii,kk))
                endif
                if(hnew(jj,ii,kk).gt.hupper) then
                  pentsum=pentsum + Pent2*(hnew(jj,ii,kk)-hupper)
                endif
 100            if(Ldbg.eq.2) write(ioutcsn,20)
     &           kkper,kk,ii,jj,hlower,hupper,hnew(jj,ii,kk)
              enddo
            enddo
          enddo
        endif

      enddo
   15 format(1x,'Stress Period  Location(K,I,J)  Lower Bound',
     & '   Upper Bound    Head Value')
   20 format(1x,2x,i3,11x,3(i3,2x),3(g13.6,1x))
c
      return
      end
c
c
c#######################################################################
c
      Subroutine checktran(kkper,ncol,nrow,nlay,ncomp,cnew,icbund)
c
c add ncomp by cz Sat  03-31-01
c
      implicit double precision (a-h,o-z)
      include 'mgo.inc'

      real cnew(ncol,nrow,nlay,ncomp)
      integer icbund(ncol,nrow,nlay,ncomp)

	INTEGER STEP
	REAL TIME,CVALUE(4)
c
c  check type 4 constraints -- concentration limit
c  and add penalty to obj func if constraint not met
      if(NCnstr4.gt.0.and.Ldbg.eq.2) write(ioutcsn,10)

      do m=1,NCnstr4
        nStart=Cnstr4(9,m)
        nStop=Cnstr4(10,m)
        iComp=Cnstr4(11,m)

        if(kkper.ge.nStart.and.kkper.le.nStop) then
          k1=Cnstr4(1,m)
          i1=Cnstr4(2,m)
          j1=Cnstr4(3,m)
          k2 =Cnstr4(4,m)
          i2 =Cnstr4(5,m)
          j2 =Cnstr4(6,m)
          clower=Cnstr4(7,m)
          cupper=Cnstr4(8,m)
          do kk=k1,k2
            do ii=i1,i2
              do jj=j1,j2
                if(icbund(jj,ii,kk,iComp).eq.0) goto 100
                if(cnew(jj,ii,kk,iComp).lt.clower) then
                  pentsum=pentsum + Pent4*(clower-cnew(jj,ii,kk,iComp))
                endif
                if(cnew(jj,ii,kk,iComp).gt.cupper) then
                  pentsum=pentsum + Pent4*(cnew(jj,ii,kk,iComp)-cupper)
                endif
  100           if(Ldbg.eq.2) write(ioutcsn,20)
     &           iComp,kkper,kk,ii,jj,clower,cupper,cnew(jj,ii,kk,iComp)
              enddo
            enddo
          enddo
        endif

      enddo
      

      OPEN(202,FILE='MT3D001.OBS')
	READ(202,*)
	READ(202,*)
	DO WHILE(.NOT.EOF(202))
	READ(202,*) STEP,TIME,CVALUE(1:1)

	DO I=1,1
	IF(CVALUE(I).GT.2) THEN
       pentsum=pentsum + Pent4*(CVALUE(I)-2)
	ENDIF
	ENDDO
	ENDDO
      CLOSE(202)


   10 format(1x,'Species  Stress Period  Location(K,I,J)  Lower Bound',
     & '   Upper Bound    Concentration')
   20 format(1x,2x,i3,8x,i3,9x,3(i3,2x),3(g13.6,1x))
c
      return
      end
c
c#######################################################################
c
      Subroutine readdd(kper,ncol,nrow,nlay,hnew,dd)
c
      implicit double precision (a-h,o-z)
      include 'mgo.inc'
	real dd(100,100,1)

      dimension hnew(ncol,nrow,nlay),
     &          htop(ncol,nrow)
c
      open(8957,file='htop.txt')
	do ii=1,nrow
	  read(8957,FMT="(3x,15f8.3)") (htop(jj,ii),jj=1,23)
	enddo
      rewind(8957)
          do kk=1,nlay
            do ii=1,nrow
              do jj=1,ncol
                dd(jj,ii,kk)=htop(jj,ii)-hnew(jj,ii,kk)
	if(dd(jj,ii,kk)<0) dd(jj,ii,kk)=0
              enddo
            enddo
          enddo
      open(8956,file='dd.txt')
	do ii=1,17
	  write(8956,FMT="(61f6.2)") (dd(jj,ii,1),jj=1,23)
	enddo
	close(8956)
      return
      end
