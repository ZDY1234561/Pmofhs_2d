C     Last change:  PW    7 May 103    1:47 pm
c
c#######################################################################
c
      subroutine funcTS(P,funcval,nparam,lenx,x)
c
      implicit double precision (a-h,o-z)
      save
c
      include 'ga.inc'
      include 'modflow.inc'
      include 'mgo.inc'
      dimension x(lenx)
c
!!PW      dimension parent(indmax,nparmax), p(nparmax)
      dimension parent(indmax,nparmax), p(2*nparmax) !!CHANGED 2/22/02
      dimension iparent(indmax,nchrmax)
c
      common / ga3   / parent,iparent
c
c--copy parameter information from 'parent' to 'p'
!!pw      ionoff=1
      IF(nactive.gt.0)then
         do np=1,nparam
            p(np)=p(np)*p(np+nparam)
         enddo
      endif
!!pw          ionoff=parent(j,np+nparam)
!!pw        endif
!!pw        parent(j,np)=parent(j,np)*float(ionoff)
!!pw        if(nactive.eq.-1) p(np+nparam)=parent(j,np+nparam)
!!pw  enddo

!! PW 1/3/2001
!cz   wmass(1)=-1.e6  !  for cmax or cmax-c*     !Sat  05-10-03


c--check global minmax constraints
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
!!pw            parent(j,np)=p(np)
            endif
          enddo

        elseif(gsum.lt.gmin.and.gsum.ne.SumUpper) then

          rescale=(gmin-gsum)/(SumUpper-gsum)
          do np=1,nparam
            pcoeff=cnstr6(np,nc)
            if(abs(pcoeff).gt.0) then
              p(np)=p(np)+rescale*(pmodflow(2,np)-p(np))
!!pw              parent(j,np)=p(np)
            endif
          enddo

        endif

!af     print *, 'rescale=',rescale
        
        do np=1,nparam
!af       p(np)=p(np)*rescale
!af       parent(j,np)=parent(j,np)*rescale
        enddo
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
      call modsub2(1,ncnvg,kkiter,lenx,x)
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
        call mt3dsub(1,ncnvg,1,Ldbg,iunit(22))
      elseif(Transport.and.iunit(22).eq.0) then
        write(*,*) 'Error: Unformatted flow-transport link not saved'
        write(*,*) '       by MODFLOW LKMT Package.'
        stop
      endif
      if(ncnvg.ne.0) write(ioutga,202) j,igen
  202 format(1x,'MT3D fails to converge in',i3,
     & ' transport steps for individual #',i4.4,
     & ' in Generation ',i3)

c
c--evaluate obj function
      call objf(funcval,nparam,p,nper,perlen0,mxwell,nwells0,x(lcwell0))
c
c--reset flow model
      call Reset(ncol,nrow,nlay,totim,vbvl,
     &           x(lcibou),x(lcib0),x(lchnew),x(lch0),x(lccr),
     &               x(lccr0),x(lccc),x(lccc0),x(lccv),x(lccv0))
c
c--return
      return
      end
