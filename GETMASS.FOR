c
c#######################################################################
c  Assign total mass (dissolved+sorbed) in aquifer to wmass(1)
c         total mass extracted by wells to wmass(2)
c  Include all mobile components
c#######################################################################
c
      SUBROUTINE GetMass(kkper,kkstp,ntrans,NCOL,NROW,NLAY,NCOMP,mCOMP,
     & ICBUND,DELR,DELC,DH,MXSS,NTSS,SS,SSMC,CNEW,DTRANS,time2,
     & rmasio,tmasio,tmass,TMASIN)
c
      implicit double precision (a-h,o-z)
      include 'mgo.inc'

      INTEGER   NCOL,NROW,NLAY,NCOMP,mCOMP,icomp,ICBUND,MXSS,
     &          NTSS,NUM,IQ,KK,II,JJ,kw,iw,jw,kper,kstp,ntrans,ierr,
     &          np,nc,ncnstr5
      REAL      DTRANS,SS,SSMC,CNEW,qss,CTMP,DELR,DELC,DH,time2,
     &          rmasio,tmasio,tmass,TMASIN
      DIMENSION ICBUND(NCOL,NROW,NLAY,NCOMP),SS(6,MXSS),
     &          SSMC(NCOMP,MXSS),CNEW(NCOL,NROW,NLAY,NCOMP),
     &          DELR(NCOL),DELC(NROW),DH(NCOL,NROW,NLAY),
     &          rmasio(122,2,ncomp),tmasio(122,2,ncomp),
     &          tmass(4,3,ncomp),TMASIN(ncomp)


! assign total mass extracted by wells to wmass(2)
! all mobile components
      wmass(2)=0.
      do icomp=1,mcomp
        wmass(2) = wmass(2) + tmasio(2,2,icomp)
      enddo
! assign total mass remaining (dissolved+sorbed) to wmass(1)
      wmass(1)=0.
      do icomp=1,mcomp
        wmass(1) = wmass(1) + tmass(1,2,icomp) +
     &   tmass(2,2,icomp) + tmass(3,2,icomp) + tmass(4,2,icomp)
      enddo
	wmass(3)=0
	do icomp=1,mcomp
	wmass(3)=wmass(3)+TMASIN(icomp)
	enddo

! done

      return
      end
