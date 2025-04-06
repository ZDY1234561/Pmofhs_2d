! mgo.inc
      parameter    (inga=111,ioutga=112,inres=113,
     &              ioutwel=114,ioutobf=115,ioutcsn=116,ioutmas=117,
     &              ioutrsp=118,ncnstrmax=2000,npmodmax=1000)
      character     Title(2)*200,TextLine*200
      logical       Transport,Uncertainty,logtrans
      dimension     pmodflow(30,npmodmax),
     &              Cnstr1(9,ncnstrmax),
     &              Cnstr2(10,ncnstrmax),
     &              Cnstr3(7,ncnstrmax),
     &              Cnstr4(11,ncnstrmax),
     &              Cnstr5(11,ncnstrmax),
     &              Cnstr6(npmodmax+2,ncnstrmax),
     &              Cnstr7(1,ncnstrmax),
     &              Cnstr8(1,ncnstrmax),
     &              Cnstr9(1,ncnstrmax),
     &              Cnstr10(1,ncnstrmax)
      common / opt /pmodflow,cobj(10),PScale,PentSum,
     &                       Cnstr1, Pent1, Cnstr2, Pent2,
     &                       Cnstr3, Pent3, Cnstr4, Pent4,
     &                       Cnstr5, Pent5, Cnstr6, Pent6,
     &                       Cnstr7, Pent7, Cnstr8, Pent8,
     &                       Cnstr9, Pent9, Cnstr10,Pent10,
     &   Pent0,NActive,NCnstr1, NCnstr2, NCnstr3, NCnstr4,
     &   NCnstr5, NCnstr6, NCnstr7, NCnstr8, NCnstr9, NCnstr10,
     &   MinMax,  Ldbg, iwrite, title, Transport
	  common / opt2     / WMASS(3), totalQ
	  common / noisy    / NNoisy0,Ngen,MaxiNoisy
	  common / noisy1   / pcon
      common / opt2     / Uncertainty,logtrans
! mgo.inc
