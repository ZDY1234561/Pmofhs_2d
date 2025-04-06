c
      Program Modular_Groundwater_Optimizer
c
c Define version number of this code 
      implicit double precision (a-h,o-z)
      integer iversion,iyear          
      character*8 date
      parameter (iversion=1,iyear=2003)
      save
c 
      include 'ga.inc'
      include 'modflow.inc'
      include 'mgo.inc'
c Algorithm parameters
! HS parameters
      dimension parent(indmax,nparmax),iparent(indmax,nchrmax)
      dimension parmax(nparmax),parmin(nparmax),pardel(nparmax)
	dimension child(indmax,nparmax),ichild(indmax,nchrmax)
	dimension nposibl(nparmax),nichflg(nparmax)
      dimension g0(nparmax),g1(nparmax),ig2(nparmax)
      real*8    HMCR,PAR,PAR1,PAR2
	integer   istart,POS,number
! Optimization parameters
	dimension fitness_avg(indmax,nobjmax),fitness_var(indmax,nobjmax) 
	dimension fitness_noisy(indmax)
      dimension funcval(2,nobjmax)
      integer   nobj,npop      
	integer   nparam,npopsiz,maxgen,NTOL,irestrt,iresponse,ioptsolver
	integer   igen
! pareto sorting parameters
      dimension isequen(indmax),pwnich(indmax)  
	dimension parentemp(indmax,nparmax),iparentemp(indmax,nchrmax)
	dimension ic(indmax)
	real*8    cigema,sumdij
	integer   jbest,issum,jrand	          
! pareto solutions filter parameters
	dimension filter_avg(indmax,nobjmax),filter_var(indmax,nobjmax)
	dimension filter_noisy(indmax)
	dimension parent_flt(indmax,nparmax),iparent_flt(indmax,nparmax)
      dimension iarray(indmax)
	dimension pwnich_flt(indmax),isequen_flt(indmax)
      integer   Nptomax,Npareto
	integer irandom,imodel
c
	real*4  cpu,cpu0,cpu1,tarray(2)
c Modflow parameters
      dimension x(:),ipLoc(:)
      allocatable :: x,ipLoc
c
      common / ga1   / npopsiz,nowrite
      common / ga2   / nparam,nchrome
      common / ga3   / parent
	common / ga4   / iparent           
      common / ga5   / g0,g1,ig2
      common / ga6   / parmax,parmin,pardel
	common / ga7   / nposibl
      common / ga8   / nichflg
	common / ga9   / isequen
	common / ga10  / pwnich          

      namelist / HS / HMCR,PAR1,PAR2                 
      namelist / modflow / thkmin
	common /rand  / irandom,imodel

c
c Set up HMS parameters  !Can be changed for parallel computation
      HMCR=0.9
	PAR1=0.1
	PAR2=0.99
	ielite=1
	idum=-1000     
	nchrome=nparam
	Nptomax=100
	Npareto=0
	nobj=2


c
c    Calculate time 	 
	call etime(tarray)
	write(*,*) cpu0
      write(6,*) tarray(1),tarray(2)
      cpu0=tarray(1)
      TIME0=SECNDS(0.0)
c
c--print a program identifier
      write(*,9011) iyear,iversion
 9011 format('+','MGO - Modular Groundwater Optimizer, ',
     & 'Version ',i4,' Release',i3)
      write(*,9012)
 9012 format(1x,'(C) 2000-03 ',
     & 'University of Alabama & Groundwater Systems Research Ltd.'/)
c
c    Open some output files
	open(103,file='time.txt')
 	open(8876,file='allindividuals.txt')  !all population
	open(8877,file='paretofitness.txt')   !isequen=0,objective
	open(8878,file='paretoparam.txt')     !isequen=0,parameter

c
c Thkmin option added to Modflow
      thkmin=1.
      read (inga,modflow,err=1,end=1) 	                                
    1 if(thkmin.lt.0) thkmin=1.
      close(inga)    
c
c  Open [mgo] main input & output files
      FINDEX='[MGO] Standard Output File: '
	open(ioutga,file='ex2a.mgo')
      FINDEX='[MGO] Main Input File: '
	open(inga,file='ex2a.opt',status='old')
c
c  Read and prepare [MGO] main input file
      Call OPT2RP(nparam,npopsiz,maxgen,NTOL,irestrt,iResponse,
     & iOptSolver)
c
c  Stop if maximum population size was not defined large enough
      if (npopsiz.gt.indmax) then
         write(*,1600) indmax,npopsiz
         write(ioutga,1600) indmax,npopsiz
         close(ioutga)
         stop
      endif
c
c First mparam is for pumping volume ;second mparam is for pumping position / pumping or not
      mparam=nparam
      if(nactive.ne.0) then
        nparam=2*nparam
      endif
      if (nparam.gt.nparmax) then
         write(*,1700) nparmax,nparam
         write(ioutga,1700) nparmax,nparam
         close(ioutga)
         stop
      endif
c
c Define the maximum and minimum of the parameters 
      do n=1,mparam
        parmin(n) = pmodflow(1,n)
        parmax(n) = pmodflow(2,n)
        nposibl(n)= 2**pmodflow(3,n)
      enddo
c
c Nactive is the number of active wells at any time; 
c Positive define fixed wells; ionoff=0 mean on;ionoff=1 mean maybe on,maybe off 
c Negtive defines moving wells; mean the zone of wells could be choosen
      if(nactive.gt.0) then !fixed wells with on/off
        do n=1,mparam
          ionoff=pmodflow(11,n)
          if(ionoff.eq.0) then
            parmin(n+mparam)=1.
            parmax(n+mparam)=1.
            nposibl(n+mparam)=2
          else
            parmin(n+mparam)=0.
            parmax(n+mparam)=1.
            nposibl(n+mparam)=2
          endif
        enddo
      elseif(nactive.eq.-1) then !moving wells
        do n=1,mparam
          KPStart=pmodflow(12,n)
          IPStart=pmodflow(13,n)
          JPStart=pmodflow(14,n)
          KPEnd=pmodflow(15,n)
          IPEnd=pmodflow(16,n)
          JPEnd=pmodflow(17,n)
          npos=(KPEnd-KPStart+1)*(IPEnd-IPStart+1)*(JPEnd-JPStart+1)
          parmin(n+mparam)=1.
          parmax(n+mparam)=npos
          nposibl(n+mparam)=npos
        enddo
      endif
c
c  Open default file for storing modflow well file
      open(ioutwel,file='MGO.WEL',status='unknown')
      rewind(ioutwel)
c
c  Open default file for storing objective function information
      if(irestrt.eq.0) then
        open(ioutobf,file='MGO.OBF',status='unknown')
        rewind(ioutobf)
      else
        open(ioutobf,file='MGO.OBF',status='old',
     &   position='append')
      endif
c
c  Open default file for storing constraint information
      open(ioutcsn,file='MGO.CSN',status='unknown')
      rewind(ioutcsn)
c
c  Open default file for storing cummulative mass information 
      if(iResponse.ne.0) then
        inquire(file='MGO.RSP',exist=fileexisted)
        if(fileexisted) then
          open(ioutrsp,file='MGO.RSP')
          read(ioutrsp,*) nptemp,mptemp
          close(ioutrsp)
          if(nptemp.eq.nparam.and.mptemp.eq.mparam) then
            open(ioutrsp,file='MGO.RSP',position='append',
     &       status='old')
          else
            open(ioutrsp,file='MGO.RSP',position='append',
     &       status='replace')
             write(ioutrsp,*) nparam,mparam
          endif
        else
          open(ioutrsp,file='MGO.RSP',position='rewind',
     &     status='new')
          write(ioutrsp,*) nparam,mparam
        endif
      endif
c
c  Initialize modflow simulation model
      LenX=1000
      imodflow=0
	irandom=1
	imodel=1
  123 ALLOCATE (x(LenX),STAT=IERR)
      IF(IERR.NE.0) THEN
        WRITE(*,*) 'STOP.  NOT ENOUGH MEMORY for MODFLOW'
        STOP
      ENDIF
      Call ModSUB1(1,mparam,lenx,isum,x,imodflow)  

      if(isum-1.gt.lenx) then
        lenx=isum-1
        deallocate (x,stat=ierr)
        if(ierr.ne.0) Stop 'Error deallocating memory.'
        imodflow=1
        goto 123
      endif

c
c  Initialize mt3dms simulation model if needed
      if(Transport) Call MT3DSUB(1,0,0,0,0)


c
c    Set up the g0,pardel arrays for the parameters
        nchrome=nparam
      write(*,115) nchrome
      write(ioutga,115) nchrome
 115  format(/'###############  Initial Population  ##############',
     & //1x,'Number of chromosomes ',i4)
c
      if (nchrome.gt.nchrmax) then
         write(*,1800) nchrmax,nchrome
         write(ioutga,1800) nchrmax,nchrome
         close(ioutga)
         stop
      endif
c     generate the initial HM         
         do 3 i=1,nparam
         g0(i)=parmin(i)
         pardel(i)=(parmax(i)-parmin(i))/(nposibl(i)-1)	
  3      continue   
c
c  Initialize random number generator      
      call ran3(idum,rand)

	if(irestrt.eq.0) then
c Initial population    
      do 11 i=1,Npopsiz
	      do 10 j=1,nparam
            call ran3(1,rand)
            number=nint(rand*(nposibl(j)-1))
	      parent(i,j)=g0(j)+pardel(j)*number
		  iparent(i,j)=number
 10         continue
 11    continue  
      else
	 open(inres,file="MGO.RES",STATUS='OLD')
	 rewind(inres)
	 read(inres,*) istart,npopsiz
         do j=1,npopsiz
            read(inres,*) k,(iparent(j,l),l=1,nchrome)
         enddo
	   do i=1,Npopsiz
	     do j=1,nparam
	        parent(i,j)=g0(j)+pardel(j)*iparent(i,j)
	     enddo
	   enddo
         CLOSE (inres)
      endif 
c     
c Get the fitness of the initial population   
      igen=0
      do j=1,npopsiz

           call func(igen,j,funcval,nobj,NNoisy0,mparam,lenx,x)

	     do k=1,nobj
	        fitness_avg(j,k)=funcval(1,k)
	        fitness_var(j,k)=funcval(2,k)
	     enddo
	     fitness_noisy(j)=NNoisy0 
! Output to see the result in the window   
!	write(*,"(I3,3x,20f10.6)") j,(fitness_avg(j,k),k=1,nobj),
!     +            (fitness_var(j,k),k=1,nobj),fitness_noisy(j)
      enddo
c Sort the rank of the population according the fitness based on t-student function
      npop=npopsiz
!     open(10,file="population.txt")
!	do j=1,npopsiz
!	read(10,*) n,(parent(j,i),i=1,4),fitness_avg(j,1),fitness_avg(j,2),
!     +            fitness_var(j,1),fitness_var(j,2),fitness_noisy(j)
!	enddo
!	close(10)
	call pareto(nobj,npop,fitness_avg
     +            ,fitness_var,fitness_noisy,isequen,pwnich)
!Output the initinal population 
	open(10,file="initialpop.txt")
	do j=1,npopsiz
	write(10,"(I3,3x,9e20.6,i5,e20.6)")j,(parent(j,k),k=1,nparam),
     +         (fitness_avg(j,k),k=1,nobj),
     + (fitness_var(j,k),k=1,nobj),fitness_noisy(j),isequen(j),pwnich(j)
	enddo
	close(10) 
!	pause 1
c
C MAIN LOOP STARTS HERE
       do 20 i=1,maxgen	   
         igen=i
         write (*,1111) i
         write (ioutga,1111) i
         PAR=PAR1+(PAR2-PAR1)*float(i)/maxgen
c Find the individuals which have isequence 0 
	   issum=0
	   do j=1,npopsiz
	     if(isequen(j)==0) then
	       issum=issum+1
	       iarray(issum)=j
	     endif
	   enddo
	write(*,*) issum
!	pause 2
c
c Define min or max problem
      if(MinMax.eq.1) then
      best=1.e30
	bestpw=1.e30                    
	worst=-1.e30                   
	worstpw=-1.e30                   
 	bestp1=1.e30
      bestp2=1.e30
      worstp2=-1.e30 
	worstp1=-1.e30 
      elseif(MinMax.eq.2) then
      best=-1.e30
	bestpw=-1.e30                     
	worst=1.e30                   
	worstpw=1.e30                   
 	bestp1=-1.e30
      bestp2=-1.e30
      worstp2=1.e30 
	worstp1=1.e30 
      endif
c
c Find best and worst
      do   j=1,npopsiz
            if (MinMax.eq.1.and.fitness_avg(j,1).lt.best .or.
     &          MinMax.eq.2.and.fitness_avg(j,1).gt.best) then
               best=fitness_avg(j,1)
	         ibest=j
            endif
           if (MinMax.eq.1.and.fitness_avg(j,2).lt.bestpw .or.
     &          MinMax.eq.2.and.fitness_avg(j,2).gt.bestpw) then
               bestpw=fitness_avg(j,2)
	         ibestpw=j
            endif
            if (MinMax.eq.1.and.fitness_avg(j,1).gt.worst .or.
     &          MinMax.eq.2.and.fitness_avg(j,1).lt.worst) then
	          worst=fitness_avg(j,1)
	          iworst=j
	       end if
            if (MinMax.eq.1.and.fitness_avg(j,2).gt.worstpw .or.
     &          MinMax.eq.2.and.fitness_avg(j,2).lt.worstpw) then
	          worstpw=fitness_avg(j,2)
	          iworstpw=j
	      end if
      enddo
!	pause 3
c Generate new population
      do 30 j=npopsiz+1,2*npopsiz,2
c
c   Define to use best or sequence equal 1 
200   call ran3(1,rand)
       call ran3(1,rand2)
!      if(i>0.5*Maxgen) then
       if(rand2>0.5) then
 	    jrand=int(rand*Nobj)+1
	    if(jrand==1) jbest=ibest
	    if(jrand==2) jbest=ibestpw
          call ran3(1,rand1)
 	    jrand1=int(rand1*Nobj)+1
	    if(jrand1==1) jbest1=ibest
	    if(jrand1==2) jbest1=ibestpw	
	else
	    call ran3(1,rand1)
	    jrand=int(rand1*issum)+1
	    jbest=iarray(jrand)
 	    call ran3(1,rand1)
	    jrand1=int(rand1*issum)+1
	    jbest1=iarray(jrand1) 
	endif

c Choose two individuals
      call ran3(1,rand2)
	call ran3(1,rand3)
	mate1=int(rand2*npopsiz)+1
	mate2=int(rand3*npopsiz)+1
	if(mate1==mate2) goto 200 
	do k=1,nparam
      child(j,k)  = parent(mate1,k)
	child(j+1,k)= parent(mate2,k)
	enddo
c
      call ran3(1,rand1)
	if(rand1.lt.0.5) then   
c GA algorithm
c 
c   Perform crossover 
      Pcross=0.9
	call ran3(1,rand4)
	if(rand4.le.pcross) then	  
	    do k=1,nparam	 
	       Mu=20.
		   call ran3(1,rand5)
	       if(rand5.le.0.5) then
	         bq=(2*rand5)**(1.0/(Mu+1.0))
	       else
	         bq=1/((2*(1-rand5)))**(1.0/(Mu+1.0))
	       endif
	child(j,k)=0.5*((1+bq)*parent(mate1,k)+(1-bq)*parent(mate2,k))
	child(j+1,k)=0.5*((1-bq)*parent(mate1,k)+(1+bq)*parent(mate2,k))		  
		   if(child(j,k).gt.parmax(k)) child(j,k)=parmax(k)
	       if(child(j,k).lt.parmin(k)) child(j,k)=parmin(k)
		   if(child(j+1,k).gt.parmax(k)) child(j+1,k)=parmax(k)
	       if(child(j+1,k).lt.parmin(k)) child(j+1,k)=parmin(k)
	   enddo
	endif  
	
	else
c
C HS algorithm
c Perform pitch adjustment 
      do k=1,nparam 

      call ran3(1,rand2)
      if(rand2.le.PAR) then
	call ran3(1,rand3)
	if(rand3>0.5) rand3=-rand3
	child(j,k)=parent(jbest,k)+rand3*(parent(jbest,k)-child(j,k))
		   if(child(j,k).gt.parmax(k)) child(j,k)=parmax(k)
	       if(child(j,k).lt.parmin(k)) child(j,k)=parmin(k)
	call ran3(1,rand4)
	if(rand4>0.5) rand4=-rand4
	child(j+1,k)=parent(jbest1,k)+rand4*parent(jbest1,k)-child(j+1,k)
		   if(child(j+1,k).gt.parmax(k)) child(j+1,k)=parmax(k)
	       if(child(j+1,k).lt.parmin(k)) child(j+1,k)=parmin(k)
      endif
	enddo

	endif				

c Mutation  
 	   do k=1,nparam
           call ran3(1,rand2)  
	     if(rand2.lt.(1-HMCR)) then
	call ran3(1,rand1)
	Mum=20
	if(rand1.lt.0.5) then
	  delta=(2*rand1)**(1.0/(Mum+1.0))-1
	else
	  delta=1-(2*(1-rand1))**(1.0/(Mum+1.0))
	endif
	  child(j,k)=child(j,k)+delta*(parmax(k)-parmin(k))
		   if(child(j,k).gt.parmax(k)) child(j,k)=parmax(k)
	       if(child(j,k).lt.parmin(k)) child(j,k)=parmin(k)
	endif
	  enddo

	do k=1,nparam
        call ran3(1,rand2)  
	  if(rand2.lt.(1-HMCR)) then
	call ran3(1,rand1)
	Mum=20
	if(rand1.lt.0.5) then
	  delta=(2*rand1)**(1.0/(Mum+1.0))-1
	else
	  delta=1-(2*(1-rand1))**(1.0/(Mum+1.0))
	endif
	  child(j+1,k)=child(j+1,k)+delta*(parmax(k)-parmin(k))
		   if(child(j+1,k).gt.parmax(k)) child(j+1,k)=parmax(k)
	       if(child(j+1,k).lt.parmin(k)) child(j+1,k)=parmin(k)
	 endif	 	 		 		 		    
	enddo	

c     
c  mixgen
      do k=1,nparam
       parent(j,k)=child(j,k)
	 iparent(j,k)= int((parent(j,k)-g0(k))/pardel(k))+1
	 parent(j+1,k)=child(j+1,k)
	 iparent(j+1,k)=int((parent(j+1,k)-g0(k))/pardel(k))+1
	enddo

c
  	do 301  k=1,npopsiz  
  	   i1=0
  	   do  i2=1,nparam
  	   if(child(j,i2)/=parent(k,i2)) goto 301 
  	   i1=i1+1
  	   enddo
         if(i1==nparam) goto 200
  301	  continue
    	do 302  k=1,npopsiz  
  	   i1=0
  	   do  i2=1,nparam
  	   if(child(j+1,i2)/=parent(k,i2)) goto 302 
  	   i1=i1+1
  	   enddo
         if(i1==nparam) goto 200
  302	  continue
 
        Nnoisy=Nnoisy0*(1+(igen-1)/Ngen)
	  if (Nnoisy.ge.Maxinoisy) Nnoisy=MaxiNoisy	             
        call func(igen,j,funcval,nobj,Nnoisy,mparam,lenx,x)
	     do k=1,nobj
	        fitness_avg(j,k)=funcval(1,k)
	        fitness_var(j,k)=funcval(2,k)
	     enddo 
		 fitness_noisy(j)=Nnoisy

        call func(igen,j+1,funcval,nobj,Nnoisy,mparam,lenx,x)
	     do k=1,nobj
	        fitness_avg(j+1,k)=funcval(1,k)
	        fitness_var(j+1,k)=funcval(2,k)
	     enddo 
		 fitness_noisy(j+1)=Nnoisy
!		write(*,"(I3,3x,20f20.6)") j,(fitness_avg(j,k),k=1,nobj),
!     +            (fitness_var(j,k),k=1,nobj),fitness_noisy(j)	            
 30      continue
!          pause 5
!	open(10,file="222.txt")
!	do jj=1,2*npopsiz
!	write(10,"(I3,3x,9f10.6,i5,f10.6)")jj,(parent(jj,k),k=1,nparam),
!     +  (fitness_avg(jj,k),k=1,nobj),(fitness_var(jj,k),k=1,nobj),
!     + fitness_noisy(jj),isequen(jj),pwnich(jj)
!	enddo
!	close(10)

      npop=2*npopsiz
	call pareto(nobj,npop,fitness_avg
     +            ,fitness_var,fitness_noisy,isequen,pwnich)
	call newgen(nobj,npop,npopsiz,parent,iparent,fitness_avg
     +	        ,fitness_var,fitness_noisy,isequen,pwnich)
      npop=npopsiz
	call pareto(nobj,npop,fitness_avg
     +            ,fitness_var,fitness_noisy,isequen,pwnich) 

      write(8876,*) '*********',i,'  ***********'
	write(8877,*) '*********',i,'  ***********'
	write(8878,*) '*********',i,'  ***********'

	do n=1,npopsiz
	  if(isequen(n)==0)then
	    write(8877,2001) n
     +      ,(fitness_avg(n,k),k=1,nobj),
     +  	   (fitness_var(n,k),k=1,nobj),fitness_noisy(n),pwnich(n)
	    write(8878,FMT='(10e15.6)') (parent(n,k),k=1,nparam)
	   endif
	  write(8876,FMT="(i3,3x,9e15.6,i5,e15.6)")  n,
     +	(parent(n,k),k=1,nparam),(fitness_avg(n,k),k=1,nobj),
     + (fitness_var(n,k),k=1,nobj),fitness_noisy(n),isequen(n),pwnich(n)
	enddo 
2001	format(i3,1x,6e15.6)

	call filter(igen,nobj,npopsiz,Npareto,Nptomax,parent,iparent,
     +           fitness_avg,fitness_var,fitness_noisy,
     +           filter_avg,filter_var,filter_noisy,isequen,
     +           isequen_flt,pwnich_flt,parent_flt,iparent_flt)

c
	open(102,file='pareto.txt') ! average fitness for filter
	open(104,file='final_pareto.txt') !filter 
	 do  ii=1,Npareto
	  write(102,"(5e15.6)") (filter_avg(ii,k),k=1,nobj)
	  write(104,FMT="(i3,3x,9e15.6,i5,e15.6)") ii,
     +  (parent_flt(ii,l),l=1,nparam),(filter_avg(ii,k),k=1,nobj),
     + (filter_var(ii,k),k=1,nobj),filter_noisy(ii)
     + ,isequen_flt(ii),pwnich_flt(ii)
	enddo
	close(102)
	close(104)

c Output the time need for evey generation 
      call etime(tarray)
      cpu1=tarray(1)
      cpu=(cpu1-cpu0) 
      write(103,*) i,cpu,cpu/60.0

	if(irestrt.eq.1) then
	 open(inres,file="MGO.RES")
	 rewind(inres)
	 write(inres,*) igen,npopsiz
         do j=1,npopsiz
            write(inres,*) k,(iparent(j,l),l=1,nchrome)
         enddo
         CLOSE (inres)
      endif 

 20   CONTINUE !End of the iteration                                    
c125   format(/1x,'Stop: Maximum Number of Iterations Reached.')    
      close(ioutga)
	close(103)
	close(8876)
	close(8877)
	close(8878)      

c
 1111 format(/'################  GA Generation',i5.4,
     &      '  ################'/)
 1112 format('+','[Modflow] Call No. ',i5.4)
 1113 format('+','[Modflow/MT3DMS] Call No. ',i5.4)
 1050 format(1x,' #      Binary Code',8x,'Par1 Par2 Par3',
     +          ' Par4 Par5 Fitness')
 1060 format(i3,1x,80i1/(4x,80i1))
 1071 format(3x,1x,10(4x,i4.1,4x)/(4x,10(4x,i4.1,4x)))
 1100 format(1x,'Average Function Value of Generation=',g16.5)
 1200 format(1x,'Optimal Function Value             =',g16.5,g16.5/)
 1250 format(/'  Number of Crossovers      =',i5/
     +        '  Number of Jump Mutations  =',i5/
     +        '  Number of Creep Mutations =',i5)
 1260 format('  Elitist Reproduction on Individual ',i4)
 1275 format(/' Average Values:',1p,10g11.4/(16x,10g11.4))
 1300 format('*** Jump mutation performed on individual  ',i4,
     +       ', chromosome ',i3,' ***')
 1350 format('*** Creep mutation performed on individual ',i4,
     +       ', parameter  ',i3,' ***')
 1375 format(//'%%%%%%%  Restart micro-population at generation',
     +       i5,'  %%%%%%%')
 1400 format(2x,'CPU time for generation=',e10.4)
 1500 format(i6,3x,20i1,3x,4g16.5,f10.1)
 1600 format(/1x,'Error: maximum # of simulations per iteration',
     & ' (population size):',i5,
     &       /1x,'       # of simulations per iteration specified',
     & ' for current run:',i5)
 1700 format(/1x,'Error: maximum # of opt parameters allowed:',i5,
     &       /1x,'       # of opt parameters for current run:',i5)
 1800 format(/1x,'Error: maximum # of chromosomes (bits) allowed:',i5,
     &       /1x,'       # of chromosomes (bits) for current run:',i5)
 1901 format(2x,'CPU time for already generations=',e12.6,' sec'/
     +       2x,'                             ',e12.6,' min')
 2000 format(/1x,'Error: number of possibilities has exceeded 2**40',
     & ' for parameter #:',i4)

      stop
      end
c
c#######################################################################
c
      subroutine ran3(idum,rand)
c
c  Returns a uniform random deviate between 0.0 and 1.0.  Set idum to
c  any negative value to initialize or reinitialize the sequence.
c  This function is taken from W.H. Press', "Numerical Recipes" p. 199.
c
      implicit double precision (a-h,m,o-z)
      save
c      implicit real*4(m)
c      parameter (mbig=4000000.,mseed=1618033.,mz=0.,fac=1./mbig)
      parameter (mbig=8000000.,mseed=5678033.,mz=0.,fac=1./mbig)
c      parameter (mbig=1000000000,mseed=161803398,mz=0,fac=1./mbig)
c
c  According to Knuth, any large mbig, and any smaller (but still large)
c  mseed can be substituted for the above values.
      dimension ma(55)
      data iff /0/
      if (idum.lt.0 .or. iff.eq.0) then
         iff=1
         mj=mseed-dble(iabs(idum))
         mj=dmod(mj,mbig)
         ma(55)=mj
         mk=1
         do 11 i=1,54
            ii=mod(21*i,55)
            ma(ii)=mk
            mk=mj-mk
            if(mk.lt.mz) mk=mk+mbig
            mj=ma(ii)
 11      continue
         do 13 k=1,4
            do 12 i=1,55
               ma(i)=ma(i)-ma(1+mod(i+30,55))
               if(ma(i).lt.mz) ma(i)=ma(i)+mbig
 12         continue
 13      continue
         inext=0
         inextp=31
         idum=1
      endif
      inext=inext+1
      if(inext.eq.56) inext=1
      inextp=inextp+1
      if(inextp.eq.56) inextp=1
      mj=ma(inext)-ma(inextp)
      if(mj.lt.mz) mj=mj+mbig
      ma(inext)=mj
      rand=mj*fac
      return
      end
c
c#######################################################################
c
       subroutine pareto(nobj,npop,fitness_avg
     +            ,fitness_var,fitness_noisy,isequen,pwnich)
c
      implicit real*8 (a-h,o-z)
      save
c      
      include 'ga.inc'
      include 'mgo.inc'

      dimension fitness_avg(indmax,nobjmax),fitness_var(indmax,nobjmax)
	dimension fitness_noisy(indmax)
	dimension isequen(indmax),pwnich(indmax)
	dimension ojj(indmax,nobjmax),dij(indmax,indmax)
	dimension rank(indmax),crowd(indmax),temp(indmax)
	integer   nequal(indmax,indmax,2)
	real     Rxy(nobjmax),uxyleft(nobjmax),uxyright(nobjmax)
	real     cigema,dfree,p,b,dworst
      real     best(nobjmax),worst(nobjmax)
	integer   flg(indmax,nobjmax),ic(indmax)
	integer   ilast,inext,inow
	integer   nrank,ipop

c
      common / ga1   / npopsiz,nowrite
      common / ga2   / nparam,nchrome

c Find the minum of the objectives for our problems
      dworst=1000   !need to be changed according problems     
      do k=1,nobj
	  best(k) =  1.0e30
	  worst(k)= -1.0e30
	enddo       
      do j=1,npop
	  do k=1,nobj
	    if(fitness_avg(j,k).lt.best(k)) then
	      best(k)=fitness_avg(j,k)
	    endif
	if(fitness_avg(j,k).gt.worst(k).and.fitness_avg(j,k).lt.dworst)then
!          if(fitness_avg(j,k).gt.worst(k))then
	      worst(k)=fitness_avg(j,k)
	    endif
	  enddo
	enddo
c
c Normalize the fitness of each objective
      do j=1,npop
	  do k=1,nobj
	    if((best(k)-worst(k))==0) then
	      ojj(j,k)=0
 	    else
	      if(fitness_avg(j,k).lt.dworst) then
	        ojj(j,k)=(fitness_avg(j,k)-worst(k))/(best(k)-worst(k))
	      else
	        ojj(j,k)=0
	      endif
	    endif
	  enddo
	enddo
c
c Calculate the distance of each individual away from other individuals
C Do not need for calculating cowding distance 
!      sumdij=0
!	dij=0
!	ipop=0
!      do j=1,npop
!	  do i=1,npop	
!	   do k=1,nobj
!	      if(ojj(j,k).ne.0.and.ojj(i,k).ne.0.and.j.ne.i) then
!	        ipop=ipop+1
!	        dij(j,i)=dij(j,i)+(ojj(j,k)-ojj(i,k))**2
!	      endif
!	   enddo	    
!	     dij(j,i)=sqrt(dij(j,i))
!	     sumdij=sumdij+dij(j,i) 
!	  enddo
!      enddo
!	close(10)
!      cigema=sumdij/(npop*(npop-1))  !Average distance
!	write(*,*) cigema
!	pause 123
!       cigema=sumdij/ipop/nobj
c
c Get the squence of the population
	  nequal=0
      do 111 j=1,npop
	  isequen(j)=0
	do 112 i=1,npop
	  if(i==j) then
	    goto 112
	  endif
        do k=1,nobj
           Rxy(k)=(fitness_var(i,k)/fitness_noisy(i)
     +            +fitness_var(j,k)/fitness_noisy(j))**0.5
	  enddo
	  dfree=fitness_noisy(i)+fitness_noisy(j)-2   !dimension 
        p=pcon	
	  call studentb(b,dfree,p) 
        do k=1,nobj
	    uxyleft(k) =(fitness_avg(i,k)-fitness_avg(j,k))-b*Rxy(k)
          uxyright(k)=(fitness_avg(i,k)-fitness_avg(j,k))+b*Rxy(k)
	  enddo
c==========================Pareto rank=====================
        nless=0
	  nmore=0
        do k=1,nobj      
           if ((uxyleft(k).lt.0).and.(uxyright(k).gt.0)) then
             nequal(j,i,k)=nequal(j,i,k)+1
	       flg(j,k)=0
	     elseif (uxyright(k).le.0) then   !j big
             nmore=nmore+1
	       flg(j,k)=-1
	     elseif(uxyleft(k).ge.0) then     !j small
	       nless=nless+1
	       flg(j,k)=1
	     endif
	  enddo
C
      if((flg(j,1).lt.0).and.(flg(j,2).lt.0)) then
       isequen(j)=isequen(j)+1
      endif
		
	if((flg(j,1).lt.0).and.(flg(j,2).eq.0)) then
	 if(((fitness_avg(i,2)-fitness_avg(j,2)).lt.0)) then
       isequen(j)=isequen(j)+1
      endif
      endif

	if((flg(j,1).eq.0).and.(flg(j,2).lt.0)) then
      if(((fitness_avg(i,1)-fitness_avg(j,1)).lt.0)) then
       isequen(j)=isequen(j)+1
      endif
	endif

	if((flg(j,1).eq.0).and.(flg(j,2).eq.0)) then
	 if(((fitness_avg(i,1)-fitness_avg(j,1)).lt.0).and.
     +     ((fitness_avg(i,2)-fitness_avg(j,2)).lt.0)) then
       isequen(j)=isequen(j)+1
	endif
	endif
!	if((flg(j,1).eq.0).and.(flg(j,2).eq.0)) then
!	goto 112
!	endif
!	if(dij(j,i).le.cigema) then
!     pw=dij(j,i)/cigema
!	pwnich(j)=pwnich(j)+1-pw   ! smaller pwnich is better 
!     endif
112   continue
111   continue
c exlude the worst individuals
      do j=1,npop
	  if(fitness_avg(j,1)>dworst) isequen(j)=npop-1
	  if(fitness_avg(j,2)>dworst) isequen(j)=npop-1
	enddo 
c
        pwnich=0
	  crowd=0
c
c Clculate the niche value(crowding distance instead)      
      do j=1,npop
	  nrank=0
!Number of solutions with rank=j-1
        do i=1,npop
	     if(isequen(i)==(j-1)) then
	       nrank=nrank+1
	       ic(nrank)=i
	     endif
	  enddo
c	
	if(nrank.gt.1) then
!
	  do k=1,nobj 
!
	    do i=1,nrank
	      temp(i)=fitness_avg(ic(i),k)
	      rank(i)=real(i)
	    enddo
	    call sortem(1,nrank,temp,1,rank)
!
	    crowd(ic(int(rank(1))))=crowd(ic(int(rank(1))))+1e+10   !give the max distance
	    crowd(ic(int(rank(nrank))))=crowd(ic(int(rank(nrank))))+1e+10 !give the max distance 

	if(nrank.gt.2) then
!
	    do i=2,nrank-1
	       inow=int(rank(i))
	       inext=int(rank(i+1))
	       ilast=int(rank(i-1))
	      if(nequal(inow,ilast,k).eq.1) then
	        crowd(ic(inow))=crowd(ic(inow))+0   !if equal,give distance 0
	      else
	        crowd(ic(inow))=crowd(ic(inow))
     +                            +abs(ojj(ic(inow),k)-ojj(ic(ilast),k)) !not equal,give ditance average(i)-average(j)
	      endif

	      if(nequal(inow,inext,k).eq.1) then
	        crowd(ic(inow))=crowd(ic(inow))+0
	      else
	        crowd(ic(inow))=crowd(ic(inow))
     +                            +abs(ojj(ic(inow),k)-ojj(ic(inext),k))
	      endif
	    enddo
	endif
	  enddo
	endif
	enddo
c
	pwnich=-crowd ! smaller pwnich is better 
      return
	end
c
c#######################################################################
c
      subroutine studentb(b,df,p)
	use imsl
	integer nout,i
	real df,t,b,btem,b1,b2,tdf1,p
c	
	call umach(2,nout)
      b1=0
	b2=100
	btem=(b2-b1)/2
c	
	do i=1,10000
      tdf1=tdf(btem,df)
	if(abs(tdf1-p).le.0.00001) then
	b=btem
	goto 219
	endif
	if(tdf1.gt.p) then
	b2=btem
	btem=(b2-b1)/2+b1
	else
	b1=btem
	btem=(b2-b1)/2+b1
	endif
      enddo
219   continue

      return
	end
c
c#############################################################
c 
      subroutine newgen(nobj,npop,npopsiz,parent,iparent,fitness_avg
     +	        ,fitness_var,fitness_noisy,isequen,pwnich)
c
      implicit real*8 (a-h,o-z)
      save
	
	include 'ga.inc'
      include 'mgo.inc'

      dimension parent(indmax,nparmax)	
	dimension iparent(indmax,nchrmax)
      dimension fitness_avg(indmax,nobjmax),fitness_var(indmax,nobjmax)
	dimension fitness_noisy(indmax)
	dimension isequen(indmax),pwnich(indmax)
	dimension ic(indmax)
	dimension fitness_tempavg(indmax,nobjmax)
	dimension fitness_tempvar(indmax,nobjmax)
	dimension fitness_tempnoisy(indmax)
	dimension iparentemp(indmax,nparmax),parentemp(indmax,nparmax)

      common / ga2   / nparam,nchrome

	do j=1,npop
	  ic(j)=0
	enddo
c Get the number of every rank,from rank 0 and up
	do j=1,npop
	  ic(isequen(j)+1)=ic(isequen(j)+1)+1
      enddo
	jpw=0
	kpw=1
	do j=1,npop
	  jpw=jpw+ic(j)
	  if(jpw.eq.npopsiz) goto 2220
	  if(jpw.gt.npopsiz) goto 2221
	enddo
c
2220  jjpw=j    ! 
      write(*,*) 'Just OK, equal to  ',npopsiz
	do j=1,npop
	  if(isequen(j).lt.jjpw) then
	   do k=1,nobj
	     fitness_tempavg(kpw,k)=fitness_avg(j,k)
	     fitness_tempvar(kpw,k)=fitness_var(j,k)
	   enddo
	   fitness_tempnoisy(kpw)=fitness_noisy(j)
	   do k=1,nchrome
	     iparentemp(kpw,k)=iparent(j,k)
	   end do
	   do k=1,nparam
	     parentemp(kpw,k)=parent(j,k)
	   end do
	   kpw=kpw+1
	  end if
	end do
	goto 2223

2221  jjpw=j
	write(*,*) jpw, 'big than ',npopsiz
	do j=1,npop
	  if(isequen(j).lt.(jjpw-1))then
	     do k=1,nobj
	       fitness_tempavg(kpw,k)=fitness_avg(j,k)
	       fitness_tempvar(kpw,k)=fitness_var(j,k)
	     enddo
	     fitness_tempnoisy(kpw)=fitness_noisy(j)
	     do k=1,nchrome
		   iparentemp(kpw,k)=iparent(j,k)
		 end do
		 do k=1,nparam
		  parentemp(kpw,k)=parent(j,k)
		end do
		kpw=kpw+1
	  end if
	enddo
	write(*,*) 'Compare niche for more than npopsiz from rank=',jjpw-1

	do j=1,npop
	  ic(j)=0
	enddo
c  
	do j=1,npop
		if(isequen(j).eq.(jjpw-1)) then
		  ic(j)=1 !Find the rank needed to comapare the niche and assign value 1
		  do k=1,npop
		    if(isequen(j).eq.(jjpw-1).and.pwnich(k).lt.pwnich(j)) then
		      ic(j)=ic(j)+1 
		    end if
		  end do
		endif
	enddo

	do ik=1,npop
	   do j=1,npop
	     if(isequen(j).eq.(jjpw-1).and.ic(j).eq.ik) then
	       do k=1,nobj
	         fitness_tempavg(kpw,k)=fitness_avg(j,k)
	         fitness_tempvar(kpw,k)=fitness_var(j,k)
	       enddo
	       fitness_tempnoisy(kpw)=fitness_noisy(j)
		    do k=1,nchrome
			  iparentemp(kpw,k)=iparent(j,k)
			end do
			do k=1,nparam
			  parentemp(kpw,k)=parent(j,k)
			end do
			kpw=kpw+1
			end if
		end do
	end do
2223  continue
c 
	do j=1,kpw-1
	  do k=1,nobj
	    fitness_avg(j,k)=fitness_tempavg(j,k)
	    fitness_var(j,k)=fitness_tempvar(j,k)
	  enddo
	  fitness_noisy(j)=fitness_tempnoisy(j)
	  do k=1,nchrome
	    iparent(j,k)=iparentemp(j,k)
	  end do
	  do k=1,nparam
		parent(j,k)=parentemp(j,k)
	  end do
	end do
 2224  continue
      return
	end
c#############################################################
c    
      subroutine filter(igen,nobj,npopsiz,Npareto,Nptomax,parent,iparent
     +           ,fitness_avg,fitness_var,fitness_noisy,
     +           filter_avg,filter_var,filter_noisy,isequen,
     +           isequen_flt,pwnich_flt,parent_flt,iparent_flt)                            
c 把所有代中pareto 解放到解集过滤器中
c 如果满了，就计算所有的pareto解的nich值，然后替换nich值大的解编号jworst
      implicit real*8 (a-h,o-z)
      save
c      
      include 'ga.inc'
      include 'mgo.inc'
      
      dimension parent(indmax,nparmax)	
	dimension iparent(indmax,nchrmax)
	dimension fitness_noisy(indmax)
	dimension fitness_avg(indmax,nobjmax),fitness_var(indmax,nobjmax)	
	dimension filter_avg(indmax,nobjmax),filter_var(indmax,nobjmax)
	dimension filter_noisy(indmax)
	dimension parent_flt(indmax,nparmax)
	dimension iparent_flt(indmax,nparmax)
	dimension isequen(indmax)
	dimension isequen_flt(indmax)
	dimension pwnich_flt(indmax)
	integer   igen,nobj,npopsiz,Npareto,Nptomax
 	
      common / ga2   / nparam,nchrome
c
c Check there is or not the same individual in the filter
c Combine the individuals with rank=1 
      
	do 100 j=1,npopsiz
	  if(isequen(j)==0) then
!	    if(fitness_avg(j,1)>100) goto 100
!	    if(fitness_avg(j,2)>50)  goto 100
	    do k=1,Npareto
	      nflag=0
	      do n=1,nparam
	       if( parent(j,n)/=parent_flt(k,n)) then
	         nflag=0
	       else 
	         nflag=nflag+1
	       endif
	     enddo
	     if(nflag==nparam) goto 100
	   enddo	   
	  Npareto=Npareto+1
	  do k=1,nobj
     	    filter_avg(Npareto,k)=fitness_avg(j,k)
	    filter_var(Npareto,k)=fitness_var(j,k)
	  enddo
	  filter_noisy(Npareto)=fitness_noisy(j)
        do k=1,nparam
	    parent_flt(Npareto,k)=parent(j,k)
	    iparent_flt(Npareto,k)=iparent(j,k)
	  enddo
	endif
100   continue
c
c Re-sort the individuals
	  call pareto(nobj,Npareto,filter_avg,
     +     filter_var,filter_noisy,isequen_flt,pwnich_flt)    
c
c Save the individuals with rank=1 
	if(igen.gt.1) then
	  ii=0
	  do j=1,Npareto
	    if(isequen_flt(j)==0) then
	      ii=ii+1
	      do k=1,nobj
     	        filter_avg(ii,k)=filter_avg(j,k)
	        filter_var(ii,k)=filter_var(j,k)
	      enddo
            do k=1,nparam
	        parent_flt(ii,k) = parent_flt(j,k)
	        iparent_flt(ii,k)= iparent_flt(j,k)
	      enddo
	      filter_noisy(ii)=filter_noisy(j)
	      isequen_flt(ii)=isequen_flt(j)
	      pwnich_flt(ii)=pwnich_flt(j)
	    endif
	  enddo
	  Npareto=ii
      endif

!	 open(10,file="555.txt")
!	do jj=1,Npareto
!	write(10,"(I3,3x,9f10.6,i5,f10.6)")jj,(parent_flt(jj,k),k=1,nparam),
!     +  (filter_avg(jj,k),k=1,nobj),(filter_var(jj,k),k=1,nobj),
!     + filter_noisy(jj),isequen_flt(jj),pwnich_flt(jj)
!	enddo
!	close(10)
c
C Exclude number more than Nptomax
      if(Npareto.gt.Nptomax) then 
        call newgen(nobj,Npareto,Nptomax,parent_flt,iparent_flt,
     +	filter_avg,filter_var,filter_noisy,isequen_flt,pwnich_flt)
	  Npareto=Nptomax
	endif

      return
	end
c#######################################################################
c
      subroutine sortem(ib,ie,a,iperm,b)
c-----------------------------------------------------------------------
c
c                      Quickersort Subroutine
c                      **********************
c
c This is a subroutine for sorting a real array in ascending order. This
c is a Fortran translation of algorithm 271, quickersort, by R.S. Scowen
c in collected algorithms of the ACM.
c
c The method used is that of continually splitting the array into parts
c such that all elements of one part are less than all elements of the
c other, with a third part in the middle consisting of one element.  An
c element with value t is chosen arbitrarily (here we choose the middle
c element). i and j give the lower and upper limits of the segment being
c split.  After the split a value q will have been found such that 
c a(q)=t and a(l)<=t<=a(m) for all i<=l<q<m<=j.  The program then
c performs operations on the two segments (i,q-1) and (q+1,j) as follows
c The smaller segment is split and the position of the larger segment is
c stored in the lt and ut arrays.  If the segment to be split contains
c two or fewer elements, it is sorted and another segment is obtained
c from the lt and ut arrays.  When no more segments remain, the array
c is completely sorted.
c
c
c INPUT PARAMETERS:
c
c   ib,ie        start and end index of the array to be sorteda
c   a            array, a portion of which has to be sorted.
c   iperm        0 no other array is permuted.
c                1 array b is permuted according to array a
c                2 arrays b,c are permuted.
c                3 arrays b,c,d are permuted.
c                4 arrays b,c,d,e are permuted.
c                5 arrays b,c,d,e,f are permuted.
c                6 arrays b,c,d,e,f,g are permuted.
c                7 arrays b,c,d,e,f,g,h are permuted.
c               >7 no other array is permuted.
c
c   b,c,d,e,f,g,h  arrays to be permuted according to array a.
c
c OUTPUT PARAMETERS:
c
c    a      = the array, a portion of which has been sorted.
c
c    b,c,d,e,f,g,h  =arrays permuted according to array a (see iperm)
c
c NO EXTERNAL ROUTINES REQUIRED:
c
c-----------------------------------------------------------------------
c
      implicit real*8 (a-h,o-z)
      save
      dimension a(*),b(*)
c
c The dimensions for lt and ut have to be at least log (base 2) n
c
      integer   lt(64),ut(64),i,j,k,m,p,q
c
c Initialize:
c
      j     = ie
      m     = 1
      i     = ib
      iring = iperm+1
      if (iperm.gt.7) iring=1
c
c If this segment has more than two elements  we split it
c
 10   if (j-i-1) 100,90,15
c
c p is the position of an arbitrary element in the segment we choose the
c middle element. Under certain circumstances it may be advantageous
c to choose p at random.
c
 15   p    = (j+i)/2
      ta   = a(p)
      a(p) = a(i)
      go to (21,19),iring
 19      tb   = b(p)
         b(p) = b(i)
 21   continue
c
c Start at the beginning of the segment, search for k such that a(k)>t
c
      q = j
      k = i
 20   k = k+1
      if(k.gt.q)     go to 60
      if(a(k).le.ta) go to 20
c
c Such an element has now been found now search for a q such that a(q)<t
c starting at the end of the segment.
c
 30   continue
      if(a(q).lt.ta) go to 40
      q = q-1
      if(q.gt.k)     go to 30
      go to 50
c
c a(q) has now been found. we interchange a(q) and a(k)
c
 40   xa   = a(k)
      a(k) = a(q)
      a(q) = xa
      go to (45,44),iring
 44      xb   = b(k)
         b(k) = b(q)
         b(q) = xb
 45   continue
c
c Update q and search for another pair to interchange:
c
      q = q-1
      go to 20
 50   q = k-1
 60   continue
c
c The upwards search has now met the downwards search:
c
      a(i)=a(q)
      a(q)=ta
      go to (65,64),iring
 64      b(i) = b(q)
         b(q) = tb
 65   continue
c
c The segment is now divided in three parts: (i,q-1),(q),(q+1,j)
c store the position of the largest segment in lt and ut
c
      if (2*q.le.i+j) go to 70
      lt(m) = i
      ut(m) = q-1
      i = q+1
      go to 80
 70   lt(m) = q+1
      ut(m) = j
      j = q-1
c
c Update m and split the new smaller segment
c
 80   m = m+1
      go to 10
c
c We arrive here if the segment has  two elements we test to see if
c the segment is properly ordered if not, we perform an interchange
c
 90   continue
      if (a(i).le.a(j)) go to 100
      xa=a(i)
      a(i)=a(j)
      a(j)=xa
      go to (95,94),iring
   94    xb   = b(i)
         b(i) = b(j)
         b(j) = xb
   95 continue
c
c If lt and ut contain more segments to be sorted repeat process:
c
 100  m = m-1
      if (m.le.0) go to 110
      i = lt(m)
      j = ut(m)
      go to 10
 110  continue
      return
      end
c
c#######################################################################