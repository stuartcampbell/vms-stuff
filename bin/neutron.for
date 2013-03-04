c	Neutron data conversion
	character*1 par,yn,ans
c
	data c1,c2,c3,c4,c5/81.80699822,4.135709079e-2,6.283185307,3956.129248
	1,8.617069615e-2/
	c6=100.*c2
c
	write(6,*)
	write(6,*)' NEUTRON UNIT CONVERSION PROGRAM'
	write(6,*)'         Stuart Campbell        '
	write(6,*)                             
c
c	Initialise parameters to zero
1	 alambda=0.
	 e=0.
	 f=0.
	 ak=0.
	 v=0.
	 tof=0.
	 t=0.
	goto 4
c
5	write(6,*)'Invalid entry -- Enter one of the letters below'
	write(6,*)
c
4	write(6,*)' Allowed Input Variables'
	write(6,*)' -----------------------'
	write(6,*)' Wavelength (A)          = L'
	write(6,*)' Energy (meV)            = E'
	write(6,*)' Frequency (THz)         = F'
	write(6,*)' Wave Vector (A-1)       = K'
	write(6,*)' Velocity (ms-1)         = V'
	write(6,*)' Time of Flight (sm-1)   = T'
	write(6,*)' Neutron Temperature (K) = N'
c
	write(6,100)
	read(5,'(a1)')par
	 if(par.eq.' ')go to 1000
	if(par.ne.'L'.and.par.ne.'E'.and.par.ne.'F'.and.par.ne.'K'.and.par.ne.
     1	'V'.and.par.ne.'T'.and.par.ne.'N'.and.par.ne.'l'.and.par.ne.'e'.and.
     2	par.ne.'f'.and.par.ne.'k'.and.par.ne.'v'.and.par.ne.'t'.and.par.ne.'n')
     3	goto 5
c
c	Input=lambda(A)
2	if(par.eq.'L'.or.par.eq.'l')then
	 write(6,103)par
	 read(5,*)alambda
	e=c1/alambda**2
	f=e/c6
	ak=c3/alambda
	v=c4/alambda
	tof=1./v
	t=e/c5
	 write(6,105)e
	 write(6,110)f
	 write(6,106)ak
	 write(6,107)v
	 write(6,108)tof
	 write(6,109)t
c
c	Input=E(meV)
	else if(par.eq.'E'.or.par.eq.'e')then
	 write(6,103)par
	 read(5,*)e
	alambda=sqrt(c1/e)
	f=e/c6
	ak=c3/alambda
	v=c4/alambda
	tof=1./v
	t=e/c5
	 write(6,104)alambda
	 write(6,110)f
	 write(6,106)ak
	 write(6,107)v
	 write(6,108)tof
	 write(6,109)t
c
c	Input=Frequency(THz)
	else if(par.eq.'F'.or.par.eq.'f')then
	 write(6,103)par
	 read(5,*)f
	alambda=sqrt(c1/(c6*f))
	e=c1/alambda**2
	ak=c3/alambda
	v=c4/alambda
	tof=1./v
	t=e/c5
	 write(6,104)alambda
	 write(6,105)e
	 write(6,106)ak
	 write(6,107)v
	 write(6,108)tof
	 write(6,109)t
c
c	Input=k(A-1)
	else if(par.eq.'K'.or.par.eq.'k')then
	 write(6,103)par
	 read(5,*)ak
	alambda=c3/ak
	e=c1/alambda**2
	f=e/c6
	v=c4/alambda
	tof=1./v
	t=e/c5
	 write(6,104)alambda
	 write(6,105)e
	 write(6,110)f
	 write(6,107)v
	 write(6,108)tof
	 write(6,109)t
c
c	Input=v(ms-1)
	else if(par.eq.'V'.or.par.eq.'v')then
	 write(6,103)par
	 read(5,*)v
	alambda=c4/v
	e=c1/alambda**2
	f=e/c6
	ak=c3/alambda
	tof=alambda/c4
	t=e/c5
	 write(6,104)alambda
	 write(6,105)e
	 write(6,110)f
	 write(6,106)ak
	 write(6,108)tof
	 write(6,109)t
c	
c	Input=tof(sm-1)
	else if(par.eq.'T'.or.par.eq.'t')then
	 write(6,103)par
	 read(5,*)tof
	alambda=c4*tof
	e=c1/alambda**2
	f=e/c6
	ak=c3/alambda
	v=c4/alambda
	t=e/c5
	 write(6,104)alambda
	 write(6,105)e
	 write(6,110)f
	 write(6,106)ak
	 write(6,107)v
	 write(6,109)t
c
c	Input=T(K)
	else if(par.eq.'N'.or.par.eq.'n')then
	 write(6,103)par
	 read(5,*)t
	alambda=sqrt(c1/(c5*t))
	e=c1/alambda**2
	f=e/c6
	ak=c3/alambda
	v=c4/alambda
	tof=1./v
	 write(6,104)alambda
	 write(6,105)e
	 write(6,110)f
	 write(6,106)ak
	 write(6,107)v
	 write(6,108)tof
c
	end if
	write(6,101)
	read(5,'(a1)')yn
	if(yn.eq.' ')yn='y'
	 if(yn.eq.'y')then
3	 write(6,102)
	 read(5,'(a1)')ans
	 write(6,*)
	 if(ans.eq.' ')ans='y'
	  if(ans.eq.'y')then
	  go to 2
	  else if(ans.ne.'n')then
	  go to 3
	  else
	  go to 1
	  end if
	 end if
c
1000	stop
c
100	format(/1x,'Enter variable type for input (<RET> to finish) => ',$)
101	format(/1x,'Continue ? (y/n) Def:y ',$)
102	format(1x,'Same input variable type ? (y/n) Def:y ',$)
103	format(1x,'Input value of ',a1,' => ',$)
104	format(1x,' Wavelength          = ',pe14.6,' A')
105	format(1x,' Energy              = ',pe14.6,' meV')
106	format(1x,' Wave Vector         = ',pe14.6,' A-1')
107	format(1x,' Velocity            = ',pe14.6,' ms-1')
108	format(1x,' Time of Flight      = ',pe14.6,' sm-1')
109	format(1x,' Neutron Temperature = ',pe14.6,' K')
110	format(1x,' Frequency           = ',pe14.6,' THz')
	end
